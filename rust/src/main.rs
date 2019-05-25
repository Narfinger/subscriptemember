extern crate actix_web;
extern crate serde;
extern crate reqwest;
extern crate chrono;
extern crate chrono_tz;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate serde_json;
extern crate tokio_core;
extern crate oauth2;
extern crate handlebars;
#[macro_use]
extern crate diesel;
#[macro_use]
extern crate nom;
extern crate uuid;
extern crate url;
extern crate rayon;
extern crate ws;
extern crate failure;
extern crate preferences;

pub mod schema;
pub mod youtube_base;
pub mod youtube_oauth;
pub mod youtube_subscriptions;
pub mod youtube_video;
pub mod subs_and_video;
pub mod giantbomb_video;

use actix::*;
use actix_web::*;
use actix_web::http::{StatusCode, Method, header};

use std::sync::{RwLock, Mutex};
use std::thread;
use std::sync::{Arc, mpsc};
use handlebars::{Context, Handlebars, Helper, RenderContext, RenderError, Output};
use preferences::{AppInfo, prefs_base_dir};
use chrono::NaiveDateTime;
use diesel::sqlite::SqliteConnection;
use diesel::r2d2::Pool;
use diesel::r2d2::ConnectionManager;

use crate::subs_and_video::{GBKey, get_lastupdate_in_unixtime};
use crate::youtube_oauth::{Expireing, setup_oauth};

const APP_INFO: AppInfo = AppInfo{name: "subscriptemember", author: "narfinger"};
const PREFS_KEY: &'static str = "subscriptemember_prefs";

struct AppState {
    tk: RwLock<youtube_oauth::Token>,
    gbtk: GBKey,
    db: Pool<ConnectionManager<SqliteConnection>>,
    hb: handlebars::Handlebars,
    updating_videos: Mutex<()>,
    mpsc: mpsc::SyncSender<()>,
}

type AppStateShared = Arc<RwLock<AppState>>;

fn setup_gbkey() -> GBKey {
    GBKey { key: include_str!("../client_secret_gb.json").to_string() }
}


fn update_subs(state: State<AppStateShared>) -> HttpResponse {
    let tk = &state.read().unwrap().tk;
    let db = &state.read().unwrap().db;
    if tk.read().unwrap().expired() {
        let mut rwtk = tk.write().unwrap();
        *rwtk = setup_oauth().expect("Error getting fresh token");
    }
    let cl = reqwest::Client::new();
    youtube_subscriptions::get_subs(&tk.read().unwrap(), &db, &cl, true);

    HttpResponse::build(StatusCode::TEMPORARY_REDIRECT)
            .header(header::LOCATION, "/subs")
            .finish()
}

fn subs(state: State<AppStateShared>) -> String {
    let tk = &state.read().unwrap().tk;
    let db = &state.read().unwrap().db;
    let hb = &state.read().unwrap().hb;

    let cl = reqwest::Client::new();
    let sub = youtube_subscriptions::get_subs(&tk.read().unwrap(), &db, &cl, false);
    let data = json!({
        "subs": sub,
        "numberofsubs": sub.len(),
    });
    hb.render("subs", &data).unwrap()
}

fn update_videos(state: State<AppStateShared>) -> HttpResponse {
    let tk   = &state.read().unwrap().tk;
    let gbtk = &state.read().unwrap().gbtk;
    let db   = &state.read().unwrap().db;
    let ch   = &state.read().unwrap().mpsc;
    let upv  = &state.read().unwrap().updating_videos;

    if upv.try_lock().is_ok() {
        if tk.read().unwrap().expired() {
            let mut rwtk = tk.write().unwrap();
            *rwtk = setup_oauth().expect("Error in getting fresh token");
        }

        let ntk = tk.read().unwrap().clone();
        let ngbtk = gbtk.clone();
        let ndb = db.clone();

        let cl = reqwest::Client::new();
        let ncl = cl.clone();
        let nch = ch.to_owned();
        thread::spawn(move || {
            giantbomb_video::update_videos(&ngbtk, &ndb, &ncl);
            let subs = youtube_subscriptions::get_subs(&ntk, &ndb, &ncl, false);
            youtube_video::update_videos(&ntk, &ndb, &ncl, &subs);

            nch.send(());
        });
    }
    HttpResponse::build(StatusCode::TEMPORARY_REDIRECT)
            .header(header::LOCATION, "/")
            .finish()
}

fn delete(vid: Path<String>, state: State<AppStateShared>) -> HttpResponse {
    let db = &state.read().unwrap().db;
    youtube_video::delete_video(&db, &vid);
    HttpResponse::build(StatusCode::TEMPORARY_REDIRECT)
            .header(header::LOCATION, "/")
            .finish()
}

fn index(state: State<AppStateShared>) -> impl Responder {
    let db = &state.read().unwrap().db;
    let hb = &state.read().unwrap().hb;
    let vids = youtube_video::get_videos(&db);

    let lastrefreshed =
        format!("{}", NaiveDateTime::from_timestamp(get_lastupdate_in_unixtime(&db), 0)
                                .format("%H:%M:%S %d.%m.%Y"));
    let numberofvideos = vids.len();
    let totaltime: i64 = vids.iter().map(|v| v.duration).sum();

    let data = json!({
        "vids": vids,
        "lastrefreshed": lastrefreshed,
        "numberofvideos": numberofvideos,
        "totaltime": totaltime,
    });
    hb.render("index", &data).map_err(|err| err.to_string()).unwrap()
}

fn video_time(h: &Helper, _: &Handlebars, _: &Context, _: &mut RenderContext, out: &mut Output) -> Result<(), RenderError> {
    let param = h.param(0).unwrap().value().to_string().replace("\"", "").parse::<i64>().unwrap();
    let d = NaiveDateTime::from_timestamp(param, 0);
    let dt = d.format("%Y.%m.%d - %H:%M").to_string();
    //println!("this needs to be timezone aware");

    out.write(&dt)?;
    Ok(())
}

fn video_duration(h: &Helper, _: &Handlebars, _: &Context, _: &mut RenderContext, out: &mut Output) -> Result<(), RenderError> {
    let param = h.param(0).unwrap().value().to_string().replace("\"", "").parse::<i64>().unwrap();

    let seconds = param % 60;
    let minutes = param / 60 % 60;
    let hours = param / 60 / 60;

    let mut st = String::new();
    if hours > 0 {
        st.push_str(format!("{}:", hours).as_str());
    }
    if (minutes > 0) | (hours > 0) {
        st.push_str(format!("{:02}:", minutes).as_str());
    }
    st.push_str(format!("{:02}", seconds).as_str());
    out.write(&st)?;
    Ok(())
}

fn static_datatablescss(_state: State<AppStateShared>) -> HttpResponse {
    HttpResponse::Ok()
        .content_type("text/css")
        .body(include_str!("../static/datatables.css"))
}

fn static_datatablesjs(_state: State<AppStateShared>) -> HttpResponse {
    HttpResponse::Ok()
        .content_type("text/javascript")
        .body(include_str!("../static/datatables.js"))
}

struct Ws;

//impl Actor for Ws {
//    type Context = ws::WebSocketContext<Self>;
//}

//impl StreamHandler<ws::Message, ws::ProtocolError> for Ws {
//    fn handle(&mut self, msg: ws::Message, ctx: &mut Self::Context) {
//    }
//}

fn main() {
    println!("Registering templates");
    let mut hb = Handlebars::new();
    {
        let its = include_str!("../templates/index.hbs");
        assert!(hb.register_template_string("index", its).is_ok());

        let its = include_str!("../templates/subs.hbs");
        assert!(hb.register_template_string("subs", its).is_ok());

        hb.register_helper("video_time", Box::new(video_time));
        hb.register_helper("video_duration", Box::new(video_duration));
        //        HB.lock().unwrap().register_helper("video_url", Box::new(video_url));
    }

    println!("Setting up database");
    let mut database_url = prefs_base_dir().expect("Could not get prefs base dir");
    database_url.push("subscriptemember");
    database_url.push("subscriptemember.db");
    let manager = ConnectionManager::<SqliteConnection>::new(database_url.to_str().expect("Error in converting path"));
    let pool = diesel::r2d2::Pool::new(manager).expect("Failed to create pool.");
    println!("Setting up channels");
    let (sender, receiver) = mpsc::sync_channel::<()>(2);
    //let chstruct = MPSC(sender);

    {
        println!("Starting websockets");
        let ws = ws::WebSocket::new(|_| {
            move |_| {
                Ok(())
            }
        }).unwrap();
        let broadcaster = ws.broadcaster();

        thread::spawn(move || {
            if receiver.recv().is_ok() {
                broadcaster.send(ws::Message::text("refresh please"))
            } else {
                broadcaster.send(ws::Message::text("disconnected"))
            }
        });

        thread::spawn(|| ws.listen("127.0.0.1:3012"));
    }


    //let cl = reqwest::Client::new().expect("Error in creating connection pool");

    println!("Do transaction for update!");

    println!("Starting server");
    let oauth = setup_oauth().expect("Problem with oauth");


    let state = Arc::new(
        RwLock::new(
            AppState {
                tk: RwLock::new(oauth),
                gbtk: setup_gbkey(),
                db: pool,
                hb: hb,
                updating_videos: Mutex::new(()),
                mpsc: sender,
    }));

    server::new(move || {
            App::with_state(state.clone())
            .route("/update_subs", Method::GET, update_subs)
            .route("/subs", Method::GET, subs)
            .route("/update_videos", Method::GET, update_videos)
            .route("/delete/{vid}", Method::GET, delete)
            .route("/static_datatablecss", Method::GET, static_datatablescss)
            .route("/static_datatablejss", Method::GET, static_datatablesjs)
            .route("/", Method::GET, index)
    }).bind("127.0.0.1:8000")
        .unwrap()
        .run();
}
