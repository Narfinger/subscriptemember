#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]
#![cfg_attr(feature="clippy", allow(needless_pass_by_value))] //rocket state uses this
#![cfg_attr(feature = "nightly", feature(proc_macro))]
#![feature(plugin,custom_derive)]
#![plugin(rocket_codegen)]
#[cfg(feature = "nightly")]
#[macro_use]
extern crate rocket;
extern crate serde;
extern crate reqwest;
extern crate chrono;
extern crate chrono_tz;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate serde_json;
extern crate hyper;
extern crate hyper_rustls;
extern crate tokio_core;
extern crate yup_oauth2 as oauth2;
extern crate handlebars;
extern crate rocket;
extern crate rocket_contrib;
#[macro_use]
extern crate diesel;
extern crate r2d2;
extern crate r2d2_diesel;
extern crate dotenv;
#[macro_use]
extern crate nom;
extern crate uuid;
extern crate rayon;
extern crate ws;

pub mod schema;
pub mod youtube_base;
pub mod youtube_subscriptions;
pub mod youtube_video;
pub mod subs_and_video;
pub mod giantbomb_video;

use std::sync::{RwLock, Mutex};
use oauth2::{Authenticator, DefaultAuthenticatorDelegate, ConsoleApplicationSecret,
             DiskTokenStorage, GetToken, FlowType};

use std::io::prelude::*;
use std::fs::File;
use std::path::{Path,PathBuf};
use std::thread;
use std::env;
use std::sync::mpsc;
use std::str::FromStr;
use serde_json as json;
use hyper::Url;
use hyper::net::HttpsConnector;
use handlebars::{Handlebars, Helper, RenderContext, RenderError};
use chrono::NaiveDateTime;
use diesel::sqlite::SqliteConnection;
use r2d2::Pool;
use r2d2_diesel::ConnectionManager;
use dotenv::dotenv;
use rocket::request::{State, Form, FromFormValue};
use rocket::response::{Redirect, NamedFile};
use rocket::response::content::Content;
use rocket::http::ContentType;

use subs_and_video::{GBKey, get_lastupdate_in_unixtime};

struct TK(RwLock<oauth2::Token>);
struct GBTK(GBKey);
struct DB(Pool<ConnectionManager<SqliteConnection>>);
struct HB(handlebars::Handlebars);
struct UpdatingVideos(Mutex<()>);
#[derive(Clone)]
struct MPSC(mpsc::SyncSender<()>);

struct MyURL(Url);
#[derive(FromForm)]
struct AddForm {
    url: MyURL,
}

impl<'v> FromFormValue<'v> for MyURL {
    type Error = reqwest::UrlError;
    fn from_form_value(v: &'v rocket::http::RawStr) -> Result<Self, Self::Error> {
        let mut nv = v.replace("%3A", ":");
        nv = nv.replace("%2F", "/");
        let parsed = Url::from_str(&nv);
        match parsed {
            Ok(x) => Ok(MyURL(x)),
            Err(x) => Err(x),
        }
    }
}

fn setup_oauth() -> oauth2::Token {
    let f = File::open("client_secret.json").expect("Did not find client_secret.json");
    let secret = json::from_reader::<File,ConsoleApplicationSecret>(f).unwrap().installed.unwrap();
    let mut cwd = std::env::current_dir().unwrap();
    cwd.push("tk");
    let cwd: String = String::from(cwd.to_str().expect("string conversion error"));
    let ntk = DiskTokenStorage::new(&cwd).expect("disk storage token is broken");

//    let core = tokio_core::reactor::Core::new().unwrap();
//    let handle = core.handle();

    //let client = HttpsConnector::new(2,&handle);
    let client = hyper::Client::with_connector(HttpsConnector::new(hyper_rustls::TlsClient::new()));
    let realtk = Authenticator::new(&secret,
                                    DefaultAuthenticatorDelegate,
                                    client,
                                    ntk,
                                    Some(FlowType::InstalledInteractive))
        .token(&["https://www.googleapis.com/auth/youtube"]);
    if let Err(e) = realtk {
        panic!("Error in token generation: {:?}", e);
    }
    realtk.unwrap()
}

fn setup_gbkey() -> GBKey {
    let mut f = File::open("client_secret_gb.json").expect("Did not find client_secret_gb.json");
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();

    GBKey { key: s }
}

#[get("/updateSubs")]
fn update_subs(tk: State<TK>, db: State<DB>) -> Redirect {
    if tk.0.read().unwrap().expired() {
        let mut rwtk = tk.0.write().unwrap();
        *rwtk = setup_oauth();
    }
    let cl = reqwest::Client::new().expect("Error in creating connection pool");
    youtube_subscriptions::get_subs(&tk.0.read().unwrap(), &db.0, &cl, true);
    Redirect::to("/subs")
}

#[get("/subs")]
fn subs(tk: State<TK>, db: State<DB>, hb: State<HB>) -> Content<String> {
    let cl = reqwest::Client::new().expect("Error in creating connection pool");
    let sub = youtube_subscriptions::get_subs(&tk.0.read().unwrap(), &db.0, &cl, false);
    let data = json!({
        "subs": sub,
        "numberofsubs": sub.len(),
    });
    Content(ContentType::HTML, hb.0.render("subs", &data).unwrap())
}

#[get("/updateVideos")]
fn update_videos(tk: State<TK>,
                 gbtk: State<GBTK>,
                 db: State<DB>,
                 ch: State<MPSC>,
                 upv: State<UpdatingVideos>)
                 -> Redirect {
    if upv.0.try_lock().is_ok() {
        if tk.0.read().unwrap().expired() {
            let mut rwtk = tk.0.write().unwrap();
            *rwtk = setup_oauth();
        }

        let ntk = tk.0.read().unwrap().clone();
        let ngbtk = gbtk.0.clone();
        let ndb = db.0.clone();

        let cl = reqwest::Client::new().expect("Error in creating connection pool");
        let ncl = cl.clone();
        let nch = ch.to_owned();
        thread::spawn(move || {
            giantbomb_video::update_videos(&ngbtk, &ndb, &ncl);
            let subs = youtube_subscriptions::get_subs(&ntk, &ndb, &ncl, false);
            youtube_video::update_videos(&ntk, &ndb, &ncl, &subs);

            nch.0.send(());
        });
    }
    Redirect::to("/")
}

#[get("/delete/<vid>")]
fn delete(vid: String, db: State<DB>) -> Redirect {
    youtube_video::delete_video(&db.0, &vid);
    Redirect::to("/")
}

// #[get("/sockettest")]
// fn sockettest(sc: State<MPSC>) {
//     sc.0.send(());
// }

// #[get("/static/<file..>")]
// fn static_files(file: PathBuf) -> Option<NamedFile> {
//     NamedFile::open(Path::new("static/").join(file)).ok()
// }

#[post("/addurl", data="<form>")]
fn addurl(form: Form<AddForm>) -> Redirect {
    let url = &form.get().url.0;
    println!("found something {:?}", url);
    Redirect::to("/")
}

#[get("/small")]
fn small(db: State<DB>, hb: State<HB>) -> Content<String> {
    let vids = youtube_video::get_videos(&db.0);

    let lastrefreshed =
        format!("{}", NaiveDateTime::from_timestamp(get_lastupdate_in_unixtime(&db.0), 0)
                                .format("%H:%M:%S %d.%m.%Y"));
    let numberofvideos = vids.len();
    let totaltime: i64 = vids.iter().map(|v| v.duration).sum();

    let data = json!({
        "vids": vids,
        "lastrefreshed": lastrefreshed,
        "numberofvideos": numberofvideos,
        "totaltime": totaltime,
    });
    Content(ContentType::HTML,
            hb.0.render("index", &data).map_err(|err| err.to_string()).unwrap())
}


#[get("/")]
fn index(db: State<DB>, hb: State<HB>) -> Content<String> {
    let vids = youtube_video::get_videos(&db.0);

    let lastrefreshed =
        format!("{}", NaiveDateTime::from_timestamp(get_lastupdate_in_unixtime(&db.0), 0)
                                .format("%H:%M:%S %d.%m.%Y"));
    let numberofvideos = vids.len();
    let totaltime: i64 = vids.iter().map(|v| v.duration).sum();

    let data = json!({
        "vids": vids,
        "lastrefreshed": lastrefreshed,
        "numberofvideos": numberofvideos,
        "totaltime": totaltime,
    });
    Content(ContentType::HTML,
            hb.0.render("index", &data).map_err(|err| err.to_string()).unwrap())
}

fn template_filename_to_string(s: &str) -> Result<String, String> {
    let mut f = match File::open(s) {
        Ok(ff) => ff,
        Err(e) => return Err(e.to_string()),
    };

    let mut s = String::new();
    match f.read_to_string(&mut s) {
        Ok(br) => br,
        Err(e) => return Err(e.to_string()),
    };
    Ok(s)
}


fn video_time(h: &Helper, _: &Handlebars, rc: &mut RenderContext) -> Result<(), RenderError> {
    let param = h.param(0).unwrap().value().to_string().replace("\"", "").parse::<i64>().unwrap();
    let d = NaiveDateTime::from_timestamp(param, 0);
    let dt = d.format("%Y.%m.%d - %H:%M").to_string();
    //println!("this needs to be timezone aware");

    try!(rc.writer.write_all(dt.into_bytes().as_ref()));
    Ok(())
}

fn video_duration(h: &Helper, _: &Handlebars, rc: &mut RenderContext) -> Result<(), RenderError> {
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
    try!(rc.writer.write_all(st.into_bytes().as_ref()));
    Ok(())
}

// fn video_url(h: &Helper, _: &Handlebars, rc: &mut RenderContext) -> Result<(), RenderError> {
//     let param = h.param(0).unwrap().value().to_string().replace("\"", "");
//     let url = "https://www.youtube.com/watch?v=".to_string() + param.as_str();
//     try!(rc.writer.write_all(url.into_bytes().as_ref()));
//     Ok(())
// }

#[get("/static/<file..>")]
fn static_files(file: PathBuf) -> Option<NamedFile> {
    NamedFile::open(Path::new("static/").join(file)).ok()
}

fn main() {
    println!("Registering templates");
    let mut hb = Handlebars::new();
    {
        let its = template_filename_to_string("templates/index.hbs").unwrap();
        assert!(hb.register_template_string("index", its).is_ok());

        let its = template_filename_to_string("templates/subs.hbs").unwrap();
        assert!(hb.register_template_string("subs", its).is_ok());

        hb.register_helper("video_time", Box::new(video_time));
        hb.register_helper("video_duration", Box::new(video_duration));
        //        HB.lock().unwrap().register_helper("video_url", Box::new(video_url));
    }
    //println!("Checking token: {}", TK.token_type);
    dotenv().ok();

    println!("Setting up database");
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let manager = ConnectionManager::<SqliteConnection>::new(database_url);
    let pool = r2d2::Pool::new(manager).expect("Failed to create pool.");

    println!("Setting up channels");
    let (sender, receiver) = mpsc::sync_channel::<()>(2);
    let chstruct = MPSC(sender);
    
    {
        println!("Starting websockets");
        let ws = ws::WebSocket::new(|_| {
            move |_| {
                Ok(())
            }
        }).unwrap();
        let broadcaster = ws.broadcaster();
        
        thread::spawn(move || {
            if let Ok(_) = receiver.recv() {
                broadcaster.send(ws::Message::text("refresh please"))
            } else {
                broadcaster.send(ws::Message::text("disconnected"))
            }
        });
        
        thread::spawn(|| ws.listen("127.0.0.1:3012"));
    }

    
    //let cl = reqwest::Client::new().expect("Error in creating connection pool");
    
    println!("Starting server");
    rocket::ignite()
        .mount("/",
               routes![update_subs, subs, update_videos, delete,
                         static_files, addurl, small, index])
        .manage(TK(RwLock::new(setup_oauth())))
        .manage(GBTK(setup_gbkey()))
        .manage(DB(pool))
        .manage(HB(hb))
        .manage(UpdatingVideos(Mutex::new(())))
        .manage(chstruct)
        .launch();
}
