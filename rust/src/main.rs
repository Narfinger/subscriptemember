#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]
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
extern crate yup_oauth2 as oauth2;
extern crate handlebars;
extern crate rocket;
extern crate rocket_contrib;
#[macro_use]
extern crate diesel;
#[macro_use]
extern crate diesel_codegen;
extern crate r2d2;
extern crate r2d2_diesel;
extern crate dotenv;
#[macro_use]
extern crate nom;
extern crate uuid;
extern crate rayon;

pub mod schema;
pub mod youtube_base;
pub mod youtube_subscriptions;
pub mod youtube_video;
pub mod subs_and_video;
pub mod giantbomb_video;

use std::sync::Mutex;
use oauth2::{Authenticator, DefaultAuthenticatorDelegate, ConsoleApplicationSecret,
             DiskTokenStorage, GetToken, FlowType};

use std::io::prelude::*;
use std::fs::File;
use std::thread;
use std::env;
use std::sync::mpsc;
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
use rocket::response::Redirect;
use rocket::response::content::Content;
use rocket::http::ContentType;
use rocket_contrib::JSON;
use subs_and_video::{GBKey, get_lastupdate_in_unixtime};

struct TK(oauth2::Token);
struct GBTK(GBKey);
struct DB(Pool<ConnectionManager<SqliteConnection>>);
struct HB(handlebars::Handlebars);
struct UpdatingVideos(Mutex<()>);
struct MPSC {
    send: Mutex<mpsc::Sender<i64>>,
    recv: Mutex<mpsc::Receiver<i64>>,
}

struct MyURL(Url);
#[derive(FromForm)]
struct AddForm {
    url: MyURL,
}

impl<'v> FromFormValue<'v> for MyURL {
    type Error = hyper::error::ParseError;
    fn from_form_value(v: &'v str) -> Result<Self, Self::Error> {
        let mut nv = v.replace("%3A", ":");
        nv = nv.replace("%2F", "/");
        let parsed = Url::parse(&nv);
        match parsed {
            Ok(x) => Ok(MyURL(x)),
            Err(x) => Err(x),
        }
    }
}

fn setup_oauth() -> oauth2::Token {
    let f = File::open("client_secret.json").expect("Did not find client_secret.json");

    let secret = json::from_reader::<File, ConsoleApplicationSecret>(f).unwrap().installed.unwrap();
    let mut cwd = std::env::current_dir().unwrap();
    cwd.push("tk");
    let cwd: String = String::from(cwd.to_str().expect("string conversion error"));
    let ntk = DiskTokenStorage::new(&cwd).expect("disk storage token is broken");

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
    youtube_subscriptions::get_subs(&tk.0, &db.0, true);
    Redirect::to("/subs")
}

#[get("/subs")]
fn subs(tk: State<TK>, db: State<DB>, hb: State<HB>) -> Content<String> {
    let sub = youtube_subscriptions::get_subs(&tk.0, &db.0, false);
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
                 upv: State<UpdatingVideos>)
                 -> Redirect {
    if upv.0.try_lock().is_ok() {
        let ntk = tk.0.clone();
        let ngbtk = gbtk.0.clone();
        let ndb = db.0.clone();
        thread::spawn(move || {
            giantbomb_video::update_videos(&ngbtk, &ndb);
            let subs = youtube_subscriptions::get_subs(&ntk, &ndb, false);
            youtube_video::update_videos(&ntk, &ndb, &subs);
        });
    }
    Redirect::to("/")
}

#[get("/delete/<vid>")]
fn delete(vid: &str, db: State<DB>) -> Redirect {
    youtube_video::delete_video(&db.0, vid);
    Redirect::to("/")
}

#[get("/socket")]
fn socket(sc: State<MPSC>) -> JSON<i64> {
    let reader: &mpsc::Receiver<i64> = &sc.recv.lock().unwrap();
    JSON(reader.recv().unwrap())
}


#[get("/sockettest")]
fn sockettest(sc: State<MPSC>) {
    let writer: &mpsc::Sender<i64> = &sc.send.lock().unwrap();
    writer.send(64).expect("Error in writing");
}

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
    let dt = d.format("%H:%M - %d.%m").to_string();
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
    let config = r2d2::Config::default();
    let manager = ConnectionManager::<SqliteConnection>::new(database_url);
    let pool = r2d2::Pool::new(config, manager).expect("Failed to create pool.");

    println!("Setting up channels");
    let ch = mpsc::channel();
    let chstruct = MPSC {
        send: Mutex::new(ch.0),
        recv: Mutex::new(ch.1),
    };

    println!("Starting server");
    rocket::ignite()
        .mount("/",
               routes![update_subs, subs, update_videos, delete,
                       socket, sockettest, /*static_files,*/ addurl, index])
        .manage(TK(setup_oauth()))
        .manage(GBTK(setup_gbkey()))
        .manage(DB(pool))
        .manage(HB(hb))
        .manage(UpdatingVideos(Mutex::new(())))
        .manage(chstruct)
        .launch();
}
