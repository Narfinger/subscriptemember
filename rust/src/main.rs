#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]
#![cfg_attr(feature = "nightly", feature(proc_macro))]
#![feature(plugin)]
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
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate diesel;
#[macro_use]
extern crate diesel_codegen;
extern crate dotenv;
#[macro_use]
extern crate nom;

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
use std::time::Duration;
use serde_json as json;
use hyper::net::HttpsConnector;
use handlebars::{Handlebars, Helper, RenderContext, RenderError};
use chrono::NaiveDateTime;
use diesel::Connection;
use diesel::sqlite::SqliteConnection;
use dotenv::dotenv;
use rocket::response::Redirect;
use rocket::response::content::Content;
use rocket::http::ContentType;
use subs_and_video::GBKey;

lazy_static! {
    static ref TK : oauth2::Token = setup_oauth();
    static ref HB : Mutex<handlebars::Handlebars> = Mutex::new(Handlebars::new());
    static ref DB : Mutex<SqliteConnection> = Mutex::new(establish_connection());
    static ref GBTK : GBKey = setup_gbkey();
    static ref UPDATING_VIDEOS: Mutex<()> = Mutex::new(());
}

pub fn establish_connection() -> SqliteConnection {
    dotenv().ok();

    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    SqliteConnection::establish(&database_url)
        .expect(&format!("Error connecting to {}", database_url))
}

fn setup_oauth() -> oauth2::Token {
    let f = File::open("client_secret.json").expect("Did not find client_secret.json");

    let secret = json::from_reader::<File, ConsoleApplicationSecret>(f).unwrap().installed.unwrap();
    let mut cwd = std::env::current_dir().unwrap();
    cwd.push("tk");
    let cwd: String = String::from(cwd.to_str().expect("string conversion error"));
    let ntk = DiskTokenStorage::new(&cwd).expect("disk storage token is broken");
    println!("s {:?}", secret.client_id);

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
fn update_subs() -> Redirect {
    youtube_subscriptions::get_subs(&TK, &DB, true);
    Redirect::to("/subs")
}

#[get("/subs")]
fn subs() -> Content<String> {
    let sub = youtube_subscriptions::get_subs(&TK, &DB, false);
    let data = json!({
        "subs": sub,
        "numberofsubs": sub.len(),
    });
    Content(ContentType::HTML,
            HB.lock().unwrap().render("subs", &data).unwrap())
}

#[get("/updateVideos")]
fn update_videos() -> Redirect {
    let l = UPDATING_VIDEOS.try_lock();
    if l.is_ok() {
        thread::spawn(|| {
            giantbomb_video::update_videos(&GBTK, &DB);
            let subs = youtube_subscriptions::get_subs(&TK, &DB, false);
            youtube_video::update_videos(&TK, &DB, &subs);
        });
    }
    Redirect::to("/")
}

#[get("/delete/<vid>")]
fn delete(vid: &str) -> Redirect {
    youtube_video::delete_video(&DB, vid);
    Redirect::to("/")
}

#[get("/")]
fn index() -> Content<String> {
    let vids = youtube_video::get_videos(&DB);

    let lastrefreshed = "NA";
    let numberofvideos = vids.len();
    let totaltime: i64 = vids.iter().map(|v| v.duration).sum();

    let data = json!({
        "vids": vids,
        "lastrefreshed": lastrefreshed,
        "numberofvideos": numberofvideos,
        "totaltime": totaltime,
    });
    Content(ContentType::HTML,
            HB.lock().unwrap().render("index", &data).map_err(|err| err.to_string()).unwrap())
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
    println!("this needs to be timezone aware");

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
        st.push_str(format!("{}H:", hours).as_str());
    }
    if minutes > 0 {
        st.push_str(format!("{:01}M:", minutes).as_str());
    }
    st.push_str(format!("{}S", seconds).as_str());
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
    {
        let its = template_filename_to_string("templates/index.hbs").unwrap();
        assert!(HB.lock().unwrap().register_template_string("index", its).is_ok());

        let its = template_filename_to_string("templates/subs.hbs").unwrap();
        assert!(HB.lock().unwrap().register_template_string("subs", its).is_ok());

        HB.lock().unwrap().register_helper("video_time", Box::new(video_time));
        HB.lock().unwrap().register_helper("video_duration", Box::new(video_duration));
        //        HB.lock().unwrap().register_helper("video_url", Box::new(video_url));
    }
    println!("Checking token: {}", TK.token_type);

    println!("Starting server");
    rocket::ignite()
        .mount("/",
               routes![update_subs, subs, update_videos, delete, index])
        .launch();
}
