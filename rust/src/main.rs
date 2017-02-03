#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]
#![cfg_attr(feature = "nightly", feature(proc_macro))]
#![feature(plugin)]
#![plugin(rocket_codegen)]
#[cfg(feature = "nightly")]
#[macro_use]
extern crate rocket;
extern crate hyper;
extern crate serde;
extern crate flate2;
extern crate chrono;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate serde_json;
extern crate yup_oauth2 as oauth2;
extern crate rocket;
extern crate handlebars;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate diesel;
#[macro_use]
extern crate diesel_codegen;
extern crate dotenv;

pub mod schema;
pub mod youtube_base;
pub mod youtube_subscriptions;
pub mod youtube_video;
pub mod subs_and_video;
pub mod giantbomb_video;

use std::sync::Mutex;
use oauth2::{Authenticator, DefaultAuthenticatorDelegate, ApplicationSecret, ConsoleApplicationSecret,
             DiskTokenStorage, GetToken};

use std::io::prelude::*;
use std::fs::File;
use std::thread;
use std::collections::BTreeMap;
use std::env;
use serde_json as json;
use serde_json::value::ToJson;
use serde_json::value::Map;
use handlebars::{Handlebars, Helper, RenderContext, RenderError};
use chrono::NaiveDateTime;
use diesel::Connection;
use diesel::sqlite::SqliteConnection;
use dotenv::dotenv;
use rocket::response::Redirect;
use subs_and_video::GBKey;

lazy_static! {
    static ref TK : oauth2::Token = setup_oauth().unwrap();
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

fn setup_oauth() -> Result<oauth2::Token, Box<std::error::Error>> {
    let mut f = File::open("client_secret.json").expect("Did not find client_secret.json");
    // let mut s = String::new();
    // f.read_to_string(&mut s).unwrap();

    // let b = json::from_reader(f).unwrap();
    // let i: Map<String,String> = b["installed"].unwrap();
    // let secret = oauth2::ApplicationSecret {
    //     client_id: i["client_id"].to_string(),
    //     client_secret: i["client_secret"].to_string(),
    //     token_uri: b["installed"]["token_uri"],
    //     auth_uri: b["installed"]["auth_uri"],
    //     auth_provider_x509_cert_url: Some(b["installed"]["auth_provider_x509_cert_url"]),
    //     redirect_uris: vec![b["installed"]["redirect_uris"][0],b["installed"]["redirect_uris"][1]],
    //     client_email: None,
    //     client_x509_cert_url: None,
    //     project_id: b["installed"]["project_id"],
    // };
    

    let secret = json::from_reader::<File,ApplicationSecret>(f).unwrap().installed.unwrap();
    let mut cwd = std::env::current_dir().unwrap();
    cwd.push("tk");
    let cwd: String = String::from(cwd.to_str().expect("string conversion error"));
    println!("{}", cwd);
    let ntk = DiskTokenStorage::new(&cwd).expect("disk storage token is broken");

    Authenticator::new(&secret,
                       DefaultAuthenticatorDelegate,
                       hyper::Client::new(),
                       ntk,
                       None)
        .token(&["https://www.googleapis.com/auth/youtube"])
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
fn subs() -> String {
    let sub = youtube_subscriptions::get_subs(&TK, &DB, false);
    let data = json!({
        "subs": sub,
        "numberofsubs": sub.len(),
    });
    HB.lock().unwrap().render("subs", &data.to_string()).unwrap()
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
fn index() -> String {
    let mut data = BTreeMap::new();
    let vids = youtube_video::get_videos(&DB);

    let lastrefreshed = "NA";
    let numberofvideos = vids.len();
    let totaltime = "NA";

    let data = json!({
        "vids": vids,
        "lastrefreshed": lastrefreshed,
        "numberofvideos": numberofvideos,
        "totaltime": totaltime,
    });
    HB.lock().unwrap().render("index", &data.to_string()).map_err(|err| err.to_string()).unwrap()
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

// fn video_url(h: &Helper, _: &Handlebars, rc: &mut RenderContext) -> Result<(), RenderError> {
//     let param = h.param(0).unwrap().value().to_string().replace("\"", "");
//     let url = "https://www.youtube.com/watch?v=".to_string() + param.as_str();
//     try!(rc.writer.write_all(url.into_bytes().as_ref()));
//     Ok(())
// }

fn main() {
    println!("Registering templates");
    {
        let its = template_filename_to_string("templates/index.html").unwrap();
        assert!(HB.lock().unwrap().register_template_string("index", its).is_ok());

        let its = template_filename_to_string("templates/subs.html").unwrap();
        assert!(HB.lock().unwrap().register_template_string("subs", its).is_ok());

        HB.lock().unwrap().register_helper("video_time", Box::new(video_time));
//        HB.lock().unwrap().register_helper("video_url", Box::new(video_url));
    }


    println!("Switching mutex to arcs?");
    println!("Starting server");
    rocket::ignite()
        .mount("/",
               routes![update_subs, subs, update_videos, delete, index])
        .launch();
}
