#![cfg_attr(feature = "nightly", feature(proc_macro))]
#![feature(plugin,proc_macro)]
#![plugin(rocket_codegen)]
#[cfg(feature = "nightly")]
#[macro_use] extern crate rocket;
extern crate hyper;
extern crate yup_oauth2 as oauth2;
extern crate serde;
#[macro_use] extern crate serde_derive;
extern crate serde_json;
extern crate rocket;
extern crate handlebars;
#[macro_use] extern crate lazy_static;

pub mod youtube_handler;

use std::sync::Mutex;
use oauth2::{Authenticator, DefaultAuthenticatorDelegate, PollInformation, ConsoleApplicationSecret, DiskTokenStorage, GetToken,};
use serde_json as json;
use std::default::Default;
use std::io::prelude::*;
use std::fs::File;
use std::string;
use std::error::Error;
use handlebars::{Handlebars, HelperDef, RenderError, RenderContext, Helper, Context, JsonRender};

lazy_static! {
    static ref tk : oauth2::Token = setup_oauth().unwrap();
    static ref hb : Mutex<handlebars::Handlebars> = Mutex::new(Handlebars::new());
    //static ref handlebars : Handlebars::Registry = Handlebars::new();
}

fn setup_oauth() -> Result<oauth2::Token, Box<std::error::Error>> { 
    let mut f = File::open("client_secret.json").expect("Did not find client_secret.json");
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    
    let secret = json::from_str::<ConsoleApplicationSecret>(&s).unwrap().installed.unwrap();
    let mut cwd = std::env::current_dir().unwrap();
    cwd.push("tk");
    let cwd : String = String::from(cwd.to_str().expect("string conversion error"));
    println!("{}", cwd);
    let ntk = DiskTokenStorage::new(&cwd).expect("disk storage token is broken");
    
    let res = Authenticator::new(&secret, DefaultAuthenticatorDelegate,
                                 hyper::Client::new(),
                                 ntk, None).token(&["https://www.googleapis.com/auth/youtube"]);
    return res;
}

#[get("/")]
fn hello() -> String {
    let sub = youtube_handler::get_subs(&tk);
    let ref s = sub[0];
    return hb.lock().unwrap().render("index", &sub).unwrap();
    
    //return String::from("No template you idiot");
}

fn templateFilenameToString(s : &str) -> Result<String,String> {
    let mut f = match File::open(s) {
        Ok(ff) => ff,
        Err(e) => return Err(e.to_string())
    };
    
    let mut s = String::new();
    let bytesread = match f.read_to_string(&mut s) {
        Ok(br) => br,
        Err(e) => return Err(e.to_string())
    };
    return Ok(s);
}


fn main() {
    // println!("storing token to disk");
    // let mut st = DiskTokenStorage("tk");
    // st.set(1,["https://www.googleapis.com/auth/youtube"],t);
//    let tk = setup_oauth();

    println!("DONE!!!");
    println!("Registering templates");
    {
        let its = templateFilenameToString("templates/index.html").unwrap();
        assert!(hb.lock().unwrap().register_template_string("index",its ).is_ok());
    }
    
    println!("Starting server"); 
    rocket::ignite().mount("/", routes![hello]).launch();
        

    // now you can use t.access_token to authenticate API calls within your
    // given scopes. It will not be valid forever, but Authenticator will automatically
    // refresh the token for you.

    println!("Hello, world!");
}
