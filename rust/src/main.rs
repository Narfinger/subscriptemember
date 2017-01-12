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
#[macro_use] extern crate lazy_static;

pub mod youtube_handler;

use oauth2::{Authenticator, DefaultAuthenticatorDelegate, PollInformation, ConsoleApplicationSecret, DiskTokenStorage, GetToken,};
use serde_json as json;
use std::default::Default;
use std::io::prelude::*;
use std::fs::File;
use std::string;
use std::error::Error;


lazy_static! {
    static ref t : oauth2::Token = setup_oauth().unwrap();
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
    let tk = DiskTokenStorage::new(&cwd).expect("disk storage token is broken");
    
    let res = Authenticator::new(&secret, DefaultAuthenticatorDelegate,
                                 hyper::Client::new(),
                                 tk, None).token(&["https://www.googleapis.com/auth/youtube"]);
    return res;
}

#[get("/")]
fn hello() -> String {
    let sub = youtube_handler::get_subs(&t);

    let mut f = match File::open("templates/index.html") {
        Ok(ff) => {ff}
        Err(_) => panic!("template not found")
    };
    
    let mut s = String::new();
    let bytesread = match f.read_to_string(&mut s) {
        Ok(br) => {br}
        Err(_) => panic!("could not read template to string")
    };
    
    return s
}


fn main() {
    // println!("storing token to disk");
    // let mut st = DiskTokenStorage("tk");
    // st.set(1,["https://www.googleapis.com/auth/youtube"],t);
    let tk = setup_oauth();
    match tk {
        Ok(nt) => {
            //t = Some(nt);
            println!("DONE!!!");
            println!("Starting server"); 
            rocket::ignite().mount("/", routes![hello]).launch()
                

            // now you can use t.access_token to authenticate API calls within your
            // given scopes. It will not be valid forever, but Authenticator will automatically
            // refresh the token for you.
        },
        Err(err) => println!("Failed to acquire token: {}", err),
    }


    println!("Hello, world!");
}
