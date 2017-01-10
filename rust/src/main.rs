#![cfg_attr(feature = "nightly", feature(proc_macro))]
#![feature(plugin)]
#![plugin(rocket_codegen)]
#[cfg(feature = "nightly")]
#[macro_use]
extern crate rocket;
extern crate hyper;
extern crate yup_oauth2 as oauth2;
extern crate serde;
extern crate serde_json;
extern crate liquid;
extern crate rocket;
#[macro_use] extern crate lazy_static;

pub mod youtube_handler;

use oauth2::{Authenticator, DefaultAuthenticatorDelegate, PollInformation, ConsoleApplicationSecret, DiskTokenStorage, GetToken,};
use serde_json as json;
use std::default::Default;
use std::io::prelude::*;
use std::fs::File;
use std::string;
use liquid::{Renderable, Context, Value};


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

    let mut f = try!(File::open("template/index.html"));
    let mut s = String::new();
    try!(f.read_to_string(&mut s));
    
    let template = liquid::parse(&s, Default::default()).unwrap();
    let mut context = Context::new();
    context.set_val("num", Value::Num(4f32));
    
    let string:Result<Option<String>, liquid::Error> = template.render(&mut context);
    let first:Option<String> = string.unwrap();
    let stri:String = first.unwrap();
    return stri;
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
