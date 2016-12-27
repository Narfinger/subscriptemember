#![cfg_attr(feature = "nightly", feature(proc_macro))]
#[cfg(feature = "nightly")]
#[macro_use]
extern crate serde_derive;

extern crate hyper;
extern crate yup_oauth2 as oauth2;
extern crate serde;
extern crate serde_json;

use oauth2::{Authenticator, DefaultAuthenticatorDelegate, PollInformation, ConsoleApplicationSecret, MemoryStorage, GetToken};
use serde_json as json;
use std::default::Default;
use std::io::prelude::*;
use std::fs::File;


fn main() {
    let mut f = File::open("client_secret.json").unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    
    let secret = json::from_str::<ConsoleApplicationSecret>(&s).unwrap().installed.unwrap();
    let res = Authenticator::new(&secret, DefaultAuthenticatorDelegate,
                                 hyper::Client::new(),
                                 <MemoryStorage as Default>::default(), None)
        .token(&["https://www.googleapis.com/auth/youtube.upload"]);
    match res {
        Ok(t) => {
            println!("DONE!!!")

            // now you can use t.access_token to authenticate API calls within your
            // given scopes. It will not be valid forever, but Authenticator will automatically
            // refresh the token for you.
        },
        Err(err) => println!("Failed to acquire token: {}", err),
    }


    println!("Hello, world!");
}
