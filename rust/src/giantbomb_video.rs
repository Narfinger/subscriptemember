use hyper::Client;
use serde;
use serde_json;
use std::sync::Mutex;
use diesel::sqlite::SqliteConnection;

#[derive(Debug,Deserialize)]
pub struct GiantBombResult<T> {
    results: Vec<T>,
}

#[derive(Debug,Deserialize)]
pub struct GiantBombVideo {
    pub deck: String,
    pub hd_url: String,
}

pub struct GBApiKey {
    pub key: String,
}

fn query_giantbomb<T>(t: &GBApiKey, url: &str) -> GiantBombResult<T> where T: serde::Deserialize {
    let client = Client::new();
    let mut q = String::from(url);
    q.push_str("&api_key=");
    q.push_str(&t.key);
    let res = client.get(q.as_str()).send().unwrap();
    serde_json::from_reader(res).unwrap_or_else(|e:serde_json::error::Error| panic!("error in json parsing: {}", e))
}

fn update_videos(t: &GBApiKey, db: &Mutex<SqliteConnection>) {
    
    
}
