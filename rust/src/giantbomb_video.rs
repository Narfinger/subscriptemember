use hyper::Client;
use serde;
use serde_json;
use std::sync::Mutex;
use diesel::insert;
use diesel::sqlite::SqliteConnection;
use subs_and_video::{GBKey, NewVideo, make_gb_url, from_giantbomb_datetime_to_timestamp};
use std::time::Duration;

static LIMIT: &'static str = "10";

#[derive(Debug,Deserialize)]
pub struct GiantBombResult<T> {
    results: Vec<T>,
}

#[derive(Debug,Deserialize)]
pub struct GiantBombVideo {
    pub deck: String,
    pub hd_url: String,
    pub name: String,
    pub length_seconds: String,
    pub publish_date: String,
    pub site_detail_url: String,
}

fn query_giantbomb<T>(t: &GBKey, url: String) -> GiantBombResult<T>
    where T: serde::Deserialize
{
    let client = Client::new();
    let mut q = String::from(url);
    q.push_str("&api_key=");
    q.push_str(&t.key);
    let res = client.get(q.as_str()).send().unwrap();
    serde_json::from_reader(res)
        .unwrap_or_else(|e: serde_json::error::Error| panic!("error in json parsing: {}", e))
}

fn construct_new_video(v: &GiantBombVideo) -> NewVideo {
    NewVideo {
        vid: "NA".to_string(),
        title: v.name.clone(),
        thumbnail: "NA".to_string(),
        published_at: from_giantbomb_datetime_to_timestamp(&v.publish_date),
        channelname: "GiantBomb".to_string(),
        url: make_gb_url(v.site_detail_url.clone()),
        duration: v.length_seconds.parse().unwrap_or(0),
    }
}

fn query_videos(t: &GBKey) -> Vec<NewVideo> {
    let qstring = "https://www.giantbomb.com/api/videos/?format=json&limit=".to_string() + LIMIT;
    let res = query_giantbomb(t, qstring);
    res.results
        .iter()
        .map(construct_new_video)
        .collect::<Vec<NewVideo>>()
}

pub fn update_videos(t: &GBKey, db: &Mutex<SqliteConnection>) {
    use schema::videos;
    use diesel::ExecuteDsl;

    let vids: Vec<NewVideo> = query_videos(t);
    let dbconn: &SqliteConnection = &db.lock().unwrap();
    insert(&vids)
        .into(videos::table)
        .execute(dbconn)
        .expect("Insertion of Videos Failed");
}
