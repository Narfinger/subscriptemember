use serde;
use diesel::insert;
use diesel::sqlite::SqliteConnection;
use r2d2::Pool;
use r2d2_diesel::ConnectionManager;
use std::ops::Deref;
use reqwest;
use uuid::Uuid;
use subs_and_video::{GBKey, NewVideo, make_gb_url, from_giantbomb_datetime_to_timestamp,
                     get_lastupdate_in_unixtime};

static LIMIT: &'static str = "10";
const VID_URL: &'static str = "https://www.giantbomb.com/api/videos/?format=json&limit=";

#[derive(Debug,Deserialize)]
struct GiantBombResult<T> {
    results: Vec<T>,
}

#[derive(Debug,Deserialize)]
struct GiantBombThumbnail {
    pub medium_url: String,
    pub small_url: String,
}

#[derive(Debug,Deserialize)]
struct GiantBombVideo {
    pub deck: String,
    pub hd_url: String,
    pub youtube_id: Option<String>,
    pub name: String,
    pub length_seconds: i64,
    pub publish_date: String,
    pub site_detail_url: String,
    pub image: GiantBombThumbnail,
}

/// query giantbomb api and returns the result in a `GiantBombResult<T>`
fn query_giantbomb<T>(t: &GBKey, url: String) -> GiantBombResult<T>
    where T: serde::Deserialize
{
    let mut q = String::from(url);
    q.push_str("&api_key=");
    q.push_str(&t.key);
    println!("Query: {}", q);
    reqwest::get(q.as_str())
        .and_then(|mut s| s.json())
        .unwrap_or_else(|e| panic!("error in json parsing: {}", e))
}

/// Given a `GiantBombVideo` and builds a `NewVideo` out of it
fn construct_new_video(v: &GiantBombVideo) -> NewVideo {
    let id = match v.youtube_id {
        Some(ref i) => i.clone(),
        None => format!("{}", Uuid::new_v4().simple()),
    };
    NewVideo {
        vid: id,
        title: v.name.clone(),
        thumbnail: v.image.small_url.clone(),
        published_at: from_giantbomb_datetime_to_timestamp(&v.publish_date),
        channelname: "GiantBomb".to_string(),
        url: make_gb_url(v.site_detail_url.as_ref()),
        duration: v.length_seconds,
    }
}

/// query the giantbomb videos, filter them according to `unix_stamp` and returns them
fn query_videos(t: &GBKey, unix_stamp: i64) -> Vec<NewVideo> {
    let qstring = VID_URL.to_string() + LIMIT;
    let res = query_giantbomb(t, qstring);
    res.results
        .iter()
        .map(construct_new_video)
        .filter(|v| v.published_at > unix_stamp)
        .collect::<Vec<NewVideo>>()
}

/// Update giantbomb videos and put them into the database
pub fn update_videos(t: &GBKey, db: &Pool<ConnectionManager<SqliteConnection>>) {
    use schema::videos;
    use diesel::ExecuteDsl;

    let us = get_lastupdate_in_unixtime(db);
    let vids: Vec<NewVideo> = query_videos(t, us);
    let dbconn = db.get().expect("DB pool problem");
    insert(&vids)
        .into(videos::table)
        .execute(dbconn.deref())
        .expect("Insertion of Videos Failed");
}
