use crate::chrono::TimeZone;
use diesel::insert_into;
use diesel::sqlite::SqliteConnection;
use diesel::r2d2::Pool;
use diesel::r2d2::ConnectionManager;
use std::ops::Deref;
use reqwest;
use serde;
use uuid::Uuid;
use crate::subs_and_video::{GBKey, NewVideo, make_gb_url, from_giantbomb_datetime_to_timestamp,
                     get_lastupdate_in_utctime};

static LIMIT: &str = "10";
const VID_URL: &str = "https://www.giantbomb.com/api/videos/?format=json&limit=";

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
    pub hd_url: Option<String>,
    pub youtube_id: Option<String>,
    pub name: String,
    pub length_seconds: i64,
    pub publish_date: String,
    pub site_detail_url: String,
    pub image: GiantBombThumbnail,
}

/// query giantbomb api and returns the result in a `GiantBombResult<T>`
fn query_giantbomb<T>(t: &GBKey, client: &reqwest::Client, url: String) -> GiantBombResult<T>
    where T: serde::de::DeserializeOwned
{
    let mut q = url;
    q.push_str("&api_key=");
    q.push_str(&t.key);
    //println!("Query: {}", q);
    client.get(q.as_str())
        .send()
        .and_then(|mut s| s.json())
        .unwrap_or_else(|e| panic!("error in json parsing: {}", e))
}

/// Given a `GiantBombVideo` and builds a `NewVideo` out of it
fn construct_new_video(v: &GiantBombVideo) -> NewVideo {
    let id = match v.youtube_id {
        Some(ref i) => i.clone(),
        None => format!("{}", Uuid::new_v4().to_simple()),
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

/// query the giantbomb videos, filter them according to `date` and returns them
fn query_videos(t: &GBKey, client: &reqwest::Client, date: Option<chrono::DateTime<chrono::Utc>>) -> Vec<NewVideo> {
    let qstring = VID_URL.to_string() + LIMIT;
    let res = query_giantbomb(t, client, qstring);
    let iter = res.results
        .iter()
        .map(construct_new_video);
    if let Some(fdate) = date {
        iter.filter(|v| chrono::Utc.timestamp(v.published_at,0) > fdate)
            .collect::<Vec<NewVideo>>()
    } else {
        iter.collect::<Vec<NewVideo>>()
    }
}

/// Update giantbomb videos and put them into the database
pub fn update_videos(t: &GBKey, db: &Pool<ConnectionManager<SqliteConnection>>, client: &reqwest::Client) {
    use crate::schema::videos;
    use diesel::RunQueryDsl;

    let us = get_lastupdate_in_utctime(db);
    let vids: Vec<NewVideo> = query_videos(t, client, us);
    let dbconn = db.get().expect("DB pool problem");
    insert_into(videos::table)
        .values(&vids)
        .execute(dbconn.deref())
        .expect("Insertion of Videos Failed");
}
