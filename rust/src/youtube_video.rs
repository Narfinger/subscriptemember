use oauth2;
use std::sync::Mutex;
use std::iter::Iterator;
use chrono::{DateTime,UTC,FixedOffset};
use diesel::sqlite::SqliteConnection;
use diesel::prelude::*;
use diesel::{insert,delete};
use youtube_base::{YoutubeItem,YoutubeSnippet,query};
use subs_and_video;
use subs_and_video::{Subscription,Video,NewVideo,ToUnixTime,Config,NewConfig};

const PL_URL: &'static str = "https://www.googleapis.com/youtube/v3/playlistItems?part=snippet&maxResults=50&playlistId=";


fn query_videos<'f>(t: &'f oauth2::Token, subs: &'f Vec<Subscription>, unix_stamp: i64) -> Vec<NewVideo> {
    fn build_string(s: &Subscription) -> String {
        PL_URL.to_string() + &s.uploadplaylist + "&access_token="
    }
    
    subs.iter()
        .map(|s| (s,query::<YoutubeSnippet>(t, &build_string(s))))
        .map(|(s,q)| (s,q.take(10)))
        .flat_map(|(s,q)|
                  q.map(move |sn| construct_new_video(s,sn)))
        .filter(|s| s.to_unix() > unix_stamp)
        .collect::<Vec<NewVideo>>()
}

fn construct_new_video(s :&Subscription, i: YoutubeItem<YoutubeSnippet>) -> NewVideo {
    let snippet = i.snippet.unwrap();
    NewVideo{vid: snippet.resource.video_id.unwrap(),
             title: snippet.title,
             thumbnail: snippet.thumbnails.default.thmburl,
             published_at: subs_and_video::from_youtube_datetime_to_string(&snippet.published_at),
             channelname: s.channelname.clone(),
             //duration: "".to_string(),
             url: "".to_string()}
}

fn get_lastupdate_in_unixtime(db: &Mutex<SqliteConnection>) -> i64 {
    use schema::config::dsl::*;

    let dbconn: &SqliteConnection = &db.lock().unwrap();
    let val = config.load::<Config>(dbconn).unwrap();

    if val.is_empty() {
        0
    } else {
        DateTime::parse_from_rfc3339(&val[0].lastupdate).map(|s| s.timestamp()).unwrap_or(0);
    }
}

pub fn update_videos(t: &oauth2::Token, db: &Mutex<SqliteConnection>, subs: &Vec<Subscription>) {
    use schema::videos;
    use schema::config;


    let us = get_lastupdate_in_unixtime(db);
    let dbconn: &SqliteConnection = &db.lock().unwrap();

    let vids:Vec<NewVideo> = query_videos(t,subs, us);

    
    
    insert(&vids)
        .into(videos::table)
        .execute(dbconn);
    delete(config::table).execute(dbconn);
    let nc = NewConfig{ lastupdate: UTC::now().to_rfc3339()};
    insert(&nc).into(config::table).execute(dbconn);
}


pub fn get_videos(db: &Mutex<SqliteConnection>) -> Vec<Video> {
    use schema::videos::dsl::*;
    
    let dbconn: &SqliteConnection = &db.lock().unwrap();
    videos.load::<Video>(dbconn).unwrap()
}
