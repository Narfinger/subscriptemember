use oauth2;
use std::sync::Mutex;
use std::iter::Iterator;
use chrono::UTC;
use diesel::sqlite::SqliteConnection;
use diesel::prelude::*;
use diesel::{insert,delete};
use youtube_base::{YoutubeItem,YoutubeSnippet,query};
use subs_and_video;
use subs_and_video::{Subscription,Video,NewVideo,NewConfig,get_lastupdate_in_unixtime,make_youtube_url};

const PL_URL: &'static str = "https://www.googleapis.com/youtube/v3/playlistItems?part=snippet&maxResults=50&playlistId=";


fn query_videos<'f>(t: &'f oauth2::Token, subs: &'f [Subscription], unix_stamp: i64) -> Vec<NewVideo> {
    fn build_string(s: &Subscription) -> String {
        PL_URL.to_string() + &s.uploadplaylist + "&access_token="
    }
    
    subs.iter()
        .map(|s| (s,query::<YoutubeSnippet>(t, &build_string(s))))
        .map(|(s,q)| (s,q.take(10)))
        .flat_map(|(s,q)|
                  q.map(move |sn| construct_new_video(s,sn)))
        .filter(|s| s.published_at > unix_stamp)
        .collect::<Vec<NewVideo>>()
}

fn construct_new_video(s :&Subscription, i: YoutubeItem<YoutubeSnippet>) -> NewVideo {
    let snippet = i.snippet.unwrap();
    let vid = snippet.resource.video_id.unwrap();
    NewVideo{vid: vid.clone(),
             title: snippet.title,
             thumbnail: snippet.thumbnails.default.thmburl,
             published_at: subs_and_video::from_youtube_datetime_to_timestamp(&snippet.published_at),
             channelname: s.channelname.clone(),
             //duration: "".to_string(),
             url: make_youtube_url(vid),
    }
}

pub fn update_videos(t: &oauth2::Token, db: &Mutex<SqliteConnection>, subs: &[Subscription]) {
    use schema::videos;
    use schema::config;


    let us = get_lastupdate_in_unixtime(db);
    let vids:Vec<NewVideo> = query_videos(t,subs, us);

    
    println!("New Videos length: {}", vids.len());
    let dbconn: &SqliteConnection = &db.lock().unwrap();
    insert(&vids)
        .into(videos::table)
        .execute(dbconn)
        .expect("Insertion of Videos Failed");
    delete(config::table).execute(dbconn).expect("Deletion of old config failed");
    let nc = NewConfig{ lastupdate: UTC::now().to_rfc3339()};
    insert(&nc).into(config::table).execute(dbconn).expect("Insertion of config failed");
}


pub fn get_videos(db: &Mutex<SqliteConnection>) -> Vec<Video> {
    use schema::videos::dsl::*;
    
    let dbconn: &SqliteConnection = &db.lock().unwrap();

    videos.order(published_at.desc())
        .load(dbconn)
        .unwrap()
}

pub fn delete_video(db: &Mutex<SqliteConnection>, videoid: &str) {
    use schema::videos::dsl::*;

    let dbconn: &SqliteConnection = &db.lock().unwrap();
    delete(videos.filter(vid.like(videoid)))
        .execute(dbconn)
        .expect("Deleting failed");
}
