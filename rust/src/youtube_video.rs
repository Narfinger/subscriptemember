use oauth2;
use std::sync::Mutex;
use std::slice::Iter;
use std::iter::{Iterator,FlatMap};
use diesel::sqlite::SqliteConnection;
use diesel::prelude::*;
use diesel::{insert,delete,update};


use youtube_base::{YoutubeItem,YoutubeSnippet,Query,query};
use subs_and_video::{Subscription,Video,NewVideo};

const PL_URL: &'static str = "https://www.googleapis.com/youtube/v3/playlistItems?part=snippet&maxResults=50&playlistId=";


fn query_videos<'f>(t: &'f oauth2::Token, subs: &'f Vec<Subscription>) -> Vec<NewVideo> {
    fn build_string(s: &Subscription) -> String {
        PL_URL.to_string() + &s.uploadplaylist + "&access_token="
    }
    
    subs.iter()
        .map(|s| (s,query::<YoutubeSnippet>(t, &build_string(s))))
        .map(|(s,q)| (s,q.take(10)))
        .flat_map(|(s,q)|
                  q.map(move |sn| construct_new_video(s,sn)))
        .collect::<Vec<NewVideo>>()
}

fn construct_new_video(s :&Subscription, i: YoutubeItem<YoutubeSnippet>) -> NewVideo {
    let snippet = i.snippet.unwrap();
    NewVideo{vid: snippet.resource.video_id.unwrap(),
             title: snippet.title,
             thumbnail: snippet.thumbnails.default.thmburl,
             published_at: "".to_string(),
             //duration: "".to_string(),
             url: "".to_string()}
}

pub fn update_videos(t: &oauth2::Token, db: &Mutex<SqliteConnection>, subs: &Vec<Subscription>) {
    use schema::videos::dsl::*;
    use schema::videos;

    let dbconn: &SqliteConnection = &db.lock().unwrap();
    //panic!("Something is wrong with the update, it queries the same thing forever");
    let vids:Vec<NewVideo> = query_videos(t,subs);
    insert(&vids)
        .into(videos::table)
        .execute(dbconn);
    
}


pub fn get_videos(db: &Mutex<SqliteConnection>) -> Vec<Video> {
    use schema::videos::dsl::*;
    use schema::videos;
    
    let dbconn: &SqliteConnection = &db.lock().unwrap();
    videos.load::<Video>(dbconn).unwrap()
}
