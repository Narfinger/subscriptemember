use oauth2;
use std::sync::Mutex;
use std::iter::Iterator;
use diesel::sqlite::SqliteConnection;
use diesel::prelude::*;
use diesel::{insert,delete,update};


use youtube_base::{YoutubeItem,YoutubeSnippet,query};
use subs_and_video::{Subscription,Video,NewVideo};

const PL_URL: &'static str = "https://www.googleapis.com/youtube/v3/playlistItems?part=snippet&maxResults=5&playlistId=";



fn query_videos(t: &oauth2::Token, subs: &Vec<Subscription>) -> Vec<YoutubeItem<YoutubeSnippet>> {
    let mut res = Vec::new();
    for s in subs {
        let qstring = PL_URL.to_string() + &s.uploadplaylist +   "&access_token=";
        let mut r = query(t, &qstring);
        res.append(&mut r);
    }
    return res;
}

fn construct_new_video(s: YoutubeItem<YoutubeSnippet>) -> NewVideo {
    let snippet = s.snippet.unwrap();
    NewVideo{vid: s.iid,
             title: snippet.title,
             thumbnail: snippet.thumbnails.default.thmburl,
             published_at: "".to_string(),
             //duration: "".to_string(),
             url: "".to_string()}
}

pub fn update_videos(t: &oauth2::Token, db: &Mutex<SqliteConnection>, subs: &Vec<Subscription>) -> Vec<Video> {
    use schema::videos::dsl::*;
    use schema::videos;

    let dbconn: &SqliteConnection = &db.lock().unwrap();
    let vids = query_videos(t,subs).into_iter().map(construct_new_video);
    insert(&vids)
        .into(videos::table)
        .execute(dbconn);

    let nvids = videos.load::<Video>(dbconn).unwrap();
    nvids
}
