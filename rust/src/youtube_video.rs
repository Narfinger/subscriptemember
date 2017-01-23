extern crate yup_oauth2 as oauth2;
extern crate hyper;

use std::sync::Mutex;
use std::iter::Iterator;
use diesel::sqlite::SqliteConnection;
use diesel::prelude::*;
use diesel::{insert,delete,update};

use youtube_base::{YoutubeItem,YoutubeSnippet,query};
use subs_and_video::{Subscription,Video};

const PL_URL: &'static str = "https://www.googleapis.com/youtube/v3/playlistItems?part=snippet&maxResults=5&playlistId=";



fn query_videos(t: &oauth2::Token, subs: &Vec<Subscription>) -> Vec<YoutubeItem<YoutubeSnippet>> {
    let mut res = Vec::new();
    for s in subs {
        let mut qstring = PL_URL.to_string() + &s.uploadplaylist +   "&access_token=";
        let mut r = query(t, &qstring);
        res.append(&mut r);
    }
    return res;
}

fn construct_new_video(s: YoutubeItem<YoutubeSnippet>) -> NewVideo {

}
