extern crate yup_oauth2 as oauth2;
extern crate hyper;

use std::fmt;
use std::sync::Mutex;
use hyper::Client;
use serde;
use serde_json;
use diesel::sqlite::SqliteConnection;
use diesel::prelude::*;
use diesel::{insert, delete};
use schema::subscriptions;

const SUB_URL: &'static str = "https://www.googleapis.\
                               com/youtube/v3/subscriptions\
                               ?part=snippet&mine=true&maxResults=50&access_token=";

const UPLOAD_PL_URL: &'static str = "https://www.googleapis.com/youtube/v3/channels?part=contentDetails&access_token=";

#[derive(Serialize, Deserialize)]
struct YoutubePageInfo {
    #[serde(rename="totalResults")]
    total_results: i32,

    #[serde(rename="resultsPerPage")]
    results_per_page: i32,
}

#[derive(Serialize, Deserialize)]
struct YoutubeResult<T> {
    items: Vec<YoutubeItem<T>>,
    #[serde(rename="pageInfo")]
    page_info: YoutubePageInfo,
    #[serde(rename="nextPageToken")]
    next_page_token: Option<String>,
}

#[derive(Serialize, Deserialize)]
struct YoutubeItem<T> {
    #[serde(rename="id")]
    iid: String,
    snippet: Option<T>,
    content_details: Option<T>,
}

#[derive(Serialize, Deserialize)]
struct YoutubeRelatedPlaylists {
    uploads: String,
}

#[derive(Serialize, Deserialize)]
struct YoutubeThumbnailDetail {
    #[serde(rename="url")]
    thmburl: String,
}

#[derive(Serialize, Deserialize)]
struct YoutubeThumbnails {
    default: YoutubeThumbnailDetail,
    medium: YoutubeThumbnailDetail,
    high: YoutubeThumbnailDetail,
}

#[derive(Serialize, Deserialize)]
struct YoutubeResource {
    kind: String,
    #[serde(rename="channelId")]
    channel_id: String,
}

#[derive(Serialize, Deserialize)]
struct YoutubeSubscription {
    #[serde(rename="title")]
    subscription_title: String,
    #[serde(rename="description")]
    sdescription: String,
    #[serde(rename="channelId")]
    channel_id: String,
    thumbnails: YoutubeThumbnails,
    #[serde(rename="resourceId")]
    resource_id: YoutubeResource,
}

#[derive(Eq,PartialEq,PartialOrd,Ord,Debug,Hash,Serialize,Deserialize,Queryable)]
pub struct Subscription {
    pub sid: i32,
    pub channelid: String,
    pub channelname: String,
    pub upload_playlist: String,
    pub thumbnail: String,
    pub description: String,
}

#[derive(Insertable)]
#[table_name="subscriptions"]
struct NewSubscription {
    channelid: String,
    channelname: String,
    uploadplaylist: String,
    thumbnail: String,
    description: String,
}

impl fmt::Display for Subscription {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "(sid: {}, chnn: {}, uplpl: {}, thmb: {})",
               self.sid,
               self.channelname,
               self.upload_playlist,
               self.thumbnail)
    }
}

fn query_simple_page<T>(t: &oauth2::Token,
                        url: &'static str,
                        nextpage: Option<String>)
                        -> YoutubeResult<T>
    where T: serde::Deserialize
{
    let client = Client::new();
    let mut q = String::from(url);
    q.push_str(t.access_token.as_str());
    if let Some(nextpagetk) = nextpage {
        q.push_str("&pageToken=");
        q.push_str(nextpagetk.as_str());
    }

    let res = client.get(q.as_str()).send().unwrap();
    //    res.read_to_string(&mut s);
    println!("query: {}", q);

    serde_json::from_reader(res).unwrap()

    // let mut s = String::new();
    // res.read_to_string(&mut s);
    // s
}

fn query<T>(t: &oauth2::Token, url: &'static str) -> Vec<YoutubeItem<T>>
    where T: serde::Deserialize
{
    let mut result: Vec<YoutubeItem<T>> = Vec::new();
    let mut next_page = None;
    loop {
        let res = query_simple_page(t, url, next_page);
        result.extend(res.items);
        next_page = res.next_page_token;
        println!("query again with tk:");
        if next_page.is_none() {
            break;
        }
    }
    result
}

fn get_subscriptions_for_me(t: &oauth2::Token) -> Vec<YoutubeItem<YoutubeSubscription>> {
    query(t, SUB_URL)
}

fn get_upload_playlists(t: &oauth2::Token, subs: &Subscription) -> Vec<YoutubeItem<YoutubeRelatedPlaylists>> {
    let mut vec: Vec<String> = Vec::new();
    for chunk in subs.chunk(50) {
        let ids: String = chunk.into_iter().map(| s: Subscription| s.channelid).fold("", |comb, s| comb.push_str(s)).collect();
    }
}

fn construct_subscription(s: YoutubeItem<YoutubeSubscription>) -> NewSubscription {
    let item = s.snippet.unwrap();
    NewSubscription {
        channelname: item.subscription_title,
        channelid: item.resource_id.channel_id,
        uploadplaylist: String::from("test playlist"),
        thumbnail: item.thumbnails.default.thmburl,
        description: item.sdescription,
    }
}

pub fn get_subs(t: &oauth2::Token,
                db: &Mutex<SqliteConnection>,
                update_subs: bool)
                -> Vec<Subscription> {
    use schema::subscriptions::dsl::*;
    use schema::subscriptions;

    let dbconn: &SqliteConnection = &db.lock().unwrap();
    if update_subs {
        delete(subscriptions::table).execute(dbconn);

        let ytsubs = get_subscriptions_for_me(t);
        let it = ytsubs.into_iter();
        let subs = it.map(construct_subscription).collect::<Vec<NewSubscription>>();
        insert(&subs)
            .into(subscriptions::table)
            .execute(dbconn);
    }
    subscriptions.load::<Subscription>(dbconn).unwrap()
}
