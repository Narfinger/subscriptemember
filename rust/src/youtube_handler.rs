extern crate yup_oauth2 as oauth2;
extern crate hyper;
use std::fmt;
use std::io::Read;
use hyper::{Client, Url};
use serde;
use serde_json;

const SUB_URL:&'static str = "https://www.googleapis.com/youtube/v3/subscriptions?part=snippet&mine=true&maxResults=50&access_token=";

#[derive(Serialize, Deserialize)]
struct YoutubePageInfo {
    totalResults : i32,
    resultsPerPage: i32,
}

#[derive(Serialize, Deserialize)]
struct YoutubeResult<T> {
    items : Vec<YoutubeItem<T>>,
    pageInfo : YoutubePageInfo,
    nextPageToken : Option<String>
}

#[derive(Serialize, Deserialize)]
struct YoutubeItem<T> {
    #[serde(rename="id")]
    iid : String,
    snippet : Option<T>,
    content_details : Option<T>
}

#[derive(Serialize, Deserialize)]
struct YoutubeThumbnailDetail {
    #[serde(rename="url")]
    thmburl : String
}

#[derive(Serialize, Deserialize)]
struct YoutubeThumbnails {
    default : YoutubeThumbnailDetail,
    medium : YoutubeThumbnailDetail,
    high : YoutubeThumbnailDetail
}

#[derive(Serialize, Deserialize)]
struct YoutubeSubscription {
    #[serde(rename="title")]
    subscription_title : String,
    #[serde(rename="description")]
    sdescription : String,
    channelId : String,
    thumbnails : YoutubeThumbnails
    
    // resourceId : YoutubeResource,
    // thumbnails : YoutubeThumbnails,

}

#[derive(Eq,PartialEq,PartialOrd,Ord,Debug,Hash,Serialize,Deserialize)]
pub struct Subscription {
    pub sid : String,
    pub channelname : String,
    pub upload_playlist : String,
    pub thumbnail : String,
    pub description : String,
}

impl fmt::Display for Subscription {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(sid: {}, chnn: {}, uplpl: {}, thmb: {})", self.sid, self.channelname, self.upload_playlist, self.thumbnail)
    }
}

fn querySimplePage<T>(t : &oauth2::Token, url : &'static str, nextpage : Option<String>) -> YoutubeResult<T> where T: serde::Deserialize {
    let client = Client::new();
    let mut q  = String::from(url);
    q.push_str(t.access_token.as_str());
    if let Some(nextpagetk) = nextpage {
        q.push_str("&pageToken=");
        q.push_str(nextpagetk.as_str());
    }

    let mut res = client.get(q.as_str()).send().unwrap();

    let mut s = String::new();
//    res.read_to_string(&mut s);
    println!("query: {}, result: {}", q, s);

    serde_json::from_reader(res).unwrap()

    // let mut s = String::new();
    // res.read_to_string(&mut s);
    // s
}

fn query<T>(t : &oauth2::Token, url : &'static str) -> Vec<YoutubeItem<T>> where T : serde::Deserialize {
    let mut result : Vec<YoutubeItem<T>> = Vec::new();
    let mut nextPage = None;
    loop {
        let res = querySimplePage(t,url,nextPage);
        result.extend(res.items);
        nextPage = res.nextPageToken;
        println!("query again with tk:");
        if nextPage.is_none() { break;}
    }
    result
}

fn get_subscriptions_for_me(t : &oauth2::Token) -> Vec<YoutubeItem<YoutubeSubscription>> {
    query(t, SUB_URL)
}

fn construct_subscription(s : YoutubeItem<YoutubeSubscription>) -> Subscription {
    let item = s.snippet.unwrap();
    Subscription { sid : String::from("stitch responses together"), channelname : item.subscription_title, upload_playlist : String::from("test playlist"), thumbnail : item.thumbnails.default.thmburl, description : item.sdescription}
}

pub fn get_subs(t : &oauth2::Token) -> Vec<Subscription> {
    let ytsubs = get_subscriptions_for_me(t);
    let it = ytsubs.into_iter();
    it.map(construct_subscription).collect::<Vec<Subscription>>()
}


//     getSubscriptionsForMe :: C.Manager -> AccessToken -> IO [YoutubeItems YoutubeSubscription]
// getSubscriptionsForMe mgr token =
//   let qurl = constructQueryString "/subscriptions?&maxResults=50&part=snippet&mine=True" in
//   authGetJSONPages mgr token qurl :: (IO [YoutubeItems YoutubeSubscription])

