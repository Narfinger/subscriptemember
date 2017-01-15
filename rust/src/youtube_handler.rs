extern crate yup_oauth2 as oauth2;
extern crate hyper;
use std::fmt;
use std::io::Read;
use hyper::Client;
use serde;
use serde_json;

const SUB_URL:&'static str = "https://www.googleapis.com/youtube/v3/subscriptions?part=snippet&mine=true&?access_token=";

#[derive(Serialize, Deserialize)]
struct YoutubeResult<T> {
    items : Vec<YoutubeItems<T>>
}

#[derive(Serialize, Deserialize)]
struct YoutubeItems<T> {
    iid : String,
    snippet : Option<T>,
    content_details : Option<T>
}

#[derive(Serialize, Deserialize)]
struct YoutubeSubscription {
    subscription_title : String,
    description : String,
    // resourceId : YoutubeResource,
    // thumbnails : YoutubeThumbnails,

}

#[derive(Eq,PartialEq,PartialOrd,Ord,Debug,Hash,Serialize,Deserialize)]
pub struct Subscription {
    pub sid : String,
    pub channelname : String,
    pub upload_playlist : String,
    pub thumbnail : String,
}

impl fmt::Display for Subscription {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(sid: {}, chnn: {}, uplpl: {}, thmb: {})", self.sid, self.channelname, self.upload_playlist, self.thumbnail)
    }
}

fn query<T>(t : &oauth2::Token, url : &'static str) -> YoutubeResult<T> where T: serde::Deserialize {
    let client = Client::new();
    let mut q  = String::from(url);
    q.push_str(t.access_token.as_str());
    let mut res = client.get(q.as_str()).send().unwrap();

    let mut s = String::new();
    res.read_to_string(&mut s);
    println!("{}", s);

    serde_json::from_reader(res).unwrap()

    // let mut s = String::new();
    // res.read_to_string(&mut s);
    // s
}

fn get_subscriptions_for_me(t : &oauth2::Token) -> Vec<YoutubeItems<YoutubeSubscription>> {
    let res : YoutubeResult<YoutubeSubscription> = query(t, SUB_URL);
    
    let ys = YoutubeSubscription { subscription_title : String::from("title test"), description : String::from("desc test")};
    let yi = YoutubeItems { iid : String::from("test iid"), snippet : None, content_details : Some(ys), };
    return vec![yi];
}

fn construct_subscription(s : &YoutubeItems<YoutubeSubscription>) -> Subscription {
    Subscription { sid : String::from("test sid"), channelname : String::from("testchannelname"), upload_playlist : String::from("uploadplaylist test"), thumbnail : String::from("testthumbnail")}
}

pub fn get_subs(t : &oauth2::Token) -> Vec<Subscription> {
    let ytsubs = get_subscriptions_for_me(t);
    let it = ytsubs.iter();
    it.map(construct_subscription).collect::<Vec<Subscription>>()
}


//     getSubscriptionsForMe :: C.Manager -> AccessToken -> IO [YoutubeItems YoutubeSubscription]
// getSubscriptionsForMe mgr token =
//   let qurl = constructQueryString "/subscriptions?&maxResults=50&part=snippet&mine=True" in
//   authGetJSONPages mgr token qurl :: (IO [YoutubeItems YoutubeSubscription])

