extern crate yup_oauth2 as oauth2;
use self::oauth2::Token;
use std::fmt;
use std::collections::HashMap;

struct YoutubeItems<T> {
    iid : String,
    snippet : Option<T>,
    contentDetails : Option<T>
}

struct YoutubeSubscription {
    subscriptiontitle : String,
    description : String,
    // resourceId : YoutubeResource,
    // thumbnails : YoutubeThumbnails,

}


pub trait HashMapable {
    fn toHMap(&self) -> HashMap<String,String>; 
}

#[derive(Eq,PartialEq,PartialOrd,Ord,Debug,Hash)]
pub struct Subscription {
    pub sid : String,
    pub channelname : String,
    pub uploadPlaylist : String,
    pub thumbnail : String,
}

impl fmt::Display for Subscription {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(sid: {}, chnn: {}, uplpl: {}, thmb: {})", self.sid, self.channelname, self.uploadPlaylist, self.thumbnail)
    }
}

impl HashMapable for Subscription {
    fn toHMap(&self) -> HashMap<String, String>{
        let mut hmap = HashMap::new();
        hmap.insert(String::from("sid"), self.sid);
        hmap.insert(String::from("channelname"), self.channelname);
        hmap.insert(String::from("uploadPlaylist"), self.uploadPlaylist);
        hmap.insert(String::from("thumbnail"), self.thumbnail);
        return hmap;
    }
}

fn get_subscriptions_for_me(t : &oauth2::Token) -> Vec<YoutubeItems<YoutubeSubscription>> {
    let ys = YoutubeSubscription { subscriptiontitle : String::from("title test"), description : String::from("desc test")};
    let yi = YoutubeItems { iid : String::from("test iid"), snippet : None, contentDetails : Some(ys), };
    return vec![yi];
}

fn construct_subscription(s : &YoutubeItems<YoutubeSubscription>) -> Subscription {
    return Subscription { sid : String::from("test sid"), channelname : String::from("testchannelname"), uploadPlaylist : String::from("uploadplaylist test"), thumbnail : String::from("testthumbnail")};
}

pub fn get_subs(t : &oauth2::Token) -> Vec<Subscription> {
    let ytsubs = get_subscriptions_for_me(t);
    let mut it = ytsubs.iter();
    return it.map(construct_subscription).collect::<Vec<Subscription>>();
}


//     getSubscriptionsForMe :: C.Manager -> AccessToken -> IO [YoutubeItems YoutubeSubscription]
// getSubscriptionsForMe mgr token =
//   let qurl = constructQueryString "/subscriptions?&maxResults=50&part=snippet&mine=True" in
//   authGetJSONPages mgr token qurl :: (IO [YoutubeItems YoutubeSubscription])

