extern crate yup_oauth2 as oauth2;
use std::fmt;

struct YoutubeItems<T> {
    iid : String,
    snippet : Option<T>,
    content_details : Option<T>
}

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

fn get_subscriptions_for_me(t : &oauth2::Token) -> Vec<YoutubeItems<YoutubeSubscription>> {
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

