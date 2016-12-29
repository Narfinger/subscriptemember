extern crate yup_oauth2 as oauth2;
use self::oauth2::Token;

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

#[derive(Eq,PartialEq,PartialOrd,Ord,Debug)]
pub struct Subscription {
    pub sid : String,
    pub channelname : String,
    pub uploadPlaylist : String,
    pub thumbnail : String,
}


fn get_subscriptions_for_me(t : &oauth2::Token) -> &Vec<YoutubeItems<YoutubeSubscription>> {
    let ys = YoutubeSubscription { subscriptiontitle : String::from("title test"), description : String::from("desc test")};
    let yi = YoutubeItems { iid : String::from("test iid"), snippet : None, contentDetails : Some(ys), };
    return &vec![yi];
}

fn construct_subscription(s : YoutubeItems<YoutubeSubscription>) -> Subscription {
    return Subscription { sid : String::from("test sid"), channelname : String::from("testchannelname"), uploadPlaylist : String::from("uploadplaylist test"), thumbnail : String::from("testthumbnail")};
}

pub fn get_subs(t : &oauth2::Token) -> Vec<Subscription> {
    return get_subscriptions_for_me(t).iter().map(construct_subscription).collect::<Vec<_>>();
}


//     getSubscriptionsForMe :: C.Manager -> AccessToken -> IO [YoutubeItems YoutubeSubscription]
// getSubscriptionsForMe mgr token =
//   let qurl = constructQueryString "/subscriptions?&maxResults=50&part=snippet&mine=True" in
//   authGetJSONPages mgr token qurl :: (IO [YoutubeItems YoutubeSubscription])

