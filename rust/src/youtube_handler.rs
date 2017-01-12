extern crate yup_oauth2 as oauth2;
extern crate liquid;
use self::oauth2::Token;
use std::fmt;
use std::io::Read;
use std::fs::File;
use liquid::{Value,Context,Renderable,Error};


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

impl Renderable for Subscription {
    fn render(&self, ctx: &mut Context) -> Result<Option<String>, liquid::Error> {
        let mut f = match File::open("templates/sub.html") {
            Ok(ff) => {ff}
            Err(_) => panic!("template not found")
        };
        
        let mut s = String::new();
        let bytesread = match f.read_to_string(&mut s) {
            Ok(br) => {br}
            Err(_) => panic!("could not read template to string")
        };
        
        let template = match liquid::parse(&s, Default::default()) {
            Ok(te) => {te}
            Err(_) => panic!("something wrong with template parsing")
        };
        ctx.set_local_val("sid", Value::Str(self.sid.clone()));
        ctx.set_local_val("channelname", Value::Str(self.channelname.clone()));
        ctx.set_local_val("uploadPlaylist", Value::Str(self.uploadPlaylist.clone()));
        ctx.set_local_val("thumbnail", Value::Str(self.thumbnail.clone()));
        return template.render(ctx);
    }
}

type Subscriptions = Vec<Subscription>;

impl Renderable for Subscriptions {
    fn render(&self, ctx: &mut Context) -> Result<Option<String>, liquid::Error> {
        let mut f = match File::open("templates/subs.html") {
            Ok(ff) => {ff}
            Err(_) => panic!("template not found")
        };
        
        let mut s = String::new();
        let bytesread = match f.read_to_string(&mut s) {
            Ok(br) => {br}
            Err(_) => panic!("could not read template to string")
        };
        
        let template = match liquid::parse(&s, Default::default()) {
            Ok(te) => {te}
            Err(_) => panic!("something wrong with template parsing")
        };
        ctx.set_local_val("subs", Value::Array(self));
        return template.render(ctx);
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

