extern crate yup_oauth2 as oauth2;
extern crate hyper;

use std::sync::Mutex;
use std::iter::Iterator;
use diesel::sqlite::SqliteConnection;
use diesel::prelude::*;
use diesel::{insert,delete,update};

use youtube_base::{YoutubeItem,YoutubeSubscription,YoutubeRelatedPlaylists,YoutubeContentDetails,query};
use subs_and_video::{Subscription,NewSubscription};

const SUB_URL: &'static str = "https://www.googleapis.\
                               com/youtube/v3/subscriptions\
                               ?part=snippet&mine=true&maxResults=50&access_token=";

const UPLOAD_PL_URL: &'static str = "https://www.googleapis.com/youtube/v3/channels?part=contentDetails&maxResults=50&";

fn get_subscriptions_for_me(t: &oauth2::Token) -> Vec<YoutubeItem<YoutubeSubscription>> {
    query(t, SUB_URL)
}

fn get_upload_playlists(t: &oauth2::Token, subs: &mut Vec<Subscription>) {
    print!("starting\n");
    let mut upload_playlists: Vec<YoutubeItem<YoutubeContentDetails>> = Vec::new();
    for chunk in subs.chunks(50) {
        let onlyids = chunk.iter().map(| s: &Subscription| s.channelid.clone());
        let mut singlestringids: String = onlyids.fold("".to_string(), |comb:String, s| comb + &s + ",");
        singlestringids.pop();
        let queryurl = UPLOAD_PL_URL.to_string() + "id=" + &singlestringids + "&access_token=";
        let mut res : Vec<YoutubeItem<YoutubeContentDetails>> = query(t, &queryurl);
        upload_playlists.append(&mut res);
    }

    
    //match them
    match_subs_to_res(subs, &upload_playlists);
    print!("first: {}", subs[0].uploadplaylist);
}

fn match_subs_to_res(subs: &mut Vec<Subscription>, ups: &[YoutubeItem<YoutubeContentDetails>]) {
    fn find_and_replace(s: &mut Subscription, ups: &[YoutubeItem<YoutubeContentDetails>]) {
        let val: &YoutubeItem<YoutubeContentDetails> = ups.iter().find(|ups_elem| ups_elem.iid == s.channelid).unwrap();
        let v = val.content_details.as_ref().and_then(|ref cd| cd.related_playlists.as_ref()).map(|ref rlp| rlp.uploads.clone()).unwrap_or("No Playlist Found".to_string());
        s.uploadplaylist = v;
    }

    for s in subs {
        find_and_replace(s,ups);
    }
}

fn construct_subscription(s: YoutubeItem<YoutubeSubscription>) -> NewSubscription {
    let item = s.snippet.unwrap();
    NewSubscription {
        channelname: item.subscription_title,
        channelid: item.resource_id.channel_id,
        uploadplaylist: String::from("default value"),
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
    let mut nsubs = subscriptions.load::<Subscription>(dbconn).unwrap();
    if update_subs {
        get_upload_playlists(t, &mut nsubs);
        //update values
        for s in &nsubs {
            update(subscriptions.find(s.sid)).set(uploadplaylist.eq(s.uploadplaylist.clone())).execute(dbconn);
        }
    }
    nsubs
}
