use oauth2;
use std::sync::Mutex;
use std::iter::{Iterator,empty,Chain};
use diesel::sqlite::SqliteConnection;
use diesel::prelude::*;
use diesel::{insert,delete,update};

use youtube_base::{YoutubeItem,YoutubeSubscription,YoutubeContentDetails,Query,query};
use subs_and_video::{Subscription,NewSubscription};

const SUB_URL: &'static str = "https://www.googleapis.\
                               com/youtube/v3/subscriptions\
                               ?part=snippet&mine=true&maxResults=50&access_token=";

const UPLOAD_PL_URL: &'static str = "https://www.googleapis.com/youtube/v3/channels?part=contentDetails&maxResults=50&";

fn get_subscriptions_for_me(t: &oauth2::Token) -> Query<YoutubeSubscription> {
    query(t, SUB_URL)
}

fn get_upload_playlists(t: &oauth2::Token, subs: &mut Vec<Subscription>) {
    let mut singlestringids = subs.iter().map(|s: &Subscription| s.channelid.clone())
                              .fold("".to_string(), |comb: String, s| comb + &s + ",");
    singlestringids.pop();
    let queryurl = UPLOAD_PL_URL.to_string() + "id=" + &singlestringids + &"access_token=";
    let res = query(t, &queryurl).collect::<Vec<YoutubeItem<YoutubeContentDetails>>>();
    
    // for chunk in subs.chunks(50) {
    //     let onlyids = chunk.iter().map(| s: &Subscription| s.channelid.clone());
    //     let mut singlestringids: String = onlyids.fold("".to_string(), |comb:String, s| comb + &s + ",");
    //     singlestringids.pop();
    //     let queryurl = UPLOAD_PL_URL.to_string() + "id=" + &singlestringids + "&access_token=";
    //     let mut res : Query<YoutubeContentDetails> = query(t, &queryurl);

    //     upload_playlists = upload_playlists.chain(res);
    // }

    //match them
    match_subs_to_res(subs, &res);
}

fn match_subs_to_res(subs: &mut Vec<Subscription>, ups: &Vec<YoutubeItem<YoutubeContentDetails>>) {
    fn find_and_replace(s: &mut Subscription, ups: &Vec<YoutubeItem<YoutubeContentDetails>>) {
        let val: &YoutubeItem<YoutubeContentDetails> = ups.iter().find(|ups_elem| ups_elem.iid == s.channelid).unwrap();
        let v = val.content_details.as_ref().and_then(|cd| cd.related_playlists.as_ref()).map(|rlp| rlp.uploads.clone()).unwrap_or_else(||"No Playlist Found".to_string());
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
        channelid: item.resource_id.channel_id.unwrap(),
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
