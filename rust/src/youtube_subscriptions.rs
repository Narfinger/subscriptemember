use std::iter::Iterator;
use diesel::sqlite::SqliteConnection;
use diesel::prelude::*;
use diesel::{insert_into, delete, update};
use diesel::r2d2::Pool;
use diesel::r2d2::ConnectionManager;
use std::ops::Deref;
use reqwest;
use crate::subs_and_video::{Subscription, NewSubscription};
use crate::youtube_base::{YoutubeItem, YoutubeSubscription, YoutubeRelatedPlaylistsContentDetails, Query,
                   query};
use crate::youtube_oauth;

const SUB_URL: &str = "https://www.googleapis.\
                               com/youtube/v3/subscriptions\
                               ?part=snippet&mine=true&maxResults=50&access_token=";

const UPLOAD_PL_URL: &str = "https://www.googleapis.\
                                     com/youtube/v3/channels?part=contentDetails&maxResults=50&";

fn get_subscriptions_for_me<'a>(t: &youtube_oauth::Token, client: &'a reqwest::Client) -> Query<'a,YoutubeSubscription> {
    query(t, client, SUB_URL)
}

fn get_upload_playlists(t: &youtube_oauth::Token, client: &reqwest::Client, subs: &mut Vec<Subscription>) {
    let mut upload_playlist: Vec<YoutubeItem<YoutubeRelatedPlaylistsContentDetails>> = Vec::new();
    for chunk in subs.chunks(50) {
        let onlyids = chunk.iter().map(|s: &Subscription| s.channelid.clone());
        let mut singlestringids: String =
            onlyids.fold("".to_string(), |comb: String, s| comb + &s + ",");
        singlestringids.pop();
        let queryurl = UPLOAD_PL_URL.to_string() + "id=" + &singlestringids + "&access_token=";
        let res: Query<YoutubeRelatedPlaylistsContentDetails> = query(t, client, &queryurl);

        println!("this is super inefficient");
        let mut realres = res.collect::<Vec<YoutubeItem<YoutubeRelatedPlaylistsContentDetails>>>();
        upload_playlist.append(&mut realres);
    }

    //match them
    match_subs_to_res(subs, &upload_playlist);
}

fn match_subs_to_res(subs: &mut Vec<Subscription>,
                     ups: &[YoutubeItem<YoutubeRelatedPlaylistsContentDetails>]) {
    fn find_and_replace(s: &mut Subscription,
                        ups: &[YoutubeItem<YoutubeRelatedPlaylistsContentDetails>]) {
        let val: &YoutubeItem<YoutubeRelatedPlaylistsContentDetails> =
            ups.iter().find(|ups_elem| ups_elem.iid == s.channelid).unwrap();
        let v = val.content_details
            .as_ref()
            .and_then(|cd| cd.related_playlists.as_ref())
            .map(|rlp| rlp.uploads.clone())
            .unwrap_or_else(|| "No Playlist Found".to_string());
        s.uploadplaylist = v;
    }

    for s in subs {
        find_and_replace(s, ups);
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

pub fn get_subs(t: &youtube_oauth::Token,
                db: &Pool<ConnectionManager<SqliteConnection>>,
                client: &reqwest::Client,
                update_subs: bool)
                -> Vec<Subscription> {
    use crate::schema::subscriptions::dsl::*;
    use crate::schema::subscriptions;

    let dbconn = db.get().expect("DB pool problem");
    if update_subs {
        delete(subscriptions::table)
            .execute(dbconn.deref())
            .expect("Deletion of old subscriptions failed");

        let subs = get_subscriptions_for_me(t,client)
            .map(construct_subscription)
            .collect::<Vec<NewSubscription>>();
        insert_into(subscriptions::table)
            .values(&subs)
            .execute(dbconn.deref())
            .expect("Insertion of new subs failed");
    }
    let mut nsubs = subscriptions.load::<Subscription>(dbconn.deref()).unwrap();
    if update_subs {
        get_upload_playlists(t, client, &mut nsubs);
        //update values
        for s in &nsubs {
            update(subscriptions.find(s.sid))
                .set(uploadplaylist.eq(s.uploadplaylist.clone()))
                .execute(dbconn.deref())
                .expect("Updating Playlist failed on a sub");
        }
    }
    nsubs
}
