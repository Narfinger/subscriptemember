use std::iter::Iterator;
use std::ops::Deref;
use chrono::Utc;
use diesel::sqlite::SqliteConnection;
use diesel::prelude::*;
use diesel::{insert_into, delete};
use rayon::prelude::*;
use reqwest;
use r2d2::Pool;
use r2d2_diesel::ConnectionManager;
use youtube_base::{YoutubeItem, YoutubeSnippet, YoutubeDurationContentDetails, query};
use subs_and_video;
use subs_and_video::{Subscription, Video, NewVideo, NewConfig, get_lastupdate_in_unixtime,
                     make_youtube_url, youtube_duration};
use youtube_oauth;


const PL_URL: &str = "https://www.googleapis.\
                              com/youtube/v3/playlistItems?part=snippet&maxResults=50&playlistId=";

const VID_URL: &str = "https://www.googleapis.\
                               com/youtube/v3/videos?part=contentDetails&maxResults=50&id=";

/// Queries current Youtube Videos, filters them by `unix_stamp`
/// and uses `subs` to get the uploadplaylist ids`
fn query_videos<'f>(t: &'f youtube_oauth::Token,
                    client: &reqwest::Client,
                    subs: &'f [Subscription],
                    unix_stamp: i64)
                    -> Vec<NewVideo> {
    fn build_string(s: &Subscription) -> String {
        PL_URL.to_string() + &s.uploadplaylist + "&access_token="
    }

    //why does this not work? it should be better
    // subs.par_iter()
    //     .map(|s| (s, query::<YoutubeSnippet>(t, &build_string(s))))
    //     .map(|(s, q)| (s, q.take(10).collect::<Vec<YoutubeItem<YoutubeSnippet>>>()))
    //     .flat_map(|&(s, ref q)| q.into_iter().map(move |sn| construct_new_video(s, sn)))
    //     .filter(|s| s.published_at > unix_stamp)
    //     .collect::<Vec<NewVideo>>()

    let vids = subs.par_iter()
        .map(|s| (s, query::<YoutubeSnippet>(t, client, &build_string(s))))
        .map(|(s, q)| (s, q.take(10).collect::<Vec<YoutubeItem<YoutubeSnippet>>>()))
        .collect::<Vec<(&Subscription, Vec<YoutubeItem<YoutubeSnippet>>)>>();
    vids.iter()
        .flat_map(|&(s, ref q)| q.into_iter().map(move |sn| construct_new_video(s, sn)))
        .filter(|s| s.published_at > unix_stamp)
        .collect::<Vec<NewVideo>>()
}

/// Takes a single `YoutubeItem<YoutubeSnippet>` and constructs a `NewVideo` according to this
fn construct_new_video(s: &Subscription, i: &YoutubeItem<YoutubeSnippet>) -> NewVideo {
    let snippet = i.snippet.as_ref().unwrap();
    let vid = snippet.resource.video_id.as_ref().unwrap();
    NewVideo {
        vid: vid.clone(),
        title: snippet.title.clone(),
        thumbnail: snippet.thumbnails.default.thmburl.clone(),
        published_at: subs_and_video::from_youtube_datetime_to_timestamp(&snippet.published_at),
        channelname: s.channelname.clone(),
        duration: 0,
        url: make_youtube_url(vid),
    }
}

/// Gets a current `YoutubeItem<YoutubeDurationcontentdetails>`,
/// finds the appropiate video in `v` and updates the running time
fn update_vid_time(i: &YoutubeItem<YoutubeDurationContentDetails>, v: &mut [NewVideo]) {
    let pos = v.iter().position(|e| e.vid == i.iid);
    if pos.is_some() {
        //println!("Dur: {}", i.content_details.as_ref().unwrap().duration );
        let dur = youtube_duration(i.content_details.as_ref().unwrap().duration.as_bytes())
            .to_result()
            .unwrap_or(0) as i64;
        v[pos.unwrap()].duration = dur;
        //v[pos.unwrap()].duration = 0;
    }
}

/// update all running time of videos given in `v`
fn update_video_running_time(t: &youtube_oauth::Token, client: &reqwest::Client, v: &mut Vec<NewVideo>) {
    for chunk in v.chunks_mut(50) {
        let mut singlestringids = chunk.iter()
            .map(|s: &NewVideo| s.vid.clone())
            .fold("".to_string(), |comb: String, s| comb + &s + ",");
        singlestringids.pop();
        let queryurl = VID_URL.to_string() + &singlestringids + "&access_token=";

        let q = query::<YoutubeDurationContentDetails>(t, client, &queryurl);

        for i in q {
            update_vid_time(&i, chunk);
        }
    }
}

/// Get new Videos and inserts them into the database
pub fn update_videos(t: &youtube_oauth::Token,
                     db: &Pool<ConnectionManager<SqliteConnection>>,
                     client: &reqwest::Client,
                     subs: &[Subscription]) {
    use schema::videos;
    use schema::config;

    let us = get_lastupdate_in_unixtime(db);
    let mut vids: Vec<NewVideo> = query_videos(t, client, subs, us);
    println!("Updating video running time");
    update_video_running_time(t, client, &mut vids);

    println!("New Youtube Videos length: {}", vids.len());
    let dbconn = db.get().expect("DB pool problem");
    insert_into(videos::table)
        .values(&vids)
        .execute(dbconn.deref())
        .expect("Insertion of Videos Failed");

    delete(config::table).execute(dbconn.deref()).expect("Deletion of old config failed");
    let nc = NewConfig { lastupdate: Utc::now().to_rfc3339() };
    insert_into(config::table).values(&nc).execute(dbconn.deref()).expect("Insertion of config failed");
}

/// Returns current videos in the database
pub fn get_videos(db: &Pool<ConnectionManager<SqliteConnection>>) -> Vec<Video> {
    use schema::videos::dsl::*;

    let dbconn = db.get().expect("DB pool problem");

    videos.order(published_at.desc())
        .load(dbconn.deref())
        .unwrap()
}

pub fn delete_video(db: &Pool<ConnectionManager<SqliteConnection>>, videoid: &str) {
    use schema::videos::dsl::*;
    let dbconn = db.get().expect("DB pool problem");
    delete(videos.filter(vid.like(videoid)))
        .execute(dbconn.deref());
}
