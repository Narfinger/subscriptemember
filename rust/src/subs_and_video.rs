use std::fmt;
use std::cmp::Ordering;
use std::str;
use std::str::FromStr;
use schema::{subscriptions, videos, config};
use chrono::{NaiveDateTime,DateTime,TimeZone};
use chrono_tz::US::Pacific;
use std::sync::Mutex;
use diesel::sqlite::SqliteConnection;
use diesel::LoadDsl;
use nom::{IResult,digit};

#[derive(Debug,Deserialize)]
pub struct GBKey {
    pub key: String,
}

#[derive(Debug,Serialize,Deserialize,Queryable)]
pub struct Subscription {
    pub sid: i32,
    pub channelid: String,
    pub channelname: String,
    pub uploadplaylist: String,
    pub thumbnail: String,
    pub description: String,
}

impl fmt::Display for Subscription {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "(sid: {}, chnn: {}, uplpl: {}, thmb: {})",
               self.sid,
               self.channelname,
               self.uploadplaylist,
               self.thumbnail)
    }
}

#[derive(Insertable)]
#[table_name="subscriptions"]
pub struct NewSubscription {
    pub channelid: String,
    pub channelname: String,
    pub uploadplaylist: String,
    pub thumbnail: String,
    pub description: String,
}

#[derive(Debug,Serialize,Deserialize,Queryable,Eq,PartialEq,PartialOrd)]
pub struct Video {
    pub id: i32,
    pub vid: String,
    pub title: String,
    pub thumbnail: String,
    pub published_at: i64, //unix timestamp
    pub channelname: String,
    pub duration: i64, //duration in seconds
    pub url: String,
}

impl Ord for Video {
    fn cmp(&self, other: &Video) -> Ordering {
        self.published_at.cmp(&other.published_at)
    }
}

#[derive(Insertable)]
#[table_name="videos"]
pub struct NewVideo {
    pub vid: String,
    pub title: String,
    pub thumbnail: String,
    pub published_at: i64,
    pub channelname: String,
    pub duration: i64,
    pub url: String,
}

pub fn make_youtube_url(s: &str) -> String {
    "https://www.youtube.com/watch?v=".to_string() + s
}

pub fn make_gb_url(s: &str) -> String {
    s.to_string()
}

pub fn from_youtube_datetime_to_timestamp(s: &str) -> i64 {
    let dt = DateTime::parse_from_rfc3339(s).unwrap();
    dt.timestamp()
}

pub fn from_giantbomb_datetime_to_timestamp(s: &str) -> i64 {
    //let dt = Pacific::parse_from_str(s, "%Y-%m-%d %H:%M:%S").unwrap_or_else(|e| panic!("Error: {}", e));
    let dt = NaiveDateTime::parse_from_str(s, "%Y-%m-%d %H:%M:%S").unwrap_or_else(|e| panic!("Error: {}", e));
    let dttz = Pacific.from_local_datetime(&dt).unwrap();
    dttz.timestamp()
}

named!(number<u64>, map_res!(map_res!(ws!(digit),str::from_utf8),FromStr::from_str) );
named!(youtube_duration_hour    <&[u8],u64>, chain!(v: number ~ tag!("H"), || {v} ) );
named!(youtube_duration_minutes <&[u8],u64>, chain!(v: number ~ tag!("M"), || {v} ) );
named!(youtube_duration_seconds <&[u8],u64>, chain!(v: number ~ tag!("S"), || {v} ) );

named!(pub youtube_duration <&[u8],u64>, chain!(
    tag!("P") ~
    hours: opt!(youtube_duration_hour) ~
        minutes: opt!(youtube_duration_minutes) ~
        seconds: youtube_duration_seconds
        ,
    || {
        hours.unwrap_or(0)*60*60 + minutes.unwrap_or(0) *60 + seconds
    }
));

#[test]
fn youtube_duration_parse_test() {
    //simple tests    
    assert_eq!(youtube_duration_hour   (&b"23H"[..]), IResult::Done(&b""[..], 23));
    assert_eq!(youtube_duration_minutes(&b"23M"[..]), IResult::Done(&b""[..], 23));
    assert_eq!(youtube_duration_seconds(&b"23S"[..]), IResult::Done(&b""[..], 23));
    
    assert_eq!(youtube_duration(&b"P23H4M1S"[..]), IResult::Done(&b""[..], 83041));
    
    assert_eq!(youtube_duration(&b"P1S"[..]), IResult::Done(&b""[..], 1));
    assert_eq!(youtube_duration(&b"P14S"[..]), IResult::Done(&b""[..], 14));
    assert_eq!(youtube_duration(&b"P4M1S"[..]), IResult::Done(&b""[..], 241));
    assert_eq!(youtube_duration(&b"P17M5S"[..]), IResult::Done(&b""[..], 1025));
    assert_eq!(youtube_duration(&b"P23M14S"[..]), IResult::Done(&b""[..], 1394));
    assert_eq!(youtube_duration(&b"P1H33M14S"[..]), IResult::Done(&b""[..], 5594));
}

#[derive(Debug,Queryable)]
pub struct Config {
    pub id: i32,
    pub lastupdate: String,
}

#[derive(Insertable)]
#[table_name="config"]
pub struct NewConfig {
    pub lastupdate: String,
}

pub fn get_lastupdate_in_unixtime(db: &Mutex<SqliteConnection>) -> i64 {
    use schema::config::dsl::*;

    let dbconn: &SqliteConnection = &db.lock().unwrap();
    let val = config.load::<Config>(dbconn).unwrap();

    if val.is_empty() {
        0
    } else {
        DateTime::parse_from_rfc3339(&val[0].lastupdate).map(|s| s.timestamp()).unwrap_or(0)
    }
}
