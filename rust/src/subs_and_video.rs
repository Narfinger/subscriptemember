use std::fmt;
use std::cmp::Ordering;
use std::str;
use std::str::FromStr;
use chrono::{NaiveDateTime, DateTime, TimeZone};
use chrono_tz::US::Pacific;
use diesel::sqlite::SqliteConnection;
use diesel::r2d2::Pool;
use diesel::r2d2::ConnectionManager;
use std::ops::Deref;
use nom::digit;
#[cfg(test)]
use nom::IResult;

use crate::schema::{subscriptions, videos, config};


#[derive(Debug,Deserialize,Clone)]
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

/// Construct a Giantbomb URL from the url field
pub fn make_gb_url(s: &str) -> String {
    s.to_string()
}

/// Construct a unix epoch from a given youtube timestamp string
pub fn from_youtube_datetime_to_timestamp(s: &str) -> i64 {
    let dt = DateTime::parse_from_rfc3339(s).unwrap();
    dt.timestamp()
}

/// Construct a unix epoch from a given giantbomb time string
pub fn from_giantbomb_datetime_to_timestamp(s: &str) -> i64 {
    let dt = NaiveDateTime::parse_from_str(s, "%Y-%m-%d %H:%M:%S")
        .unwrap_or_else(|e| panic!("Error: {}", e));
    let dttz = Pacific.from_local_datetime(&dt).unwrap();
    dttz.timestamp()
}

named!(number<u64>, map_res!(map_res!(ws!(digit),str::from_utf8),FromStr::from_str) );
named!(youtube_duration_hour    <&[u8],u64>, do_parse!(v: number >> tag!("H") >> (v) ));
named!(youtube_duration_minutes <&[u8],u64>, do_parse!(v: number >> tag!("M") >> (v) ));
named!(youtube_duration_seconds <&[u8],u64>, do_parse!(v: number >> tag!("S") >> (v) ));

/// Parses a youtube duration string into unix epoch
named!(pub youtube_duration <&[u8],u64>, do_parse!(
    tag!("P") >>
    tag!("T") >>
        hours: opt!(youtube_duration_hour) >>
        minutes: opt!(youtube_duration_minutes) >>
        seconds: opt!(complete!(youtube_duration_seconds)) >>
        (hours.unwrap_or(0)*60*60 + minutes.unwrap_or(0) *60 + seconds.unwrap_or(0))));

#[test]
fn youtube_duration_parse_test() {
    //simple tests
    assert_eq!(youtube_duration_hour   (&b"23H"[..]), IResult::Done(&b""[..], 23));
    assert_eq!(youtube_duration_minutes(&b"23M"[..]), IResult::Done(&b""[..], 23));
    assert_eq!(youtube_duration_seconds(&b"23S"[..]), IResult::Done(&b""[..], 23));

    assert_eq!(youtube_duration(&b"PT23H4M1S"[..]), IResult::Done(&b""[..], 83041));
    assert_eq!(youtube_duration(&b"PT15M23S"[..]), IResult::Done(&b""[..], 923));

    assert_eq!(youtube_duration(&b"PT1S"[..]), IResult::Done(&b""[..], 1));
    assert_eq!(youtube_duration(&b"PT14S"[..]), IResult::Done(&b""[..], 14));
    assert_eq!(youtube_duration(&b"PT4M1S"[..]), IResult::Done(&b""[..], 241));
    assert_eq!(youtube_duration(&b"PT17M5S"[..]), IResult::Done(&b""[..], 1025));
    assert_eq!(youtube_duration(&b"PT23M14S"[..]), IResult::Done(&b""[..], 1394));
    assert_eq!(youtube_duration(&b"PT1H33M14S"[..]), IResult::Done(&b""[..], 5594));
    assert_eq!(youtube_duration(&b"PT45M"[..]), IResult::Done(&b""[..], 2700));
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

/// Get the lastupdate from the database in unix epoch
pub fn get_lastupdate_in_utctime(db: &Pool<ConnectionManager<SqliteConnection>>) -> Option<DateTime<chrono::Utc>> {
    use crate::schema::config::dsl::*;
    use diesel::RunQueryDsl;

    let dbconn = db.get().expect("DB pool problem");
    let val = config.load::<Config>(dbconn.deref()).unwrap();

    if val.is_empty() {
        None
    } else {
        DateTime::parse_from_rfc3339(&val[0].lastupdate).ok().map(|s| s.with_timezone(&chrono::Utc))
    }
}
