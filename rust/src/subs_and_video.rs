use std::fmt;
use std::error::Error;
use std::time::Duration;
use std::cmp::Ordering;
use schema::{subscriptions,videos};
use diesel::sqlite::Sqlite;
use diesel::types::{FromSql,VarChar,Text};
use chrono::{DateTime,UTC,FixedOffset};

#[derive(Debug,Serialize,Deserialize)]
pub struct MyDateTime(DateTime<UTC>);

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
    pub published_at: String,
    pub channelname: String,
    //pub duration: Duration,
    //pub subscription: Option<Subscription>
    pub url: String
}

impl Ord for Video {
    fn cmp(&self, other: &Video) -> Ordering {
        let s = DateTime::parse_from_rfc3339(&self.published_at).map(|s| s.timestamp()).unwrap_or(0);

        let o = DateTime::parse_from_rfc3339(&other.published_at).map(|s| s.timestamp()).unwrap_or(0);
        s.cmp(&o)
    }
}

#[derive(Insertable)]
#[table_name="videos"]
pub struct NewVideo {
    pub vid: String,
    pub title: String,
    pub thumbnail: String,
    pub published_at: String,
    //pub duration: String,
    pub channelname: String,
    pub url: String
}

pub fn from_youtube_datetime_to_string(s: &str) -> String {
    let dt = DateTime::parse_from_rfc3339(s).unwrap();
    dt.to_rfc3339()
}
