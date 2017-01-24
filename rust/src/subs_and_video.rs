use std::fmt;
use std::error::Error;
use std::time::Duration;
use schema::{subscriptions,videos};
use diesel::sqlite::Sqlite;
use diesel::types::{FromSql,VarChar,Text};
use chrono::{DateTime,NaiveDateTime,UTC,FixedOffset};

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

#[derive(Debug,Serialize,Deserialize,Queryable)]
pub struct Video {
    pub id: i32,
    pub vid: String,
    pub title: String,
    pub thumbnail: String,
    pub published_at: String,
    //pub duration: Duration,
    //pub subscription: Option<Subscription>
    pub url: String
}

#[derive(Insertable)]
#[table_name="videos"]
pub struct NewVideo {
    pub vid: String,
    pub title: String,
    pub thumbnail: String,
    pub published_at: String,
    //pub duration: String,
    //pub subscription: Option<Subscription>
    pub url: String
}

// impl FromSql<Text, Sqlite> for MyDateTime {
//     fn from_sql(bytes: Option<&SqliteValue>) -> Result<Self, Box<Error>> {
//         let text = String::from_utf8(bytes).ok().and_then(|s| DateTime::parse_from_rfc3339(s.as_str()).ok());
        
//         match text {
//             Some(s) => {
//                 let ns = s.with_timezone(&UTC);
//                 Ok(MyDateTime(ns))
//             },
//             None => Err("something wrong".into())
//         }
//     }
// }
