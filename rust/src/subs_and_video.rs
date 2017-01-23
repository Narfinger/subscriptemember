use std::fmt;
use std::error::Error;
use std::time::Duration;
use schema::{subscriptions,videos};
use diesel::types::FromSql;
use chrono::{DateTime,NaiveDateTime,UTC};

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

#[derive(Debug,Deserialize,Queryable)]
pub struct Video {
    pub id: i32,
    pub vid: String,
    pub title: String,
    pub thumbnail: String,
    pub published_at: DateTime<UTC>,
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

impl FromSql<DateTime<UTC>, T> for NewVideo {
    fn from_sql(bytes: Option<&[u8]>) -> Result<Self, Box<Error>> {
        if bytes.is_some() {
            let s = THIS DOES NOT YET WORK
        }

        match bytes {
            Some(s) => Ok(NaiveDateTime::from_timestamp(s).with_timezone(UTC)), 
            None => Err("Invalid??".into())
        }
    }
}

impl ToSql<DateTime<UTC>, T> for NewVideo {
        fn to_sql<W: Write>(&self, out: &mut W) -> Result<IsNull, Box<Error>> {
            let bytes: &'static [u8] = *self.published_at.naive_utc

                THIS DOES NOT YET WORK
            out.write_all(bytes)
                .map(|_| IsNull::No)
                .map_err(|e| e.into())
        }
    }
