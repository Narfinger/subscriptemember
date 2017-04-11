use std::collections::VecDeque;
use oauth2;
use serde;
use reqwest;

#[derive(Debug,Deserialize)]
pub struct YoutubePageInfo {
    #[serde(rename="totalResults")]
    pub total_results: i32,

    #[serde(rename="resultsPerPage")]
    pub results_per_page: i32,
}

#[derive(Debug,Deserialize)]
pub struct YoutubeResult<T> {
    pub items: VecDeque<YoutubeItem<T>>,
    #[serde(rename="pageInfo")]
    pub page_info: YoutubePageInfo,
    #[serde(rename="nextPageToken")]
    pub next_page_token: Option<String>,
}

#[derive(Debug,Deserialize)]
pub struct YoutubeItem<T> {
    #[serde(rename="id")]
    pub iid: String,
    pub snippet: Option<T>,
    #[serde(rename="contentDetails")]
    pub content_details: Option<T>,
}

#[derive(Debug,Deserialize)]
pub struct YoutubeRelatedPlaylistsContentDetails {
    #[serde(rename="relatedPlaylists")]
    pub related_playlists: Option<YoutubeRelatedPlaylists>,
}


#[derive(Debug,Deserialize)]
pub struct YoutubeSnippet {
    #[serde(rename="publishedAt")]
    pub published_at: String,
    //pub channel_id: String,
    pub title: String,
    pub description: String,
    pub thumbnails: YoutubeThumbnails,
    #[serde(rename="resourceId")]
    pub resource: YoutubeResource,
}


#[derive(Debug,Deserialize)]
pub struct YoutubeRelatedPlaylists {
    pub uploads: String,
}

#[derive(Debug,Deserialize)]
pub struct YoutubeThumbnailDetail {
    #[serde(rename="url")]
    pub thmburl: String,
}

#[derive(Debug,Deserialize)]
pub struct YoutubeThumbnails {
    pub default: YoutubeThumbnailDetail,
    pub medium: YoutubeThumbnailDetail,
    pub high: YoutubeThumbnailDetail,
}

#[derive(Debug,Deserialize)]
pub struct YoutubeResource {
    pub kind: String,
    #[serde(rename="channelId")]
    pub channel_id: Option<String>,
    #[serde(rename="videoId")]
    pub video_id: Option<String>,
}

#[derive(Debug,Deserialize)]
pub struct YoutubeSubscription {
    #[serde(rename="title")]
    pub subscription_title: String,
    #[serde(rename="description")]
    pub sdescription: String,
    #[serde(rename="channelId")]
    pub channel_id: String,
    pub thumbnails: YoutubeThumbnails,
    #[serde(rename="resourceId")]
    pub resource_id: YoutubeResource,
}

#[derive(Debug,Deserialize)]
pub struct YoutubeDurationContentDetails {
    pub duration: String,
    pub dimension: String,
    pub definition: String,
}

fn query_simple_page<T>(t: &oauth2::Token, url: &str, nextpage: Option<String>, client: &reqwest::Client) -> YoutubeResult<T>
    where T: serde::Deserialize
{
    let mut q = String::from(url);
    q.push_str(t.access_token.as_str());
    if let Some(nextpagetk) = nextpage {
        q.push_str("&pageToken=");
        q.push_str(nextpagetk.as_str());
    }
    println!("Query: {}", q);
    client.get(q.as_str())
        .send()
        .and_then(|mut r| r.json::<YoutubeResult<T>>())
        .unwrap_or_else(|e| panic!("error in json parsing: {}", e))
}

pub struct Query<T> {
    initialised: bool,
    storage: VecDeque<YoutubeItem<T>>,
    url: String,
    t: oauth2::Token,
    next_page: Option<String>,
    client: reqwest::Client,
}

impl<'a, T> Iterator for Query<T>
    where T: serde::Deserialize
{
    type Item = YoutubeItem<T>;
    fn next(&mut self) -> Option<YoutubeItem<T>> {
        if !self.initialised {
            self.initialised = true;
            let res = query_simple_page(&self.t, &self.url, None, &self.client);
            self.storage = res.items;
            self.next_page = res.next_page_token;
        }

        if self.storage.is_empty() {
            if self.next_page.is_some() {
                let res = query_simple_page(&self.t, &self.url, self.next_page.clone(), &self.client);
                self.storage = res.items;
                self.next_page = res.next_page_token;
            } else {
                return None;
            }
        }

        self.storage.pop_front()
    }
}


pub fn query<T>(t: &oauth2::Token, client: &reqwest::Client, url: &str) -> Query<T>
    where T: serde::Deserialize
{
    Query::<T> {
        initialised: false,
        storage: VecDeque::with_capacity(50),
        url: url.to_string(),
        t: t.clone(),
        next_page: None,
        client: (*client).clone(),
    }
}
