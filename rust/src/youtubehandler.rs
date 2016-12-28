mod youtubehandler {
    use oauth2;

    struct YoutubeItems<T> {
        iid : String,
        snippet : Option<T>,
        contentDetails : Option<T>
    }

    struct YoutubeSubscription {
        subscriptiontitle : String,
        description : String,
        // resourceId : YoutubeResource,
        // thumbnails : YoutubeThumbnails,

    }

    struct Subscription {
        sid : String,
        channelname : String,
        uploadPlaylist : String,
        thumbnail : String,
    }

    
    fn get_subscriptions_for_me(t : &oauth2::Token) -> YoutubeItems<YoutubeSubscription> {
        let ys = YoutubeSubscription { subscriptiontitle : String::from("title test"), description : String::from("desc test")};
        let yi = YoutubeItems { iid : String::from("test iid"), snippet : None, contentDetails : Some(ys), };
        return yi;
    }

    fn construct_subscription(s : YoutubeItems<YoutubeSubscription>) -> Subscription {
        return Subscription { sid : String::from("test sid"), channelname : String::from("testchannelname"), uploadPlaylist : String::from("uploadplaylist test"), thumbnail : String::from("testthumbnail")};
    }
    
    

//     getSubscriptionsForMe :: C.Manager -> AccessToken -> IO [YoutubeItems YoutubeSubscription]
// getSubscriptionsForMe mgr token =
//   let qurl = constructQueryString "/subscriptions?&maxResults=50&part=snippet&mine=True" in
//   authGetJSONPages mgr token qurl :: (IO [YoutubeItems YoutubeSubscription])


}
