table! {
    config (id) {
        id -> Integer,
        lastupdate -> Text,
    }
}

table! {
    subscriptions (id) {
        id -> Integer,
        channelid -> Text,
        channelname -> Text,
        uploadplaylist -> Text,
        thumbnail -> Text,
        description -> Text,
    }
}

table! {
    videos (id) {
        id -> Integer,
        vid -> Text,
        title -> Text,
        thumbnail -> Text,
        published_at -> BigInt,
        channelname -> Text,
        duration -> BigInt,
        url -> Text,
    }
}
