use oauth2;

fn authorize() -> oauth2::Token {
    let tk_storage = File::open("tk");
    
    #[derive(Deserialize)]
    struct Installed {
        client_id: String,
        auth_uri: String,
        token_uri: String,
        client_secret: String,
    };
    #[derive(Deserialize)]
    struct ClientSecret {
        installed: Installed,
    };
    let f = File::open("client_secret.json").expect("Did not find client_secret.json");
    let secret = json::from_reader::<File, ClientSecret>(f).unwrap().installed;
    let mut config = oauth2::Config::new(secret.client_id, secret.client_secret, secret.auth_uri, secret.token_uri);    
    config.add_scope("https://www.googleapis.com/auth/youtube");
    
    
}

fn refresh() -> oauth2::Token {

}

fn setup_oauth2() -> oauth2::Token {

}