use serde_json as json;
use oauth2;
use url::Url;
use chrono::{DateTime,Duration, Utc};
use reqwest;
use std::fs::File;
use std::net::TcpListener;
use std::io::{BufRead, BufReader, Write};
use failure::Error;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Token {
    pub tk: oauth2::Token,
    created: DateTime<Utc>,
}

pub trait Expireing {
    fn expired(&self) -> bool;
}

impl Expireing for Token {
    fn expired(&self) -> bool {
        Utc::now() >= self.created.checked_add_signed(
            //we assume seconds here because I do not know what it is in
            Duration::seconds(i64::from(self.tk.expires_in.unwrap_or(0)))).unwrap()
    }
}

#[derive(Deserialize)]
struct Installed {
    client_id: String,
    auth_uri: String,
    token_uri: String,
    client_secret: String,
}

fn get_client_secrets() -> Installed {
    #[derive(Deserialize)]
    struct ClientSecret {
        installed: Installed,
    };
    let f = File::open("client_secret.json").expect("Did not find client_secret.json");
    json::from_reader::<File, ClientSecret>(f).unwrap().installed
}

fn authorize() -> Result<oauth2::Token,oauth2::TokenError> {
    let secret = get_client_secrets();
    let mut config = oauth2::Config::new(secret.client_id, secret.client_secret, secret.auth_uri, secret.token_uri);    
    config = config.add_scope("https://www.googleapis.com/auth/youtube");
    config = config.set_redirect_url("http://localhost:8080");
    let authorize_url = config.authorize_url();
    println!("Open this URL in your browser:\n{}\n", authorize_url.to_string());
    let mut code = String::new();

    // A very naive implementation of the redirect server.
    let listener = TcpListener::bind("127.0.0.1:8080").unwrap();
    for stream in listener.incoming() {
        if let Ok(mut s) = stream {
            {
                let mut reader = BufReader::new(&s);

                let mut request_line = String::new();
                reader.read_line(&mut request_line).unwrap();

                let redirect_url = request_line.split_whitespace().nth(1).unwrap();
                let url = Url::parse(&("http://localhost".to_string() + redirect_url)).unwrap();

                let code_pair = url.query_pairs().find(|pair| {
                    let &(ref key, _) = pair;
                    key == "code"
                }).unwrap();

                let (_, value) = code_pair;
                code = value.into_owned();
            }
            let response = "HTTP/1.1 301 Moved Permanently\r\n Location: http://localhost:8000";
            s.write_all(response.as_bytes()).unwrap();

            // The server will terminate itself after collecting the first code.
            break;
            }
        };
    config.exchange_code(code)
}

fn refresh(oldtoken: &Token) -> Result<Token, Error> {
    let secret = get_client_secrets();
    let params = [("refresh_token", oldtoken.tk.refresh_token.clone().unwrap()), 
                    ("client_id", secret.client_id),
                    ("client_secret", secret.client_secret),
                    ("grant_type", String::from("refresh_token"))];
    let client = reqwest::Client::new()?;
    let res = client.post("https://www.googleapis.com/oauth2/v4/token")
        .form(&params)
        .send()?;
    #[derive(Deserialize)]
    struct Response {
        access_token: String,
        expires_in: u32,
    }
    let new_response: Response = json::from_reader(res)?;
    
    //changing to new token, take the old one as a copy
    let mut newtk = oldtoken.tk.clone();
    newtk.access_token = new_response.access_token;
    newtk.expires_in = Some(new_response.expires_in);
    Ok(Token { created: Utc::now(), tk: newtk })
}

pub fn setup_oauth() -> Result<Token,Error> {
    let f = File::open(prefs_base_dir() + "tk.json");
    let tk = if let Ok(f) = f {
        json::from_reader(f)
    }
    else {
        let f = File::create(prefs_base_dir() + "tk.json")?;
        let tk = Token{ tk: authorize().unwrap(), created: Utc::now()};
        json::to_writer(f, &tk).expect("Could not write token");
        Ok(tk)
    }?;
    if tk.expired() {
        refresh(&tk)
    } else {
        Ok(tk)
    }
}