use serde_json as json;
use oauth2;
use url::Url;
use chrono::{DateTime,Duration, Utc};
use std::fs::File;
use std::net::TcpListener;
use std::io::{BufRead, BufReader, Write};

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
            Duration::seconds(self.tk.expires_in.unwrap_or(0) as i64)).unwrap()
    }
}

fn authorize() -> Result<oauth2::Token,oauth2::TokenError> {
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
    config = config.add_scope("https://www.googleapis.com/auth/youtube");
    config = config.set_redirect_url("http://localhost:8080");
    let authorize_url = config.authorize_url();
    println!("Open this URL in your browser:\n{}\n", authorize_url.to_string());
    let mut code = String::new();

    // A very naive implementation of the redirect server.
    let listener = TcpListener::bind("127.0.0.1:8080").unwrap();
    for stream in listener.incoming() {
        match stream {
            Ok(mut stream) => {
                {
                    let mut reader = BufReader::new(&stream);

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
                let response = format!("HTTP/1.1 301 Moved Permanently\r\n Location: http://localhost:8000");
                stream.write_all(response.as_bytes()).unwrap();

                // The server will terminate itself after collecting the first code.
                break;
            }
            Err(_) => {},
        }
    };

    config.exchange_code(code)
}

fn refresh() -> Token {
    panic!("not yet implemented");
}

pub fn setup_oauth() -> Token {
    let f = File::open("tk.json");
    let tk = if let Ok(f) = f {
        json::from_reader(f).unwrap()    
    }
    else {
        let f = File::create("tk.json").unwrap();
        let tk = Token{ tk: authorize().unwrap(), created: Utc::now()};
        json::to_writer(f, &tk);
        tk
    };

    if tk.expired() {
        panic!("Token expired, we don't have a refresh.");
    }
    tk
}