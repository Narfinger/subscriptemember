package main

import (
	"fmt"
	"io/ioutil"
	"golang.org/x/oauth2"
	"golang.org/x/oauth2/google"
	youtube "google.golang.org/api/youtube/v3"
)

func main() {
	fmt.Println("test")
	dat, err := ioutil.ReadFile("~/client_secret.json")
	fmt.Println(err)
	conf, err := google.ConfigFromJSON(dat)
	fmt.Println(err)

	url := conf.AuthCodeURL("state")
	fmt.Printf("Visit the URL for the auth dialog: %v", url)
	tok, err := conf.Exchange(oauth2.NoContext, "authorization-code")
	client := conf.Client(oauth2.NoContext, tok)

	service, err := youtube.New(client)
}
