package main

import (
	"fmt"
	"bufio"
	"os"
	
	"log"
	"net/http"

	"lib"
	"golang.org/x/oauth2"
	"golang.org/x/oauth2/google"
	youtube "google.golang.org/api/youtube/v3"
)

func firstOAuthSetup() *http.Client {
	fmt.Println("test")

	url := conf.AuthCodeURL("state")
	fmt.Printf("Visit the URL for the auth dialog:\n %v\n", url)

	reader := bufio.NewReader(os.Stdin)
	fmt.Print("Enter code: ")
	text, _ := reader.ReadString('\n')
	
	
	tok, err := conf.Exchange(oauth2.NoContext, text)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("this is a test")
	return conf.Client(oauth2.NoContext, tok)
	
}

func main() {
	client := firstOAuthSetup()
	
	service, err := youtube.New(client)
	if err != nil {
		log.Fatal(err)
	}
	subslistcall := youtube.NewSubscriptionsService(service).List("contentDetails")
	subs, err := subslistcall.Do()
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(subs)
	for _,elem := range subs.Items {
		fmt.Println(elem.Etag)
	}
	
}
