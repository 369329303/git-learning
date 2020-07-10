package main

import (
	"log"
	"net/http"
	"time"

	"github.com/myzhan/boomer"
)

func pkTest() {
	start := time.Now()

	// resp, err := http.Get("http://192.168.0.225:8124/realtime/pk?code=600817.SH")
	resp, err := http.Get("http://192.168.0.203:9188/realtime/pk?code=600817.SH")
	// resp, err := http.Get("http://47.102.202.27:8899/realtime/pk?code=600817.SH")
	if err != nil {
		log.Println(err)
		return
	}
	defer resp.Body.Close()

	elapsed := time.Since(start)

	if resp.Status == "200 OK" {
		boomer.RecordSuccess("http", "pkTest", elapsed.Nanoseconds()/int64(time.Millisecond), int64(10))
	} else {
		boomer.RecordFailure("http", "pkTest", elapsed.Nanoseconds()/int64(time.Millisecond), "pkTest failed.")
	}
}

func main() {
	task1 := &boomer.Task{
		Name: "pkTest",
		// The weight is used to distribute goroutines over multiple tasks.
		Weight: 10,
		Fn:     pkTest,
	}
	boomer.Run(task1)
}
