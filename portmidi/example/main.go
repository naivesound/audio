package main

import (
	"log"

	"github.com/naivesound/audio/portmidi"
)

func main() {
	in, err := portmidi.NewInputStream(portmidi.DefaultInputDeviceID(), 1024)
	if err != nil {
		log.Fatal(err)
	}

	defer in.Close()

	for e := range in.Listen() {
		log.Println(e)
	}
}
