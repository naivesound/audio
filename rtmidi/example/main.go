package main

import (
	"log"

	"github.com/naivesound/audio/rtmidi"
)

func main() {
	log.Println(rtmidi.CompiledAPI())

	in, err := rtmidi.NewMIDIInDefault()
	if err != nil {
		log.Fatal(err)
	}
	defer in.Close()

	n, err := in.PortCount()
	if err != nil {
		log.Fatal(err)
	}
	for i := 0; i < n; i++ {
		name, err := in.PortName(i)
		if err == nil {
			log.Println(name)
			if err := in.OpenPort(i, "RtMidi"); err != nil {
				log.Println(err)
			}
		} else {
			log.Println(err)
		}
	}

	//in.SetCallback(func(m rtmidi.MIDIIn, msg []byte, t float64) {
	//log.Println(msg, t)
	//})
	//c := make(chan struct{})
	//<-c

	for {
		m, t, err := in.Message()
		if len(m) > 0 {
			log.Println(m, t, err)
		}
	}
}
