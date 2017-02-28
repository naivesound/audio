package main

import (
	"log"
	"math"
	"time"

	rtaudio "github.com/naivesound/audio/rtaudio"
)

const (
	SampleRate = 44100.0
	Freq       = 440.0
)

var phase = float64(0)

func cb(out, in rtaudio.Buffer, dur time.Duration, status rtaudio.StreamStatus) int {
	samples := out.Float32()
	for i := 0; i < len(samples)/2; i++ {
		sample := float32(math.Sin(2 * math.Pi * phase))
		phase += Freq / SampleRate

		samples[i*2] = sample
		samples[i*2+1] = sample
	}
	return 0
}

func main() {
	log.Println(rtaudio.Version())
	log.Println(rtaudio.CompiledAPI())

	a, err := rtaudio.Create(rtaudio.APIUnspecified)
	if err != nil {
		log.Fatal(err)
	}
	defer a.Destroy()

	log.Println(a.CurrentAPI())

	devices, err := a.Devices()
	if err != nil {
		log.Fatal(err)
	}

	for _, device := range devices {
		log.Println(device)
	}

	params := rtaudio.StreamParams{
		DeviceID:     4, //a.DefaultOutputDevice(),
		NumChannels:  2,
		FirstChannel: 0,
	}
	options := rtaudio.StreamOptions{
	//
	}
	err = a.Open(&params, nil, rtaudio.FormatFloat32, SampleRate, 2048, cb, &options)
	if err != nil {
		log.Fatal(err)
	}
	defer a.Close()

	a.Start()
	defer a.Stop()

	time.Sleep(3 * time.Second)
}
