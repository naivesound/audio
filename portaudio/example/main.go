package main

import (
	"log"
	"math"
	"time"

	"github.com/naivesound/audio/portaudio"
)

const (
	SampleRate = 44100.0
	Freq       = 440.0
)

func main() {
	portaudio.Initialize()
	defer portaudio.Terminate()

	phase := float64(0)

	stream, err := portaudio.OpenDefaultStream(0, 2, SampleRate, 0, func(out [][]float32) {
		numCh := len(out)
		for i := range out[0] {
			sample := float32(math.Sin(2 * math.Pi * phase))
			phase += Freq / SampleRate
			for ch := 0; ch < numCh; ch++ {
				out[ch][i] = sample
			}
		}
	})
	if err != nil {
		log.Fatal(err)
	}
	defer stream.Close()
	stream.Start()
	defer stream.Stop()

	time.Sleep(2 * time.Second)
}
