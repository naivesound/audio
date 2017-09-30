package rtaudio

/*

#cgo LDFLAGS: -lstdc++

#cgo linux CXXFLAGS: -D__LINUX_ALSA__
#cgo linux LDFLAGS: -lm -lasound -pthread

#cgo windows CXXFLAGS: -D__WINDOWS_WASAPI__
#cgo windows LDFLAGS: -lm -luuid -lksuser -lwinmm -lole32

#cgo darwin CXXFLAGS: -D__MACOSX_CORE__
#cgo darwin LDFLAGS: -framework CoreAudio -framework CoreFoundation

#include "rtaudio_c.h"

extern int goCallback(void *out, void *in, unsigned int nFrames,
			    double stream_time, rtaudio_stream_status_t status,
			    void *userdata);

*/
import "C"
import (
	"errors"
	"sync"
	"time"
	"unsafe"
)

type API C.rtaudio_api_t

const (
	APIUnspecified   API = C.RTAUDIO_API_UNSPECIFIED
	APILinuxALSA         = C.RTAUDIO_API_LINUX_ALSA
	APILinuxPulse        = C.RTAUDIO_API_LINUX_PULSE
	APILinuxOSS          = C.RTAUDIO_API_LINUX_OSS
	APIUnixJack          = C.RTAUDIO_API_UNIX_JACK
	APIMacOSXCore        = C.RTAUDIO_API_MACOSX_CORE
	APIWindowsWASAPI     = C.RTAUDIO_API_WINDOWS_WASAPI
	APIWindowsASIO       = C.RTAUDIO_API_WINDOWS_ASIO
	APIWindowsDS         = C.RTAUDIO_API_WINDOWS_DS
	APIDummy             = C.RTAUDIO_API_DUMMY
)

func (api API) String() string {
	switch api {
	case APIUnspecified:
		return "unspecified"
	case APILinuxALSA:
		return "alsa"
	case APILinuxPulse:
		return "pulse"
	case APILinuxOSS:
		return "oss"
	case APIUnixJack:
		return "jack"
	case APIMacOSXCore:
		return "coreaudio"
	case APIWindowsWASAPI:
		return "wasapi"
	case APIWindowsASIO:
		return "asio"
	case APIWindowsDS:
		return "directsound"
	case APIDummy:
		return "dummy"
	}
	return "?"
}

type StreamStatus C.rtaudio_stream_status_t

const (
	StatusInputOverflow   StreamStatus = C.RTAUDIO_STATUS_INPUT_OVERFLOW
	StatusOutputUnderflow StreamStatus = C.RTAUDIO_STATUS_OUTPUT_UNDERFLOW
)

func Version() string {
	return C.GoString(C.rtaudio_version())
}

func CompiledAPI() (apis []API) {
	capis := (*[1 << 30]C.rtaudio_api_t)(unsafe.Pointer(C.rtaudio_compiled_api()))
	for i := 0; ; i++ {
		api := capis[i]
		if api == C.RTAUDIO_API_UNSPECIFIED {
			break
		}
		apis = append(apis, API(api))
	}
	return apis
}

type DeviceInfo struct {
	Name              string
	Probed            bool
	NumOutputChannels int
	NumInputChannels  int
	NumDuplexChannels int
	IsDefaultOutput   bool
	IsDefaultInput    bool

	//rtaudio_format_t native_formats;

	PreferredSampleRate uint
	SampleRates         []int
}

type StreamParams struct {
	DeviceID     uint
	NumChannels  uint
	FirstChannel uint
}

type StreamFlags C.rtaudio_stream_flags_t

const (
	FlagsNoninterleages   = C.RTAUDIO_FLAGS_NONINTERLEAVED
	FlagsMinimizeLatency  = C.RTAUDIO_FLAGS_MINIMIZE_LATENCY
	FlagsHogDevice        = C.RTAUDIO_FLAGS_HOG_DEVICE
	FlagsScheduleRealtime = C.RTAUDIO_FLAGS_SCHEDULE_REALTIME
	FlagsAlsaUseDefault   = C.RTAUDIO_FLAGS_ALSA_USE_DEFAULT
)

type StreamOptions struct {
	Flags      StreamFlags
	NumBuffers uint
	Priotity   int
	Name       string
}

type RtAudio interface {
	Destroy()
	CurrentAPI() API
	Devices() ([]DeviceInfo, error)
	DefaultOutputDevice() int
	DefaultInputDevice() int

	Open(out, in *StreamParams, format Format, sampleRate uint, frames uint, cb Callback, opts *StreamOptions) error
	Close()
	Start() error
	Stop() error
	Abort() error

	IsOpen() bool
	IsRunning() bool

	Latency() (int, error)
	SampleRate() (uint, error)
	Time() (time.Duration, error)
	SetTime(time.Duration) error

	ShowWarnings(bool)
}

type rtaudio struct {
	audio          C.rtaudio_t
	cb             Callback
	inputChannels  int
	outputChannels int
	format         Format
}

var _ RtAudio = &rtaudio{}

func Create(api API) (*rtaudio, error) {
	audio := C.rtaudio_create(C.rtaudio_api_t(api))
	if C.rtaudio_error(audio) != nil {
		return nil, errors.New(C.GoString(C.rtaudio_error(audio)))
	}
	return &rtaudio{audio: audio}, nil
}

func (audio *rtaudio) Destroy() {
	C.rtaudio_destroy(audio.audio)
}

func (audio *rtaudio) CurrentAPI() API {
	return API(C.rtaudio_current_api(audio.audio))
}

func (audio *rtaudio) DefaultInputDevice() int {
	return int(C.rtaudio_get_default_input_device(audio.audio))
}

func (audio *rtaudio) DefaultOutputDevice() int {
	return int(C.rtaudio_get_default_output_device(audio.audio))
}

func (audio *rtaudio) Devices() ([]DeviceInfo, error) {
	n := C.rtaudio_device_count(audio.audio)
	devices := []DeviceInfo{}
	for i := C.int(0); i < n; i++ {
		cinfo := C.rtaudio_get_device_info(audio.audio, i)
		if C.rtaudio_error(audio.audio) != nil {
			return nil, errors.New(C.GoString(C.rtaudio_error(audio.audio)))
		}
		sr := []int{}
		for _, r := range cinfo.sample_rates {
			if r == 0 {
				break
			}
			sr = append(sr, int(r))
		}
		devices = append(devices, DeviceInfo{
			Name:                C.GoString(&cinfo.name[0]),
			Probed:              cinfo.probed != 0,
			NumInputChannels:    int(cinfo.input_channels),
			NumOutputChannels:   int(cinfo.output_channels),
			NumDuplexChannels:   int(cinfo.duplex_channels),
			IsDefaultOutput:     cinfo.is_default_output != 0,
			IsDefaultInput:      cinfo.is_default_input != 0,
			PreferredSampleRate: uint(cinfo.preferred_sample_rate),
			SampleRates:         sr,
		})
		// TODO: formats
	}
	return devices, nil
}

type Format int

const (
	FormatInt8    Format = C.RTAUDIO_FORMAT_SINT8
	FormatInt16          = C.RTAUDIO_FORMAT_SINT16
	FormatInt24          = C.RTAUDIO_FORMAT_SINT24
	FormatInt32          = C.RTAUDIO_FORMAT_SINT32
	FormatFloat32        = C.RTAUDIO_FORMAT_FLOAT32
	FormatFloat64        = C.RTAUDIO_FORMAT_FLOAT64
)

type Buffer interface {
	Len() int
	Int8() []int8
	Int16() []int16
	Int24() []Int24
	Int32() []int32
	Float32() []float32
	Float64() []float64
}

type Int24 [3]byte

func (i *Int24) Set(n int32) {
	(*i)[0], (*i)[1], (*i)[2] = byte(n&0xff), byte((n&0xff00)>>8), byte((n&0xff0000)>>16)
}

func (i Int24) Get() int32 {
	n := int32(i[0]) | int32(i[1])<<8 | int32(i[2])<<16
	if n&0x800000 != 0 {
		n |= ^0xffffff
	}
	return n
}

type buffer struct {
	format      Format
	length      int
	numChannels int
	ptr         unsafe.Pointer
}

func (b *buffer) Len() int {
	if b.ptr == nil {
		return 0
	}
	return b.length
}

func (b *buffer) Int8() []int8 {
	if b.format != FormatInt8 {
		return nil
	}
	if b.ptr == nil {
		return nil
	}
	return (*[1 << 30]int8)(b.ptr)[:b.length*b.numChannels : b.length*b.numChannels]
}

func (b *buffer) Int16() []int16 {
	if b.format != FormatInt16 {
		return nil
	}
	if b.ptr == nil {
		return nil
	}
	return (*[1 << 30]int16)(b.ptr)[:b.length*b.numChannels : b.length*b.numChannels]
}

func (b *buffer) Int24() []Int24 {
	if b.format != FormatInt24 {
		return nil
	}
	if b.ptr == nil {
		return nil
	}
	return (*[1 << 30]Int24)(b.ptr)[:b.length*b.numChannels : b.length*b.numChannels]
}

func (b *buffer) Int32() []int32 {
	if b.format != FormatInt32 {
		return nil
	}
	if b.ptr == nil {
		return nil
	}
	return (*[1 << 30]int32)(b.ptr)[:b.length*b.numChannels : b.length*b.numChannels]
}

func (b *buffer) Float32() []float32 {
	if b.format != FormatFloat32 {
		return nil
	}
	if b.ptr == nil {
		return nil
	}
	return (*[1 << 30]float32)(b.ptr)[:b.length*b.numChannels : b.length*b.numChannels]
}

func (b *buffer) Float64() []float64 {
	if b.format != FormatFloat64 {
		return nil
	}
	if b.ptr == nil {
		return nil
	}
	return (*[1 << 30]float64)(b.ptr)[:b.length*b.numChannels : b.length*b.numChannels]
}

type Callback func(out Buffer, in Buffer, dur time.Duration, status StreamStatus) int

var (
	mu     sync.Mutex
	audios = map[int]*rtaudio{}
)

func registerAudio(a *rtaudio) int {
	mu.Lock()
	defer mu.Unlock()
	for i := 0; ; i++ {
		if _, ok := audios[i]; !ok {
			audios[i] = a
			return i
		}
	}
}

func unregisterAudio(a *rtaudio) {
	mu.Lock()
	defer mu.Unlock()
	for i := 0; ; i++ {
		if audios[i] == a {
			delete(audios, i)
			return
		}
	}
}

func findAudio(k int) *rtaudio {
	mu.Lock()
	defer mu.Unlock()
	return audios[k]
}

//export goCallback
func goCallback(out, in unsafe.Pointer, frames C.uint, sec C.double,
	status C.rtaudio_stream_status_t, userdata unsafe.Pointer) C.int {

	k := int(uintptr(userdata))
	audio := findAudio(k)
	dur := time.Duration(time.Microsecond * time.Duration(sec*1000000.0))
	inbuf := &buffer{audio.format, int(frames), audio.inputChannels, in}
	outbuf := &buffer{audio.format, int(frames), audio.outputChannels, out}
	return C.int(audio.cb(outbuf, inbuf, dur, StreamStatus(status)))
}

func (audio *rtaudio) Open(out, in *StreamParams, format Format, sampleRate uint,
	frames uint, cb Callback, opts *StreamOptions) error {
	var (
		c_in_ptr   *C.rtaudio_stream_parameters_t
		c_out_ptr  *C.rtaudio_stream_parameters_t
		c_opts_ptr *C.rtaudio_stream_options_t
		c_in       C.rtaudio_stream_parameters_t
		c_out      C.rtaudio_stream_parameters_t
		c_opts     C.rtaudio_stream_options_t
	)

	audio.inputChannels = 0
	audio.outputChannels = 0
	if out != nil {
		audio.outputChannels = int(out.NumChannels)
		c_out.device_id = C.uint(out.DeviceID)
		c_out.num_channels = C.uint(out.NumChannels)
		c_out.first_channel = C.uint(out.FirstChannel)
		c_out_ptr = &c_out
	}
	if in != nil {
		audio.inputChannels = int(in.NumChannels)
		c_in.device_id = C.uint(in.DeviceID)
		c_in.num_channels = C.uint(in.NumChannels)
		c_in.first_channel = C.uint(in.FirstChannel)
		c_in_ptr = &c_in
	}
	if opts != nil {
		c_opts.flags = C.rtaudio_stream_flags_t(opts.Flags)
		c_opts.num_buffers = C.uint(opts.NumBuffers)
		c_opts.priority = C.int(opts.Priotity)
		c_opts_ptr = &c_opts
	}
	frames_count := C.uint(frames)
	audio.format = format
	audio.cb = cb

	k := registerAudio(audio)
	C.rtaudio_open_stream(audio.audio, c_out_ptr, c_in_ptr,
		C.rtaudio_format_t(format), C.uint(sampleRate), &frames_count,
		C.rtaudio_cb_t(C.goCallback), unsafe.Pointer(uintptr(k)), c_opts_ptr, nil)
	if C.rtaudio_error(audio.audio) != nil {
		return errors.New(C.GoString(C.rtaudio_error(audio.audio)))
	}
	return nil
}

func (audio *rtaudio) Close() {
	unregisterAudio(audio)
	C.rtaudio_close_stream(audio.audio)
}

func (audio *rtaudio) Start() error {
	C.rtaudio_start_stream(audio.audio)
	if C.rtaudio_error(audio.audio) != nil {
		return errors.New(C.GoString(C.rtaudio_error(audio.audio)))
	}
	return nil
}

func (audio *rtaudio) Stop() error {
	C.rtaudio_stop_stream(audio.audio)
	if C.rtaudio_error(audio.audio) != nil {
		return errors.New(C.GoString(C.rtaudio_error(audio.audio)))
	}
	return nil
}

func (audio *rtaudio) Abort() error {
	C.rtaudio_abort_stream(audio.audio)
	if C.rtaudio_error(audio.audio) != nil {
		return errors.New(C.GoString(C.rtaudio_error(audio.audio)))
	}
	return nil
}

func (audio *rtaudio) IsOpen() bool {
	return C.rtaudio_is_stream_open(audio.audio) != 0
}

func (audio *rtaudio) IsRunning() bool {
	return C.rtaudio_is_stream_running(audio.audio) != 0
}

func (audio *rtaudio) Latency() (int, error) {
	latency := C.rtaudio_get_stream_latency(audio.audio)
	if C.rtaudio_error(audio.audio) != nil {
		return 0, errors.New(C.GoString(C.rtaudio_error(audio.audio)))
	}
	return int(latency), nil
}

func (audio *rtaudio) SampleRate() (uint, error) {
	sampleRate := C.rtaudio_get_stream_sample_rate(audio.audio)
	if C.rtaudio_error(audio.audio) != nil {
		return 0, errors.New(C.GoString(C.rtaudio_error(audio.audio)))
	}
	return uint(sampleRate), nil
}

func (audio *rtaudio) Time() (time.Duration, error) {
	sec := C.rtaudio_get_stream_time(audio.audio)
	if C.rtaudio_error(audio.audio) != nil {
		return 0, errors.New(C.GoString(C.rtaudio_error(audio.audio)))
	}
	return time.Duration(time.Microsecond * time.Duration(sec*1000000.0)), nil
}

func (audio *rtaudio) SetTime(t time.Duration) error {
	sec := float64(t) * 1000000.0 / float64(time.Microsecond)
	C.rtaudio_set_stream_time(audio.audio, C.double(sec))
	if C.rtaudio_error(audio.audio) != nil {
		return errors.New(C.GoString(C.rtaudio_error(audio.audio)))
	}
	return nil
}

func (audio *rtaudio) ShowWarnings(show bool) {
	if show {
		C.rtaudio_show_warnings(audio.audio, 1)
	} else {
		C.rtaudio_show_warnings(audio.audio, 0)
	}
}
