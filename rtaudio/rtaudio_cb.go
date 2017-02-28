package rtaudio

// #include "rtaudio_c.h"
//
// extern int goCallback(void*, void*, unsigned int, double, rtaudio_stream_status_t, void*);
//
// int CgoCallback(void *out, void *in, unsigned int nFrames,
//		    double streamTime, rtaudio_stream_status_t status,
//		    void *userdata) {
// 		return goCallback(out, in, nFrames, streamTime, status, userdata);
// }
import "C"
