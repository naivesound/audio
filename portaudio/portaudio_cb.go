package portaudio

// #include "portaudio.h"
//
// extern void streamCallback(void*, void*, unsigned long,
//        PaStreamCallbackTimeInfo*, PaStreamCallbackFlags, void*);
//
// int CgoStreamCallback(const void *inputBuffer,
//        void *outputBuffer, unsigned long frames,
//        const PaStreamCallbackTimeInfo *timeInfo,
//        PaStreamCallbackFlags statusFlags, void *userData) {
//
//    streamCallback((void*)inputBuffer,
//            outputBuffer, frames,
//            (PaStreamCallbackTimeInfo*)timeInfo, statusFlags, userData);
//    return paContinue;
// }
import "C"
