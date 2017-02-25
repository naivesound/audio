#include <math.h>
#include <stdio.h>

#include "../portaudio.h"

#define SAMPLE_RATE 44100
#define SINE_HZ 440
#define SINE_DURATION_SEC 3

struct context {
    float phase;
};

static int cb(const void *in, void *out, unsigned long nframes,
              const PaStreamCallbackTimeInfo *ti, PaStreamCallbackFlags flags,
              void *arg) {
    float *buf = (float *)out;
    struct context *context = (struct context *)arg;

    for (int i = 0; i < nframes; i++) {
        buf[i * 2] = buf[i * 2 + 1] = sinf(context->phase);
        context->phase += SINE_HZ * 2 * 3.1415926f / SAMPLE_RATE;
    }
    return 0;
}

int main() {
    PaStream *stream;
    PaError err;

    struct context context = {0};

    err = Pa_Initialize();
    if (err != paNoError) {
        goto error;
    }

    err = Pa_OpenDefaultStream(&stream, 0, 2, paFloat32, SAMPLE_RATE, 256, cb,
                               &context);
    if (err != paNoError) {
        goto error;
    }

    err = Pa_StartStream(stream);
    if (err != paNoError) {
        goto error;
    }

    Pa_Sleep(SINE_DURATION_SEC * 1000);

    err = Pa_StopStream(stream);
    if (err != paNoError) {
        goto error;
    }
    err = Pa_CloseStream(stream);
    if (err != paNoError) {
        goto error;
    }
error:
    if (err != 0) {
        fprintf(stderr, "error: %s\n", Pa_GetErrorText(err));
    }
    Pa_Terminate();
    return err;
}
