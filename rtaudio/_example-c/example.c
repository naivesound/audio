#include <math.h>
#include <stdio.h>
#include <unistd.h>

#include "../rtaudio_c.h"

#define SAMPLE_RATE 44100
#define SINE_HZ 440
#define SINE_DURATION_SEC 3

struct context {
    float phase;
};

int cb(void *out, void *in, unsigned int nframes, double stream_time,
       rtaudio_stream_status_t status, void *userdata) {
    float *buf = (float *)out;
    struct context *context = (struct context *)userdata;
    for (int i = 0; i < nframes; i++) {
        buf[i * 2] = buf[i * 2 + 1] = sinf(context->phase);
        context->phase += SINE_HZ * 2 * 3.1415926f / SAMPLE_RATE;
    }
    return 0;
}

int main() {
    int status = 0;
    struct context context = {0};
    rtaudio_t audio = rtaudio_create(RTAUDIO_API_UNSPECIFIED);
    if (rtaudio_error(audio) != NULL) {
        fprintf(stderr, "error: %s\n", rtaudio_error(audio));
        status = 1;
        goto done;
    }

#if 0
    /* Print list of device names and native sample rates */
    for (int i = 0; i < rtaudio_device_count(audio); i++) {
        rtaudio_device_info_t info = rtaudio_get_device_info(audio, i);
        if (rtaudio_error(audio) != NULL) {
            fprintf(stderr, "error: %s\n", rtaudio_error(audio));
            status = 1;
            goto done;
        }
        printf("%c%d: %s: %d\n",
               (info.is_default_input || info.is_default_output) ? '*' : ' ', i,
               info.name, info.preferred_sample_rate);
    }
#endif
    rtaudio_stream_parameters_t out_params = {
        .device_id = rtaudio_get_default_output_device(audio),
        .num_channels = 2,
        .first_channel = 0,
    };
    rtaudio_stream_options_t options = {
        .flags = RTAUDIO_FLAGS_ALSA_USE_DEFAULT,
    };
    unsigned int bufsz = 2048;
    rtaudio_open_stream(audio, &out_params, NULL, RTAUDIO_FORMAT_FLOAT32,
                        SAMPLE_RATE, &bufsz, cb, &context, &options, NULL);
    if (rtaudio_error(audio) != NULL) {
        fprintf(stderr, "error: %s\n", rtaudio_error(audio));
        status = 1;
        goto done;
    }

    rtaudio_start_stream(audio);
    if (rtaudio_error(audio) != NULL) {
        fprintf(stderr, "error: %s\n", rtaudio_error(audio));
        status = 1;
        goto done;
    }

    sleep(SINE_DURATION_SEC);

    rtaudio_stop_stream(audio);

    rtaudio_close_stream(audio);

done:
    rtaudio_destroy(audio);
    return status;
}
