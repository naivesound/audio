#include <math.h>
#include <stdio.h>

#include "soundio.h"

#define SAMPLE_RATE 44100
#define SINE_HZ 440

static const float PI = 3.1415926535f;
static float seconds_offset = 0.0f;

static void write_cb(struct SoundIoOutStream *out, int frame_count_min,
                     int frame_count_max) {
    const struct SoundIoChannelLayout *layout = &out->layout;
    float float_sample_rate = out->sample_rate;
    float seconds_per_frame = 1.0f / float_sample_rate;
    struct SoundIoChannelArea *areas;
    int frames_left = frame_count_max;
    int err;

    while (frames_left > 0) {
        int frame_count = frames_left;

        if ((err = soundio_outstream_begin_write(out, &areas, &frame_count))) {
            fprintf(stderr, "%s\n", soundio_strerror(err));
            return;
        }

        if (frame_count == 0) {
            break;
        }

        float radians_per_second = SINE_HZ * 2.0f * PI;
        for (int frame = 0; frame < frame_count; frame += 1) {
            float sample = sinf((seconds_offset + frame * seconds_per_frame) *
                                radians_per_second);
            for (int channel = 0; channel < layout->channel_count;
                 channel += 1) {
                float *ptr =
                    (float *)(areas[channel].ptr + areas[channel].step * frame);
                *ptr = sample;
            }
        }
        seconds_offset =
            fmodf(seconds_offset + seconds_per_frame * frame_count, 1.0f);

        if ((err = soundio_outstream_end_write(out))) {
            fprintf(stderr, "%s\n", soundio_strerror(err));
            return;
        }

        frames_left -= frame_count;
    }
}

static void underflow_cb(struct SoundIoOutStream *out) {
    fprintf(stderr, "underflow\n");
}

int main() {
    struct SoundIo *soundio = soundio_create();
    if (soundio == NULL) {
        fprintf(stderr, "soundio_create(): error\n");
        return 1;
    }

    int err = soundio_connect(soundio);
    if (err) {
        fprintf(stderr, "soundio_connect(): %s\n", soundio_strerror(err));
        return 1;
    }

    soundio_flush_events(soundio);

    int index = soundio_default_output_device_index(soundio);
    if (index < 0) {
        fprintf(stderr, "soundio_default_output_device_index(): not found\n");
        return 1;
    }

    struct SoundIoDevice *dev = soundio_get_output_device(soundio, index);
    if (dev == NULL) {
        fprintf(stderr, "soundio_get_output_device(): error\n");
        return 1;
    }

    struct SoundIoOutStream *out = soundio_outstream_create(dev);
    if (out == NULL) {
        fprintf(stderr, "soundio_outstream_create(): error\n");
        return 1;
    }

    out->write_callback = write_cb;
    out->underflow_callback = underflow_cb;
    out->name = "example";
    out->sample_rate = SAMPLE_RATE;
    out->format = SoundIoFormatFloat32NE;

    if ((err = soundio_outstream_open(out))) {
        fprintf(stderr, "soundio_outstream_open(): %s\n",
                soundio_strerror(err));
        return 1;
    }

    if ((err = soundio_outstream_start(out))) {
        fprintf(stderr, "soundio_outstream_start(): %s\n",
                soundio_strerror(err));
        return 1;
    }

    for (;;) {
        soundio_wait_events(soundio);
    }

    soundio_outstream_destroy(out);
    soundio_device_unref(dev);
    soundio_destroy(soundio);

    return 0;
}
