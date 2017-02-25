#include "soundio.h"
#define SOUNDIO_VERSION_STRING ""
#define SOUNDIO_VERSION_MAJOR 0
#define SOUNDIO_VERSION_MINOR 0
#define SOUNDIO_VERSION_PATCH 0
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef SOUNDIO_SOUNDIO_INTERNAL_H
#define SOUNDIO_SOUNDIO_INTERNAL_H

// This exists for __declspec(dllexport) and __declspec(dllimport) to be
// defined correctly without the library user having to do anything.
#define SOUNDIO_BUILDING_LIBRARY
// (amalg) #include "soundio/soundio.h"

#endif
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef SOUNDIO_OS_H
#define SOUNDIO_OS_H

#include <stdbool.h>
#include <stddef.h>

// safe to call from any thread(s) multiple times, but
// must be called at least once before calling any other os functions
// soundio_create calls this function.
int soundio_os_init(void);

double soundio_os_get_time(void);

struct SoundIoOsThread;
int soundio_os_thread_create(
        void (*run)(void *arg), void *arg,
        void (*emit_rtprio_warning)(void),
        struct SoundIoOsThread ** out_thread);

void soundio_os_thread_destroy(struct SoundIoOsThread *thread);


struct SoundIoOsMutex;
struct SoundIoOsMutex *soundio_os_mutex_create(void);
void soundio_os_mutex_destroy(struct SoundIoOsMutex *mutex);
void soundio_os_mutex_lock(struct SoundIoOsMutex *mutex);
void soundio_os_mutex_unlock(struct SoundIoOsMutex *mutex);

struct SoundIoOsCond;
struct SoundIoOsCond *soundio_os_cond_create(void);
void soundio_os_cond_destroy(struct SoundIoOsCond *cond);

// locked_mutex is optional. On systems that use mutexes for conditions, if you
// pass NULL, a mutex will be created and locked/unlocked for you. On systems
// that do not use mutexes for conditions, no mutex handling is necessary. If
// you already have a locked mutex available, pass it; this will be better on
// systems that use mutexes for conditions.
void soundio_os_cond_signal(struct SoundIoOsCond *cond,
        struct SoundIoOsMutex *locked_mutex);
void soundio_os_cond_timed_wait(struct SoundIoOsCond *cond,
        struct SoundIoOsMutex *locked_mutex, double seconds);
void soundio_os_cond_wait(struct SoundIoOsCond *cond,
        struct SoundIoOsMutex *locked_mutex);


int soundio_os_page_size(void);

// You may rely on the size of this struct as part of the API and ABI.
struct SoundIoOsMirroredMemory {
    size_t capacity;
    char *address;
    void *priv;
};

// returned capacity might be increased from capacity to be a multiple of the
// system page size
int soundio_os_init_mirrored_memory(struct SoundIoOsMirroredMemory *mem, size_t capacity);
void soundio_os_deinit_mirrored_memory(struct SoundIoOsMirroredMemory *mem);

#endif
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef SOUNDIO_ATOMICS_H
#define SOUNDIO_ATOMICS_H

// Simple wrappers around atomic values so that the compiler will catch it if
// I accidentally use operators such as +, -, += on them.

#ifdef __cplusplus

#include <atomic>

struct SoundIoAtomicLong {
    std::atomic<long> x;
};

struct SoundIoAtomicInt {
    std::atomic<int> x;
};

struct SoundIoAtomicBool {
    std::atomic<bool> x;
};

struct SoundIoAtomicFlag {
    std::atomic_flag x;
};

#define SOUNDIO_ATOMIC_LOAD(a) (a.x.load())
#define SOUNDIO_ATOMIC_FETCH_ADD(a, delta) (a.x.fetch_add(delta))
#define SOUNDIO_ATOMIC_STORE(a, value) (a.x.store(value))
#define SOUNDIO_ATOMIC_EXCHANGE(a, value) (a.x.exchange(value))
#define SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(a) (a.x.test_and_set())
#define SOUNDIO_ATOMIC_FLAG_CLEAR(a) (a.x.clear())
#define SOUNDIO_ATOMIC_FLAG_INIT ATOMIC_FLAG_INIT

#else

#include <stdatomic.h>

struct SoundIoAtomicLong {
    atomic_long x;
};

struct SoundIoAtomicInt {
    atomic_int x;
};

struct SoundIoAtomicBool {
    atomic_bool x;
};

struct SoundIoAtomicFlag {
    atomic_flag x;
};

#define SOUNDIO_ATOMIC_LOAD(a) atomic_load(&a.x)
#define SOUNDIO_ATOMIC_FETCH_ADD(a, delta) atomic_fetch_add(&a.x, delta)
#define SOUNDIO_ATOMIC_STORE(a, value) atomic_store(&a.x, value)
#define SOUNDIO_ATOMIC_EXCHANGE(a, value) atomic_exchange(&a.x, value)
#define SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(a) atomic_flag_test_and_set(&a.x)
#define SOUNDIO_ATOMIC_FLAG_CLEAR(a) atomic_flag_clear(&a.x)
#define SOUNDIO_ATOMIC_FLAG_INIT ATOMIC_FLAG_INIT

#endif

#endif
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef SOUNDIO_LIST_H
#define SOUNDIO_LIST_H

// (amalg) #include "util.h"
// (amalg) #include "soundio_internal.h"

#include <assert.h>

#define SOUNDIO_LIST_STATIC static
#define SOUNDIO_LIST_NOT_STATIC

#define SOUNDIO_MAKE_LIST_STRUCT(Type, Name, static_kw) \
    struct Name { \
        Type *items; \
        int length; \
        int capacity; \
    };

#define SOUNDIO_MAKE_LIST_PROTO(Type, Name, static_kw) \
    static_kw void Name##_deinit(struct Name *s); \
    static_kw int SOUNDIO_ATTR_WARN_UNUSED_RESULT Name##_append(struct Name *s, Type item); \
    static_kw Type Name##_val_at(struct Name *s, int index); \
    static_kw Type * Name##_ptr_at(struct Name *s, int index); \
    static_kw Type Name##_pop(struct Name *s); \
    static_kw int SOUNDIO_ATTR_WARN_UNUSED_RESULT Name##_add_one(struct Name *s); \
    static_kw Type Name##_last_val(struct Name *s); \
    static_kw Type *Name##_last_ptr(struct Name *s); \
    static_kw int SOUNDIO_ATTR_WARN_UNUSED_RESULT Name##_resize(struct Name *s, int new_length); \
    static_kw void Name##_clear(struct Name *s); \
    static_kw int SOUNDIO_ATTR_WARN_UNUSED_RESULT \
        Name##_ensure_capacity(struct Name *s, int new_capacity); \
    static_kw Type Name##_swap_remove(struct Name *s, int index);


#define SOUNDIO_MAKE_LIST_DEF(Type, Name, static_kw) \
    SOUNDIO_ATTR_UNUSED \
    static_kw void Name##_deinit(struct Name *s) { \
        free(s->items); \
    } \
\
    SOUNDIO_ATTR_UNUSED \
    SOUNDIO_ATTR_WARN_UNUSED_RESULT \
    static_kw int Name##_ensure_capacity(struct Name *s, int new_capacity) { \
        int better_capacity = soundio_int_max(s->capacity, 16); \
        while (better_capacity < new_capacity) \
            better_capacity = better_capacity * 2; \
        if (better_capacity != s->capacity) { \
            Type *new_items = REALLOCATE_NONZERO(Type, s->items, better_capacity); \
            if (!new_items) \
                return SoundIoErrorNoMem; \
            s->items = new_items; \
            s->capacity = better_capacity; \
        } \
        return 0; \
    } \
\
    SOUNDIO_ATTR_UNUSED \
    SOUNDIO_ATTR_WARN_UNUSED_RESULT \
    static_kw int Name##_append(struct Name *s, Type item) { \
        int err = Name##_ensure_capacity(s, s->length + 1); \
        if (err) \
            return err; \
        s->items[s->length] = item; \
        s->length += 1; \
        return 0; \
    } \
\
    SOUNDIO_ATTR_UNUSED \
    static_kw Type Name##_val_at(struct Name *s, int index) {                                            \
        assert(index >= 0);                                                              \
        assert(index < s->length);                                                          \
        return s->items[index];                                                             \
    } \
\
    /* remember that the pointer to this item is invalid after you \
     * modify the length of the list \
     */ \
    SOUNDIO_ATTR_UNUSED \
    static_kw Type * Name##_ptr_at(struct Name *s, int index) { \
        assert(index >= 0); \
        assert(index < s->length); \
        return &s->items[index]; \
    } \
\
    SOUNDIO_ATTR_UNUSED \
    static_kw Type Name##_pop(struct Name *s) { \
        assert(s->length >= 1); \
        s->length -= 1; \
        return s->items[s->length]; \
    }                                                                                    \
\
    SOUNDIO_ATTR_UNUSED \
    SOUNDIO_ATTR_WARN_UNUSED_RESULT \
    static_kw int Name##_resize(struct Name *s, int new_length) {    \
        assert(new_length >= 0);                                                         \
        int err = Name##_ensure_capacity(s, new_length);                                           \
        if (err)                                                                         \
            return err;                                                                  \
        s->length = new_length;                                                             \
        return 0;                                                                        \
    }                                                                                    \
\
    SOUNDIO_ATTR_UNUSED \
    SOUNDIO_ATTR_WARN_UNUSED_RESULT \
    static_kw int Name##_add_one(struct Name *s) { \
        return Name##_resize(s, s->length + 1); \
    } \
\
    SOUNDIO_ATTR_UNUSED \
    static_kw Type Name##_last_val(struct Name *s) {                                                   \
        assert(s->length >= 1);                                                             \
        return s->items[s->length - 1];                                                        \
    }                                                                                    \
\
    SOUNDIO_ATTR_UNUSED \
    static_kw Type *Name##_last_ptr(struct Name *s) {                                                  \
        assert(s->length >= 1);                                                             \
        return &s->items[s->length - 1];                                                       \
    }                                                                                    \
\
    SOUNDIO_ATTR_UNUSED \
    static_kw void Name##_clear(struct Name *s) {                                                      \
        s->length = 0;                                                                      \
    }                                                                                    \
\
    SOUNDIO_ATTR_UNUSED \
    static_kw Type Name##_swap_remove(struct Name *s, int index) { \
        assert(index >= 0); \
        assert(index < s->length); \
        Type last = Name##_pop(s); \
        if (index == s->length) \
            return last; \
        Type item = s->items[index]; \
        s->items[index] = last; \
        return item; \
    }

#endif
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef SOUNDIO_RING_BUFFER_H
#define SOUNDIO_RING_BUFFER_H

// (amalg) #include "os.h"
// (amalg) #include "atomics.h"

struct SoundIoRingBuffer {
    struct SoundIoOsMirroredMemory mem;
    struct SoundIoAtomicLong write_offset;
    struct SoundIoAtomicLong read_offset;
    int capacity;
};

int soundio_ring_buffer_init(struct SoundIoRingBuffer *rb, int requested_capacity);
void soundio_ring_buffer_deinit(struct SoundIoRingBuffer *rb);

#endif
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef SOUNDIO_UTIL_H
#define SOUNDIO_UTIL_H

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

#define ALLOCATE_NONZERO(Type, count) ((Type*)malloc((count) * sizeof(Type)))

#define ALLOCATE(Type, count) ((Type*)calloc(count, sizeof(Type)))

#define REALLOCATE_NONZERO(Type, old, new_count) ((Type*)realloc(old, (new_count) * sizeof(Type)))

#define ARRAY_LENGTH(array) (sizeof(array)/sizeof((array)[0]))

#ifdef _MSC_VER
#define SOUNDIO_ATTR_COLD
#define SOUNDIO_ATTR_NORETURN __declspec(noreturn)
#define SOUNDIO_ATTR_FORMAT(...)
#define SOUNDIO_ATTR_UNUSED __pragma(warning(suppress:4100))
#define SOUNDIO_ATTR_WARN_UNUSED_RESULT _Check_return_
#else
#define SOUNDIO_ATTR_COLD __attribute__((cold))
#define SOUNDIO_ATTR_NORETURN __attribute__((noreturn))
#define SOUNDIO_ATTR_FORMAT(...) __attribute__((format(__VA_ARGS__)))
#define SOUNDIO_ATTR_UNUSED __attribute__((unused))
#define SOUNDIO_ATTR_WARN_UNUSED_RESULT __attribute__((warn_unused_result))
#endif


static inline int soundio_int_min(int a, int b) {
    return (a <= b) ? a : b;
}

static inline int soundio_int_max(int a, int b) {
    return (a >= b) ? a : b;
}

static inline int soundio_int_clamp(int min_value, int value, int max_value) {
    return soundio_int_max(soundio_int_min(value, max_value), min_value);
}

static inline double soundio_double_min(double a, double b) {
    return (a <= b) ? a : b;
}

static inline double soundio_double_max(double a, double b) {
    return (a >= b) ? a : b;
}

static inline double soundio_double_clamp(double min_value, double value, double max_value) {
    return soundio_double_max(soundio_double_min(value, max_value), min_value);
}

SOUNDIO_ATTR_NORETURN
void soundio_panic(const char *format, ...)
    SOUNDIO_ATTR_COLD
    SOUNDIO_ATTR_FORMAT(printf, 1, 2)
    ;

char *soundio_alloc_sprintf(int *len, const char *format, ...)
    SOUNDIO_ATTR_FORMAT(printf, 2, 3);

static inline char *soundio_str_dupe(const char *str, int str_len) {
    char *out = ALLOCATE_NONZERO(char, str_len + 1);
    if (!out)
        return NULL;
    memcpy(out, str, str_len);
    out[str_len] = 0;
    return out;
}

static inline bool soundio_streql(const char *str1, int str1_len, const char *str2, int str2_len) {
    if (str1_len != str2_len)
        return false;
    return memcmp(str1, str2, str1_len) == 0;
}

static inline int ceil_dbl_to_int(double x) {
    const double truncation = (int)x;
    return truncation + (truncation < x);
}

static inline double ceil_dbl(double x) {
    const double truncation = (long long) x;
    const double ceiling = truncation + (truncation < x);
    return ceiling;
}

#endif
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef SOUNDIO_DUMMY_H
#define SOUNDIO_DUMMY_H

// (amalg) #include "soundio_internal.h"
// (amalg) #include "os.h"
// (amalg) #include "ring_buffer.h"
// (amalg) #include "atomics.h"

struct SoundIoPrivate;
int soundio_dummy_init(struct SoundIoPrivate *si);

struct SoundIoDummy {
    struct SoundIoOsMutex *mutex;
    struct SoundIoOsCond *cond;
    bool devices_emitted;
};

struct SoundIoDeviceDummy { int make_the_struct_not_empty; };

struct SoundIoOutStreamDummy {
    struct SoundIoOsThread *thread;
    struct SoundIoOsCond *cond;
    struct SoundIoAtomicFlag abort_flag;
    double period_duration;
    int buffer_frame_count;
    int frames_left;
    int write_frame_count;
    struct SoundIoRingBuffer ring_buffer;
    double playback_start_time;
    struct SoundIoAtomicFlag clear_buffer_flag;
    struct SoundIoAtomicBool pause_requested;
    struct SoundIoChannelArea areas[SOUNDIO_MAX_CHANNELS];
};

struct SoundIoInStreamDummy {
    struct SoundIoOsThread *thread;
    struct SoundIoOsCond *cond;
    struct SoundIoAtomicFlag abort_flag;
    double period_duration;
    int frames_left;
    int read_frame_count;
    int buffer_frame_count;
    struct SoundIoRingBuffer ring_buffer;
    struct SoundIoAtomicBool pause_requested;
    struct SoundIoChannelArea areas[SOUNDIO_MAX_CHANNELS];
};

#endif

#if SOUNDIO_HAVE_JACK
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef SOUNDIO_JACK_H
#define SOUNDIO_JACK_H

// (amalg) #include "soundio_internal.h"
// (amalg) #include "os.h"
// (amalg) #include "atomics.h"

// jack.h does not properly put `void` in function prototypes with no
// arguments, so we're forced to temporarily disable -Werror=strict-prototypes
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-prototypes"
#include <jack/jack.h>
#pragma GCC diagnostic pop

struct SoundIoPrivate;
int soundio_jack_init(struct SoundIoPrivate *si);

struct SoundIoDeviceJackPort {
    char *full_name;
    int full_name_len;
    enum SoundIoChannelId channel_id;
    jack_latency_range_t latency_range;
};

struct SoundIoDeviceJack {
    int port_count;
    struct SoundIoDeviceJackPort *ports;
};

struct SoundIoJack {
    jack_client_t *client;
    struct SoundIoOsMutex *mutex;
    struct SoundIoOsCond *cond;
    struct SoundIoAtomicFlag refresh_devices_flag;
    int sample_rate;
    int period_size;
    bool is_shutdown;
    bool emitted_shutdown_cb;
};

struct SoundIoOutStreamJackPort {
    jack_port_t *source_port;
    const char *dest_port_name;
    int dest_port_name_len;
};

struct SoundIoOutStreamJack {
    jack_client_t *client;
    int period_size;
    int frames_left;
    double hardware_latency;
    struct SoundIoOutStreamJackPort ports[SOUNDIO_MAX_CHANNELS];
    struct SoundIoChannelArea areas[SOUNDIO_MAX_CHANNELS];
};

struct SoundIoInStreamJackPort {
    jack_port_t *dest_port;
    const char *source_port_name;
    int source_port_name_len;
};

struct SoundIoInStreamJack {
    jack_client_t *client;
    int period_size;
    int frames_left;
    double hardware_latency;
    struct SoundIoInStreamJackPort ports[SOUNDIO_MAX_CHANNELS];
    struct SoundIoChannelArea areas[SOUNDIO_MAX_CHANNELS];
    char *buf_ptrs[SOUNDIO_MAX_CHANNELS];
};

#endif
#endif
#if SOUNDIO_HAVE_ALSA
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef SOUNDIO_ALSA_H
#define SOUNDIO_ALSA_H

// (amalg) #include "soundio_internal.h"
// (amalg) #include "os.h"
// (amalg) #include "list.h"
// (amalg) #include "atomics.h"

#include <alsa/asoundlib.h>

struct SoundIoPrivate;
int soundio_alsa_init(struct SoundIoPrivate *si);

struct SoundIoDeviceAlsa { int make_the_struct_not_empty; };

#define SOUNDIO_MAX_ALSA_SND_FILE_LEN 16
struct SoundIoAlsaPendingFile {
    char name[SOUNDIO_MAX_ALSA_SND_FILE_LEN];
};

SOUNDIO_MAKE_LIST_STRUCT(struct SoundIoAlsaPendingFile, SoundIoListAlsaPendingFile, SOUNDIO_LIST_STATIC)

struct SoundIoAlsa {
    struct SoundIoOsMutex *mutex;
    struct SoundIoOsCond *cond;

    struct SoundIoOsThread *thread;
    struct SoundIoAtomicFlag abort_flag;
    int notify_fd;
    int notify_wd;
    bool have_devices_flag;
    int notify_pipe_fd[2];
    struct SoundIoListAlsaPendingFile pending_files;

    // this one is ready to be read with flush_events. protected by mutex
    struct SoundIoDevicesInfo *ready_devices_info;

    int shutdown_err;
    bool emitted_shutdown_cb;
};

struct SoundIoOutStreamAlsa {
    snd_pcm_t *handle;
    snd_pcm_chmap_t *chmap;
    int chmap_size;
    snd_pcm_uframes_t offset;
    snd_pcm_access_t access;
    snd_pcm_uframes_t buffer_size_frames;
    int sample_buffer_size;
    char *sample_buffer;
    int poll_fd_count;
    int poll_fd_count_with_extra;
    struct pollfd *poll_fds;
    int poll_exit_pipe_fd[2];
    struct SoundIoOsThread *thread;
    struct SoundIoAtomicFlag thread_exit_flag;
    snd_pcm_uframes_t period_size;
    int write_frame_count;
    bool is_paused;
    struct SoundIoAtomicFlag clear_buffer_flag;
    struct SoundIoChannelArea areas[SOUNDIO_MAX_CHANNELS];
};

struct SoundIoInStreamAlsa {
    snd_pcm_t *handle;
    snd_pcm_chmap_t *chmap;
    int chmap_size;
    snd_pcm_uframes_t offset;
    snd_pcm_access_t access;
    int sample_buffer_size;
    char *sample_buffer;
    int poll_fd_count;
    struct pollfd *poll_fds;
    struct SoundIoOsThread *thread;
    struct SoundIoAtomicFlag thread_exit_flag;
    int period_size;
    int read_frame_count;
    bool is_paused;
    struct SoundIoChannelArea areas[SOUNDIO_MAX_CHANNELS];
};

#endif
#endif
#if SOUNDIO_HAVE_PULSEAUDIO
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef SOUNDIO_PULSEAUDIO_H
#define SOUNDIO_PULSEAUDIO_H

// (amalg) #include "soundio_internal.h"
// (amalg) #include "atomics.h"

#include <pulse/pulseaudio.h>

struct SoundIoPrivate;
int soundio_pulseaudio_init(struct SoundIoPrivate *si);

struct SoundIoDevicePulseAudio { int make_the_struct_not_empty; };

struct SoundIoPulseAudio {
    int device_query_err;
    int connection_err;
    bool emitted_shutdown_cb;

    pa_context *pulse_context;
    bool device_scan_queued;

    // the one that we're working on building
    struct SoundIoDevicesInfo *current_devices_info;
    char *default_sink_name;
    char *default_source_name;

    // this one is ready to be read with flush_events. protected by mutex
    struct SoundIoDevicesInfo *ready_devices_info;

    bool ready_flag;

    pa_threaded_mainloop *main_loop;
    pa_proplist *props;
};

struct SoundIoOutStreamPulseAudio {
    pa_stream *stream;
    struct SoundIoAtomicBool stream_ready;
    pa_buffer_attr buffer_attr;
    char *write_ptr;
    size_t write_byte_count;
    struct SoundIoAtomicFlag clear_buffer_flag;
    struct SoundIoChannelArea areas[SOUNDIO_MAX_CHANNELS];
};

struct SoundIoInStreamPulseAudio {
    pa_stream *stream;
    struct SoundIoAtomicBool stream_ready;
    pa_buffer_attr buffer_attr;
    char *peek_buf;
    size_t peek_buf_index;
    size_t peek_buf_size;
    int peek_buf_frames_left;
    int read_frame_count;
    struct SoundIoChannelArea areas[SOUNDIO_MAX_CHANNELS];
};

#endif
#endif
#if SOUNDIO_HAVE_COREAUDIO
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef SOUNDIO_COREAUDIO_H
#define SOUNDIO_COREAUDIO_H

// (amalg) #include "soundio_internal.h"
// (amalg) #include "os.h"
// (amalg) #include "list.h"
// (amalg) #include "atomics.h"

#include <CoreAudio/CoreAudio.h>
#include <AudioUnit/AudioUnit.h>

struct SoundIoPrivate;
int soundio_coreaudio_init(struct SoundIoPrivate *si);

struct SoundIoDeviceCoreAudio {
    AudioDeviceID device_id;
    UInt32 latency_frames;
};

SOUNDIO_MAKE_LIST_STRUCT(AudioDeviceID, SoundIoListAudioDeviceID, SOUNDIO_LIST_STATIC)

struct SoundIoCoreAudio {
    struct SoundIoOsMutex *mutex;
    struct SoundIoOsCond *cond;
    struct SoundIoOsThread *thread;
    struct SoundIoAtomicFlag abort_flag;

    // this one is ready to be read with flush_events. protected by mutex
    struct SoundIoDevicesInfo *ready_devices_info;
    struct SoundIoAtomicBool have_devices_flag;
    struct SoundIoOsCond *have_devices_cond;
    struct SoundIoOsCond *scan_devices_cond;
    struct SoundIoListAudioDeviceID registered_listeners;

    struct SoundIoAtomicBool device_scan_queued;
    struct SoundIoAtomicBool service_restarted;
    int shutdown_err;
    bool emitted_shutdown_cb;
};

struct SoundIoOutStreamCoreAudio {
    AudioComponentInstance instance;
    AudioBufferList *io_data;
    int buffer_index;
    int frames_left;
    int write_frame_count;
    double hardware_latency;
    struct SoundIoChannelArea areas[SOUNDIO_MAX_CHANNELS];
};

struct SoundIoInStreamCoreAudio {
    AudioComponentInstance instance;
    AudioBufferList *buffer_list;
    int frames_left;
    double hardware_latency;
    struct SoundIoChannelArea areas[SOUNDIO_MAX_CHANNELS];
};

#endif
#endif
#if SOUNDIO_HAVE_WASAPI
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef SOUNDIO_WASAPI_H
#define SOUNDIO_WASAPI_H

// (amalg) #include "soundio_internal.h"
// (amalg) #include "os.h"
// (amalg) #include "list.h"
// (amalg) #include "atomics.h"

#define CINTERFACE
#define COBJMACROS
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#include <mmdeviceapi.h>
#include <audioclient.h>
#include <audiosessiontypes.h>
#include <audiopolicy.h>

struct SoundIoPrivate;
int soundio_wasapi_init(struct SoundIoPrivate *si);

struct SoundIoDeviceWasapi {
    double period_duration;
    IMMDevice *mm_device;
};

struct SoundIoWasapi {
    struct SoundIoOsMutex *mutex;
    struct SoundIoOsCond *cond;
    struct SoundIoOsCond *scan_devices_cond;
    struct SoundIoOsMutex *scan_devices_mutex;
    struct SoundIoOsThread *thread;
    bool abort_flag;
    // this one is ready to be read with flush_events. protected by mutex
    struct SoundIoDevicesInfo *ready_devices_info;
    bool have_devices_flag;
    bool device_scan_queued;
    int shutdown_err;
    bool emitted_shutdown_cb;

    IMMDeviceEnumerator* device_enumerator;
    IMMNotificationClient device_events;
    LONG device_events_refs;
};

struct SoundIoOutStreamWasapi {
    IAudioClient *audio_client;
    IAudioClockAdjustment *audio_clock_adjustment;
    IAudioRenderClient *audio_render_client;
    IAudioSessionControl *audio_session_control;
    LPWSTR stream_name;
    bool need_resample;
    struct SoundIoOsThread *thread;
    struct SoundIoOsMutex *mutex;
    struct SoundIoOsCond *cond;
    struct SoundIoOsCond *start_cond;
    struct SoundIoAtomicFlag thread_exit_flag;
    bool is_raw;
    int writable_frame_count;
    UINT32 buffer_frame_count;
    int write_frame_count;
    HANDLE h_event;
    struct SoundIoAtomicBool desired_pause_state;
    struct SoundIoAtomicFlag pause_resume_flag;
    struct SoundIoAtomicFlag clear_buffer_flag;
    bool is_paused;
    bool open_complete;
    int open_err;
    bool started;
    UINT32 min_padding_frames;
    struct SoundIoChannelArea areas[SOUNDIO_MAX_CHANNELS];
};

struct SoundIoInStreamWasapi {
    IAudioClient *audio_client;
    IAudioCaptureClient *audio_capture_client;
    IAudioSessionControl *audio_session_control;
    LPWSTR stream_name;
    struct SoundIoOsThread *thread;
    struct SoundIoOsMutex *mutex;
    struct SoundIoOsCond *cond;
    struct SoundIoOsCond *start_cond;
    struct SoundIoAtomicFlag thread_exit_flag;
    bool is_raw;
    int readable_frame_count;
    UINT32 buffer_frame_count;
    int read_frame_count;
    HANDLE h_event;
    bool is_paused;
    bool open_complete;
    int open_err;
    bool started;
    char *read_buf;
    int read_buf_frames_left;
    struct SoundIoChannelArea areas[SOUNDIO_MAX_CHANNELS];
};

#endif
#endif

/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef SOUNDIO_SOUNDIO_PRIVATE_H
#define SOUNDIO_SOUNDIO_PRIVATE_H

// (amalg) #include "soundio_internal.h"
// (amalg) #include "config.h"
// (amalg) #include "list.h"

#ifdef SOUNDIO_HAVE_JACK
// (amalg) #include "jack.h"
#endif

#ifdef SOUNDIO_HAVE_PULSEAUDIO
// (amalg) #include "pulseaudio.h"
#endif

#ifdef SOUNDIO_HAVE_ALSA
// (amalg) #include "alsa.h"
#endif

#ifdef SOUNDIO_HAVE_COREAUDIO
// (amalg) #include "coreaudio.h"
#endif

#ifdef SOUNDIO_HAVE_WASAPI
// (amalg) #include "wasapi.h"
#endif

// (amalg) #include "dummy.h"

union SoundIoBackendData {
#ifdef SOUNDIO_HAVE_JACK
    struct SoundIoJack jack;
#endif
#ifdef SOUNDIO_HAVE_PULSEAUDIO
    struct SoundIoPulseAudio pulseaudio;
#endif
#ifdef SOUNDIO_HAVE_ALSA
    struct SoundIoAlsa alsa;
#endif
#ifdef SOUNDIO_HAVE_COREAUDIO
    struct SoundIoCoreAudio coreaudio;
#endif
#ifdef SOUNDIO_HAVE_WASAPI
    struct SoundIoWasapi wasapi;
#endif
    struct SoundIoDummy dummy;
};

union SoundIoDeviceBackendData {
#ifdef SOUNDIO_HAVE_JACK
    struct SoundIoDeviceJack jack;
#endif
#ifdef SOUNDIO_HAVE_PULSEAUDIO
    struct SoundIoDevicePulseAudio pulseaudio;
#endif
#ifdef SOUNDIO_HAVE_ALSA
    struct SoundIoDeviceAlsa alsa;
#endif
#ifdef SOUNDIO_HAVE_COREAUDIO
    struct SoundIoDeviceCoreAudio coreaudio;
#endif
#ifdef SOUNDIO_HAVE_WASAPI
    struct SoundIoDeviceWasapi wasapi;
#endif
    struct SoundIoDeviceDummy dummy;
};

union SoundIoOutStreamBackendData {
#ifdef SOUNDIO_HAVE_JACK
    struct SoundIoOutStreamJack jack;
#endif
#ifdef SOUNDIO_HAVE_PULSEAUDIO
    struct SoundIoOutStreamPulseAudio pulseaudio;
#endif
#ifdef SOUNDIO_HAVE_ALSA
    struct SoundIoOutStreamAlsa alsa;
#endif
#ifdef SOUNDIO_HAVE_COREAUDIO
    struct SoundIoOutStreamCoreAudio coreaudio;
#endif
#ifdef SOUNDIO_HAVE_WASAPI
    struct SoundIoOutStreamWasapi wasapi;
#endif
    struct SoundIoOutStreamDummy dummy;
};

union SoundIoInStreamBackendData {
#ifdef SOUNDIO_HAVE_JACK
    struct SoundIoInStreamJack jack;
#endif
#ifdef SOUNDIO_HAVE_PULSEAUDIO
    struct SoundIoInStreamPulseAudio pulseaudio;
#endif
#ifdef SOUNDIO_HAVE_ALSA
    struct SoundIoInStreamAlsa alsa;
#endif
#ifdef SOUNDIO_HAVE_COREAUDIO
    struct SoundIoInStreamCoreAudio coreaudio;
#endif
#ifdef SOUNDIO_HAVE_WASAPI
    struct SoundIoInStreamWasapi wasapi;
#endif
    struct SoundIoInStreamDummy dummy;
};

SOUNDIO_MAKE_LIST_STRUCT(struct SoundIoDevice*, SoundIoListDevicePtr, SOUNDIO_LIST_NOT_STATIC)
SOUNDIO_MAKE_LIST_PROTO(struct SoundIoDevice*, SoundIoListDevicePtr, SOUNDIO_LIST_NOT_STATIC)

struct SoundIoDevicesInfo {
    struct SoundIoListDevicePtr input_devices;
    struct SoundIoListDevicePtr output_devices;
    // can be -1 when default device is unknown
    int default_output_index;
    int default_input_index;
};

struct SoundIoOutStreamPrivate {
    struct SoundIoOutStream pub;
    union SoundIoOutStreamBackendData backend_data;
};

struct SoundIoInStreamPrivate {
    struct SoundIoInStream pub;
    union SoundIoInStreamBackendData backend_data;
};

struct SoundIoPrivate {
    struct SoundIo pub;

    // Safe to read from a single thread without a mutex.
    struct SoundIoDevicesInfo *safe_devices_info;

    void (*destroy)(struct SoundIoPrivate *);
    void (*flush_events)(struct SoundIoPrivate *);
    void (*wait_events)(struct SoundIoPrivate *);
    void (*wakeup)(struct SoundIoPrivate *);
    void (*force_device_scan)(struct SoundIoPrivate *);

    int (*outstream_open)(struct SoundIoPrivate *, struct SoundIoOutStreamPrivate *);
    void (*outstream_destroy)(struct SoundIoPrivate *, struct SoundIoOutStreamPrivate *);
    int (*outstream_start)(struct SoundIoPrivate *, struct SoundIoOutStreamPrivate *);
    int (*outstream_begin_write)(struct SoundIoPrivate *, struct SoundIoOutStreamPrivate *,
            struct SoundIoChannelArea **out_areas, int *out_frame_count);
    int (*outstream_end_write)(struct SoundIoPrivate *, struct SoundIoOutStreamPrivate *);
    int (*outstream_clear_buffer)(struct SoundIoPrivate *, struct SoundIoOutStreamPrivate *);
    int (*outstream_pause)(struct SoundIoPrivate *, struct SoundIoOutStreamPrivate *, bool pause);
    int (*outstream_get_latency)(struct SoundIoPrivate *, struct SoundIoOutStreamPrivate *, double *out_latency);


    int (*instream_open)(struct SoundIoPrivate *, struct SoundIoInStreamPrivate *);
    void (*instream_destroy)(struct SoundIoPrivate *, struct SoundIoInStreamPrivate *);
    int (*instream_start)(struct SoundIoPrivate *, struct SoundIoInStreamPrivate *);
    int (*instream_begin_read)(struct SoundIoPrivate *, struct SoundIoInStreamPrivate *,
            struct SoundIoChannelArea **out_areas, int *out_frame_count);
    int (*instream_end_read)(struct SoundIoPrivate *, struct SoundIoInStreamPrivate *);
    int (*instream_pause)(struct SoundIoPrivate *, struct SoundIoInStreamPrivate *, bool pause);
    int (*instream_get_latency)(struct SoundIoPrivate *, struct SoundIoInStreamPrivate *, double *out_latency);

    union SoundIoBackendData backend_data;
};

SOUNDIO_MAKE_LIST_STRUCT(struct SoundIoSampleRateRange, SoundIoListSampleRateRange, SOUNDIO_LIST_NOT_STATIC)
SOUNDIO_MAKE_LIST_PROTO(struct SoundIoSampleRateRange, SoundIoListSampleRateRange, SOUNDIO_LIST_NOT_STATIC)

struct SoundIoDevicePrivate {
    struct SoundIoDevice pub;
    union SoundIoDeviceBackendData backend_data;
    void (*destruct)(struct SoundIoDevicePrivate *);
    struct SoundIoSampleRateRange prealloc_sample_rate_range;
    struct SoundIoListSampleRateRange sample_rates;
    enum SoundIoFormat prealloc_format;
};

void soundio_destroy_devices_info(struct SoundIoDevicesInfo *devices_info);

static const int SOUNDIO_MIN_SAMPLE_RATE = 8000;
static const int SOUNDIO_MAX_SAMPLE_RATE = 5644800;

#endif

/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#if defined(__APPLE__)
#define _DARWIN_C_SOURCE
#undef _POSIX_C_SOURCE
#else
#define _GNU_SOURCE
#endif

// (amalg) #include "os.h"
// (amalg) #include "soundio_internal.h"
// (amalg) #include "util.h"

#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include <string.h>
#include <errno.h>

#if defined(_WIN32)
#define SOUNDIO_OS_WINDOWS

#if !defined(NOMINMAX)
#define NOMINMAX
#endif

#if !defined(VC_EXTRALEAN)
#define VC_EXTRALEAN
#endif

#if !defined(WIN32_LEAN_AND_MEAN)
#define WIN32_LEAN_AND_MEAN
#endif

#if !defined(UNICODE)
#define UNICODE
#endif

// require Windows 7 or later
#if WINVER < 0x0601
#undef WINVER
#define WINVER 0x0601
#endif
#if _WIN32_WINNT < 0x0601
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0601
#endif

#include <windows.h>
#include <mmsystem.h>
#include <objbase.h>

#else

#include <pthread.h>
#include <unistd.h>
#include <sys/mman.h>
#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif

#endif

#if defined(__FreeBSD__) || defined(__MACH__)
#define SOUNDIO_OS_KQUEUE
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#endif

#if defined(__MACH__)
#include <mach/clock.h>
#include <mach/mach.h>
#endif

struct SoundIoOsThread {
#if defined(SOUNDIO_OS_WINDOWS)
    HANDLE handle;
    DWORD id;
#else
    pthread_attr_t attr;
    bool attr_init;

    pthread_t id;
    bool running;
#endif
    void *arg;
    void (*run)(void *arg);
};

struct SoundIoOsMutex {
#if defined(SOUNDIO_OS_WINDOWS)
    CRITICAL_SECTION id;
#else
    pthread_mutex_t id;
    bool id_init;
#endif
};

#if defined(SOUNDIO_OS_KQUEUE)
static const uintptr_t notify_ident = 1;
struct SoundIoOsCond {
    int kq_id;
};
#elif defined(SOUNDIO_OS_WINDOWS)
struct SoundIoOsCond {
    CONDITION_VARIABLE id;
    CRITICAL_SECTION default_cs_id;
};
#else
struct SoundIoOsCond {
    pthread_cond_t id;
    bool id_init;

    pthread_condattr_t attr;
    bool attr_init;

    pthread_mutex_t default_mutex_id;
    bool default_mutex_init;
};
#endif

#if defined(SOUNDIO_OS_WINDOWS)
static INIT_ONCE win32_init_once = INIT_ONCE_STATIC_INIT;
static double win32_time_resolution;
static SYSTEM_INFO win32_system_info;
#else
static bool initialized = false;
static pthread_mutex_t init_mutex = PTHREAD_MUTEX_INITIALIZER;
#if defined(__MACH__)
static clock_serv_t cclock;
#endif
#endif

static int page_size;

double soundio_os_get_time(void) {
#if defined(SOUNDIO_OS_WINDOWS)
    unsigned __int64 time;
    QueryPerformanceCounter((LARGE_INTEGER*) &time);
    return time * win32_time_resolution;
#elif defined(__MACH__)
    mach_timespec_t mts;

    kern_return_t err = clock_get_time(cclock, &mts);
    assert(!err);

    double seconds = (double)mts.tv_sec;
    seconds += ((double)mts.tv_nsec) / 1000000000.0;

    return seconds;
#else
    struct timespec tms;
    clock_gettime(CLOCK_MONOTONIC, &tms);
    double seconds = (double)tms.tv_sec;
    seconds += ((double)tms.tv_nsec) / 1000000000.0;
    return seconds;
#endif
}

#if defined(SOUNDIO_OS_WINDOWS)
static DWORD WINAPI run_win32_thread(LPVOID userdata) {
    struct SoundIoOsThread *thread = (struct SoundIoOsThread *)userdata;
    HRESULT err = CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
    assert(err == S_OK);
    thread->run(thread->arg);
    CoUninitialize();
    return 0;
}
#else
static void assert_no_err(int err) {
    assert(!err);
}

static void *run_pthread(void *userdata) {
    struct SoundIoOsThread *thread = (struct SoundIoOsThread *)userdata;
    thread->run(thread->arg);
    return NULL;
}
#endif

int soundio_os_thread_create(
        void (*run)(void *arg), void *arg,
        void (*emit_rtprio_warning)(void),
        struct SoundIoOsThread ** out_thread)
{
    *out_thread = NULL;

    struct SoundIoOsThread *thread = ALLOCATE(struct SoundIoOsThread, 1);
    if (!thread) {
        soundio_os_thread_destroy(thread);
        return SoundIoErrorNoMem;
    }

    thread->run = run;
    thread->arg = arg;

#if defined(SOUNDIO_OS_WINDOWS)
    thread->handle = CreateThread(NULL, 0, run_win32_thread, thread, 0, &thread->id);
    if (!thread->handle) {
        soundio_os_thread_destroy(thread);
        return SoundIoErrorSystemResources;
    }
    if (emit_rtprio_warning) {
        if (!SetThreadPriority(thread->handle, THREAD_PRIORITY_TIME_CRITICAL)) {
            emit_rtprio_warning();
        }
    }
#else
    int err;
    if ((err = pthread_attr_init(&thread->attr))) {
        soundio_os_thread_destroy(thread);
        return SoundIoErrorNoMem;
    }
    thread->attr_init = true;
    
    if (emit_rtprio_warning) {
        int max_priority = sched_get_priority_max(SCHED_FIFO);
        if (max_priority == -1) {
            soundio_os_thread_destroy(thread);
            return SoundIoErrorSystemResources;
        }

        if ((err = pthread_attr_setschedpolicy(&thread->attr, SCHED_FIFO))) {
            soundio_os_thread_destroy(thread);
            return SoundIoErrorSystemResources;
        }

        struct sched_param param;
        param.sched_priority = max_priority;
        if ((err = pthread_attr_setschedparam(&thread->attr, &param))) {
            soundio_os_thread_destroy(thread);
            return SoundIoErrorSystemResources;
        }

    }

    if ((err = pthread_create(&thread->id, &thread->attr, run_pthread, thread))) {
        if (err == EPERM && emit_rtprio_warning) {
            emit_rtprio_warning();
            err = pthread_create(&thread->id, NULL, run_pthread, thread);
        }
        if (err) {
            soundio_os_thread_destroy(thread);
            return SoundIoErrorNoMem;
        }
    }
    thread->running = true;
#endif

    *out_thread = thread;
    return 0;
}

void soundio_os_thread_destroy(struct SoundIoOsThread *thread) {
    if (!thread)
        return;

#if defined(SOUNDIO_OS_WINDOWS)
    if (thread->handle) {
        DWORD err = WaitForSingleObject(thread->handle, INFINITE);
        assert(err != WAIT_FAILED);
        BOOL ok = CloseHandle(thread->handle);
        assert(ok);
    }
#else

    if (thread->running) {
        assert_no_err(pthread_join(thread->id, NULL));
    }

    if (thread->attr_init) {
        assert_no_err(pthread_attr_destroy(&thread->attr));
    }
#endif

    free(thread);
}

struct SoundIoOsMutex *soundio_os_mutex_create(void) {
    struct SoundIoOsMutex *mutex = ALLOCATE(struct SoundIoOsMutex, 1);
    if (!mutex) {
        soundio_os_mutex_destroy(mutex);
        return NULL;
    }

#if defined(SOUNDIO_OS_WINDOWS)
    InitializeCriticalSection(&mutex->id);
#else
    int err;
    if ((err = pthread_mutex_init(&mutex->id, NULL))) {
        soundio_os_mutex_destroy(mutex);
        return NULL;
    }
    mutex->id_init = true;
#endif

    return mutex;
}

void soundio_os_mutex_destroy(struct SoundIoOsMutex *mutex) {
    if (!mutex)
        return;

#if defined(SOUNDIO_OS_WINDOWS)
    DeleteCriticalSection(&mutex->id);
#else
    if (mutex->id_init) {
        assert_no_err(pthread_mutex_destroy(&mutex->id));
    }
#endif

    free(mutex);
}

void soundio_os_mutex_lock(struct SoundIoOsMutex *mutex) {
#if defined(SOUNDIO_OS_WINDOWS)
    EnterCriticalSection(&mutex->id);
#else
    assert_no_err(pthread_mutex_lock(&mutex->id));
#endif
}

void soundio_os_mutex_unlock(struct SoundIoOsMutex *mutex) {
#if defined(SOUNDIO_OS_WINDOWS)
    LeaveCriticalSection(&mutex->id);
#else
    assert_no_err(pthread_mutex_unlock(&mutex->id));
#endif
}

struct SoundIoOsCond * soundio_os_cond_create(void) {
    struct SoundIoOsCond *cond = ALLOCATE(struct SoundIoOsCond, 1);

    if (!cond) {
        soundio_os_cond_destroy(cond);
        return NULL;
    }

#if defined(SOUNDIO_OS_WINDOWS)
    InitializeConditionVariable(&cond->id);
    InitializeCriticalSection(&cond->default_cs_id);
#elif defined(SOUNDIO_OS_KQUEUE)
    cond->kq_id = kqueue();
    if (cond->kq_id == -1)
        return NULL;
#else
    if (pthread_condattr_init(&cond->attr)) {
        soundio_os_cond_destroy(cond);
        return NULL;
    }
    cond->attr_init = true;

    if (pthread_condattr_setclock(&cond->attr, CLOCK_MONOTONIC)) {
        soundio_os_cond_destroy(cond);
        return NULL;
    }

    if (pthread_cond_init(&cond->id, &cond->attr)) {
        soundio_os_cond_destroy(cond);
        return NULL;
    }
    cond->id_init = true;

    if ((pthread_mutex_init(&cond->default_mutex_id, NULL))) {
        soundio_os_cond_destroy(cond);
        return NULL;
    }
    cond->default_mutex_init = true;
#endif

    return cond;
}

void soundio_os_cond_destroy(struct SoundIoOsCond *cond) {
    if (!cond)
        return;

#if defined(SOUNDIO_OS_WINDOWS)
    DeleteCriticalSection(&cond->default_cs_id);
#elif defined(SOUNDIO_OS_KQUEUE)
    close(cond->kq_id);
#else
    if (cond->id_init) {
        assert_no_err(pthread_cond_destroy(&cond->id));
    }

    if (cond->attr_init) {
        assert_no_err(pthread_condattr_destroy(&cond->attr));
    }
    if (cond->default_mutex_init) {
        assert_no_err(pthread_mutex_destroy(&cond->default_mutex_id));
    }
#endif

    free(cond);
}

void soundio_os_cond_signal(struct SoundIoOsCond *cond,
        struct SoundIoOsMutex *locked_mutex)
{
#if defined(SOUNDIO_OS_WINDOWS)
    if (locked_mutex) {
        WakeConditionVariable(&cond->id);
    } else {
        EnterCriticalSection(&cond->default_cs_id);
        WakeConditionVariable(&cond->id);
        LeaveCriticalSection(&cond->default_cs_id);
    }
#elif defined(SOUNDIO_OS_KQUEUE)
    struct kevent kev;
    struct timespec timeout = { 0, 0 };

    memset(&kev, 0, sizeof(kev));
    kev.ident = notify_ident;
    kev.filter = EVFILT_USER;
    kev.fflags = NOTE_TRIGGER;

    if (kevent(cond->kq_id, &kev, 1, NULL, 0, &timeout) == -1) {
        if (errno == EINTR)
            return;
        if (errno == ENOENT)
            return;
        assert(0); // kevent signal error
    }
#else
    if (locked_mutex) {
        assert_no_err(pthread_cond_signal(&cond->id));
    } else {
        assert_no_err(pthread_mutex_lock(&cond->default_mutex_id));
        assert_no_err(pthread_cond_signal(&cond->id));
        assert_no_err(pthread_mutex_unlock(&cond->default_mutex_id));
    }
#endif
}

void soundio_os_cond_timed_wait(struct SoundIoOsCond *cond,
        struct SoundIoOsMutex *locked_mutex, double seconds)
{
#if defined(SOUNDIO_OS_WINDOWS)
    CRITICAL_SECTION *target_cs;
    if (locked_mutex) {
        target_cs = &locked_mutex->id;
    } else {
        target_cs = &cond->default_cs_id;
        EnterCriticalSection(&cond->default_cs_id);
    }
    DWORD ms = seconds * 1000.0;
    SleepConditionVariableCS(&cond->id, target_cs, ms);
    if (!locked_mutex)
        LeaveCriticalSection(&cond->default_cs_id);
#elif defined(SOUNDIO_OS_KQUEUE)
    struct kevent kev;
    struct kevent out_kev;

    if (locked_mutex)
        assert_no_err(pthread_mutex_unlock(&locked_mutex->id));

    memset(&kev, 0, sizeof(kev));
    kev.ident = notify_ident;
    kev.filter = EVFILT_USER;
    kev.flags = EV_ADD | EV_CLEAR;

    // this time is relative
    struct timespec timeout;
    timeout.tv_nsec = (seconds * 1000000000L);
    timeout.tv_sec  = timeout.tv_nsec / 1000000000L;
    timeout.tv_nsec = timeout.tv_nsec % 1000000000L;

    if (kevent(cond->kq_id, &kev, 1, &out_kev, 1, &timeout) == -1) {
        if (errno == EINTR)
            return;
        assert(0); // kevent wait error
    }
    if (locked_mutex)
        assert_no_err(pthread_mutex_lock(&locked_mutex->id));
#else
    pthread_mutex_t *target_mutex;
    if (locked_mutex) {
        target_mutex = &locked_mutex->id;
    } else {
        target_mutex = &cond->default_mutex_id;
        assert_no_err(pthread_mutex_lock(target_mutex));
    }
    // this time is absolute
    struct timespec tms;
    clock_gettime(CLOCK_MONOTONIC, &tms);
    tms.tv_nsec += (seconds * 1000000000L);
    tms.tv_sec += tms.tv_nsec / 1000000000L;
    tms.tv_nsec = tms.tv_nsec % 1000000000L;
    int err;
    if ((err = pthread_cond_timedwait(&cond->id, target_mutex, &tms))) {
        assert(err != EPERM);
        assert(err != EINVAL);
    }
    if (!locked_mutex)
        assert_no_err(pthread_mutex_unlock(target_mutex));
#endif
}

void soundio_os_cond_wait(struct SoundIoOsCond *cond,
        struct SoundIoOsMutex *locked_mutex)
{
#if defined(SOUNDIO_OS_WINDOWS)
    CRITICAL_SECTION *target_cs;
    if (locked_mutex) {
        target_cs = &locked_mutex->id;
    } else {
        target_cs = &cond->default_cs_id;
        EnterCriticalSection(&cond->default_cs_id);
    }
    SleepConditionVariableCS(&cond->id, target_cs, INFINITE);
    if (!locked_mutex)
        LeaveCriticalSection(&cond->default_cs_id);
#elif defined(SOUNDIO_OS_KQUEUE)
    struct kevent kev;
    struct kevent out_kev;

    if (locked_mutex)
        assert_no_err(pthread_mutex_unlock(&locked_mutex->id));

    memset(&kev, 0, sizeof(kev));
    kev.ident = notify_ident;
    kev.filter = EVFILT_USER;
    kev.flags = EV_ADD | EV_CLEAR;

    if (kevent(cond->kq_id, &kev, 1, &out_kev, 1, NULL) == -1) {
        if (errno == EINTR)
            return;
        assert(0); // kevent wait error
    }
    if (locked_mutex)
        assert_no_err(pthread_mutex_lock(&locked_mutex->id));
#else
    pthread_mutex_t *target_mutex;
    if (locked_mutex) {
        target_mutex = &locked_mutex->id;
    } else {
        target_mutex = &cond->default_mutex_id;
        assert_no_err(pthread_mutex_lock(&cond->default_mutex_id));
    }
    int err;
    if ((err = pthread_cond_wait(&cond->id, target_mutex))) {
        assert(err != EPERM);
        assert(err != EINVAL);
    }
    if (!locked_mutex)
        assert_no_err(pthread_mutex_unlock(&cond->default_mutex_id));
#endif
}

static int internal_init(void) {
#if defined(SOUNDIO_OS_WINDOWS)
    unsigned __int64 frequency;
    if (QueryPerformanceFrequency((LARGE_INTEGER*) &frequency)) {
        win32_time_resolution = 1.0 / (double) frequency;
    } else {
        return SoundIoErrorSystemResources;
    }
    GetSystemInfo(&win32_system_info);
    page_size = win32_system_info.dwAllocationGranularity;
#else
    page_size = sysconf(_SC_PAGESIZE);
#if defined(__MACH__)
    host_get_clock_service(mach_host_self(), SYSTEM_CLOCK, &cclock);
#endif
#endif
    return 0;
}

int soundio_os_init(void) {
    int err;
#if defined(SOUNDIO_OS_WINDOWS)
    PVOID lpContext;
    BOOL pending;

    if (!InitOnceBeginInitialize(&win32_init_once, INIT_ONCE_ASYNC, &pending, &lpContext))
        return SoundIoErrorSystemResources;

    if (!pending)
        return 0;

    if ((err = internal_init()))
        return err;

    if (!InitOnceComplete(&win32_init_once, INIT_ONCE_ASYNC, NULL))
        return SoundIoErrorSystemResources;
#else
    assert_no_err(pthread_mutex_lock(&init_mutex));
    if (initialized) {
        assert_no_err(pthread_mutex_unlock(&init_mutex));
        return 0;
    }
    initialized = true;
    if ((err = internal_init()))
        return err;
    assert_no_err(pthread_mutex_unlock(&init_mutex));
#endif

    return 0;
}

int soundio_os_page_size(void) {
    return page_size;
}

static inline size_t ceil_dbl_to_size_t(double x) {
    const double truncation = (size_t)x;
    return truncation + (truncation < x);
}

int soundio_os_init_mirrored_memory(struct SoundIoOsMirroredMemory *mem, size_t requested_capacity) {
    size_t actual_capacity = ceil_dbl_to_size_t(requested_capacity / (double)page_size) * page_size;

#if defined(SOUNDIO_OS_WINDOWS)
    BOOL ok;
    HANDLE hMapFile = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, actual_capacity * 2, NULL);
    if (!hMapFile)
        return SoundIoErrorNoMem;

    for (;;) {
        // find a free address space with the correct size
        char *address = (char*)MapViewOfFile(hMapFile, FILE_MAP_ALL_ACCESS, 0, 0, actual_capacity * 2);
        if (!address) {
            ok = CloseHandle(hMapFile);
            assert(ok);
            return SoundIoErrorNoMem;
        }

        // found a big enough address space. hopefully it will remain free
        // while we map to it. if not, we'll try again.
        ok = UnmapViewOfFile(address);
        assert(ok);

        char *addr1 = (char*)MapViewOfFileEx(hMapFile, FILE_MAP_ALL_ACCESS, 0, 0, actual_capacity, address);
        if (addr1 != address) {
            DWORD err = GetLastError();
            if (err == ERROR_INVALID_ADDRESS) {
                continue;
            } else {
                ok = CloseHandle(hMapFile);
                assert(ok);
                return SoundIoErrorNoMem;
            }
        }

        char *addr2 = (char*)MapViewOfFileEx(hMapFile, FILE_MAP_WRITE, 0, 0,
                actual_capacity, address + actual_capacity);
        if (addr2 != address + actual_capacity) {
            ok = UnmapViewOfFile(addr1);
            assert(ok);

            DWORD err = GetLastError();
            if (err == ERROR_INVALID_ADDRESS) {
                continue;
            } else {
                ok = CloseHandle(hMapFile);
                assert(ok);
                return SoundIoErrorNoMem;
            }
        }

        mem->priv = hMapFile;
        mem->address = address;
        break;
    }
#else
    char shm_path[] = "/dev/shm/soundio-XXXXXX";
    char tmp_path[] = "/tmp/soundio-XXXXXX";
    char *chosen_path;

    int fd = mkstemp(shm_path);
    if (fd < 0) {
        fd = mkstemp(tmp_path);
        if (fd < 0) {
            return SoundIoErrorSystemResources;
        } else {
            chosen_path = tmp_path;
        }
    } else {
        chosen_path = shm_path;
    }

    if (unlink(chosen_path)) {
        close(fd);
        return SoundIoErrorSystemResources;
    }

    if (ftruncate(fd, actual_capacity)) {
        close(fd);
        return SoundIoErrorSystemResources;
    }

    char *address = (char*)mmap(NULL, actual_capacity * 2, PROT_NONE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    if (address == MAP_FAILED) {
        close(fd);
        return SoundIoErrorNoMem;
    }

    char *other_address = (char*)mmap(address, actual_capacity, PROT_READ|PROT_WRITE,
            MAP_FIXED|MAP_SHARED, fd, 0);
    if (other_address != address) {
        munmap(address, 2 * actual_capacity);
        close(fd);
        return SoundIoErrorNoMem;
    }

    other_address = (char*)mmap(address + actual_capacity, actual_capacity,
            PROT_READ|PROT_WRITE, MAP_FIXED|MAP_SHARED, fd, 0);
    if (other_address != address + actual_capacity) {
        munmap(address, 2 * actual_capacity);
        close(fd);
        return SoundIoErrorNoMem;
    }

    mem->address = address;

    if (close(fd))
        return SoundIoErrorSystemResources;
#endif

    mem->capacity = actual_capacity;
    return 0;
}

void soundio_os_deinit_mirrored_memory(struct SoundIoOsMirroredMemory *mem) {
    if (!mem->address)
        return;
#if defined(SOUNDIO_OS_WINDOWS)
    BOOL ok;
    ok = UnmapViewOfFile(mem->address);
    assert(ok);
    ok = UnmapViewOfFile(mem->address + mem->capacity);
    assert(ok);
    ok = CloseHandle((HANDLE)mem->priv);
    assert(ok);
#else
    int err = munmap(mem->address, 2 * mem->capacity);
    assert(!err);
#endif
    mem->address = NULL;
}
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

// (amalg) #include "ring_buffer.h"
// (amalg) #include "soundio_private.h"
// (amalg) #include "util.h"

#include <stdlib.h>

struct SoundIoRingBuffer *soundio_ring_buffer_create(struct SoundIo *soundio, int requested_capacity) {
    struct SoundIoRingBuffer *rb = ALLOCATE(struct SoundIoRingBuffer, 1);

    assert(requested_capacity > 0);

    if (!rb) {
        soundio_ring_buffer_destroy(rb);
        return NULL;
    }

    if (soundio_ring_buffer_init(rb, requested_capacity)) {
        soundio_ring_buffer_destroy(rb);
        return NULL;
    }

    return rb;
}

void soundio_ring_buffer_destroy(struct SoundIoRingBuffer *rb) {
    if (!rb)
        return;

    soundio_ring_buffer_deinit(rb);

    free(rb);
}

int soundio_ring_buffer_capacity(struct SoundIoRingBuffer *rb) {
    return rb->capacity;
}

char *soundio_ring_buffer_write_ptr(struct SoundIoRingBuffer *rb) {
    long write_offset = SOUNDIO_ATOMIC_LOAD(rb->write_offset);
    return rb->mem.address + (write_offset % rb->capacity);
}

void soundio_ring_buffer_advance_write_ptr(struct SoundIoRingBuffer *rb, int count) {
    SOUNDIO_ATOMIC_FETCH_ADD(rb->write_offset, count);
    assert(soundio_ring_buffer_fill_count(rb) >= 0);
}

char *soundio_ring_buffer_read_ptr(struct SoundIoRingBuffer *rb) {
    long read_offset = SOUNDIO_ATOMIC_LOAD(rb->read_offset);
    return rb->mem.address + (read_offset % rb->capacity);
}

void soundio_ring_buffer_advance_read_ptr(struct SoundIoRingBuffer *rb, int count) {
    SOUNDIO_ATOMIC_FETCH_ADD(rb->read_offset, count);
    assert(soundio_ring_buffer_fill_count(rb) >= 0);
}

int soundio_ring_buffer_fill_count(struct SoundIoRingBuffer *rb) {
    // Whichever offset we load first might have a smaller value. So we load
    // the read_offset first.
    long read_offset = SOUNDIO_ATOMIC_LOAD(rb->read_offset);
    long write_offset = SOUNDIO_ATOMIC_LOAD(rb->write_offset);
    int count = write_offset - read_offset;
    assert(count >= 0);
    assert(count <= rb->capacity);
    return count;
}

int soundio_ring_buffer_free_count(struct SoundIoRingBuffer *rb) {
    return rb->capacity - soundio_ring_buffer_fill_count(rb);
}

void soundio_ring_buffer_clear(struct SoundIoRingBuffer *rb) {
    long read_offset = SOUNDIO_ATOMIC_LOAD(rb->read_offset);
    SOUNDIO_ATOMIC_STORE(rb->write_offset, read_offset);
}

int soundio_ring_buffer_init(struct SoundIoRingBuffer *rb, int requested_capacity) {
    int err;
    if ((err = soundio_os_init_mirrored_memory(&rb->mem, requested_capacity)))
        return err;
    SOUNDIO_ATOMIC_STORE(rb->write_offset, 0);
    SOUNDIO_ATOMIC_STORE(rb->read_offset, 0);
    rb->capacity = rb->mem.capacity;

    return 0;
}

void soundio_ring_buffer_deinit(struct SoundIoRingBuffer *rb) {
    soundio_os_deinit_mirrored_memory(&rb->mem);
}
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

// (amalg) #include "util.h"

void soundio_panic(const char *format, ...) {
    va_list ap;
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    abort();
}

char *soundio_alloc_sprintf(int *len, const char *format, ...) {
    va_list ap, ap2;
    va_start(ap, format);
    va_copy(ap2, ap);

    int len1 = vsnprintf(NULL, 0, format, ap);
    assert(len1 >= 0);

    size_t required_size = len1 + 1;
    char *mem = ALLOCATE(char, required_size);
    if (!mem)
        return NULL;

    int len2 = vsnprintf(mem, required_size, format, ap2);
    assert(len2 == len1);

    va_end(ap2);
    va_end(ap);

    if (len)
        *len = len1;
    return mem;
}
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

// (amalg) #include "dummy.h"
// (amalg) #include "soundio_private.h"

#include <stdio.h>
#include <string.h>

static void playback_thread_run(void *arg) {
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)arg;
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoOutStreamDummy *osd = &os->backend_data.dummy;

    int fill_bytes = soundio_ring_buffer_fill_count(&osd->ring_buffer);
    int free_bytes = soundio_ring_buffer_capacity(&osd->ring_buffer) - fill_bytes;
    int free_frames = free_bytes / outstream->bytes_per_frame;
    osd->frames_left = free_frames;
    if (free_frames > 0)
        outstream->write_callback(outstream, 0, free_frames);
    double start_time = soundio_os_get_time();
    long frames_consumed = 0;

    while (SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osd->abort_flag)) {
        double now = soundio_os_get_time();
        double time_passed = now - start_time;
        double next_period = start_time +
            ceil_dbl(time_passed / osd->period_duration) * osd->period_duration;
        double relative_time = next_period - now;
        soundio_os_cond_timed_wait(osd->cond, NULL, relative_time);
        if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osd->clear_buffer_flag)) {
            soundio_ring_buffer_clear(&osd->ring_buffer);
            int free_bytes = soundio_ring_buffer_capacity(&osd->ring_buffer);
            int free_frames = free_bytes / outstream->bytes_per_frame;
            osd->frames_left = free_frames;
            if (free_frames > 0)
                outstream->write_callback(outstream, 0, free_frames);
            frames_consumed = 0;
            start_time = soundio_os_get_time();
            continue;
        }

        if (SOUNDIO_ATOMIC_LOAD(osd->pause_requested)) {
            start_time = now;
            frames_consumed = 0;
            continue;
        }

        int fill_bytes = soundio_ring_buffer_fill_count(&osd->ring_buffer);
        int fill_frames = fill_bytes / outstream->bytes_per_frame;
        int free_bytes = soundio_ring_buffer_capacity(&osd->ring_buffer) - fill_bytes;
        int free_frames = free_bytes / outstream->bytes_per_frame;

        double total_time = soundio_os_get_time() - start_time;
        long total_frames = total_time * outstream->sample_rate;
        int frames_to_kill = total_frames - frames_consumed;
        int read_count = soundio_int_min(frames_to_kill, fill_frames);
        int byte_count = read_count * outstream->bytes_per_frame;
        soundio_ring_buffer_advance_read_ptr(&osd->ring_buffer, byte_count);
        frames_consumed += read_count;

        if (frames_to_kill > fill_frames) {
            outstream->underflow_callback(outstream);
            osd->frames_left = free_frames;
            if (free_frames > 0)
                outstream->write_callback(outstream, 0, free_frames);
            frames_consumed = 0;
            start_time = soundio_os_get_time();
        } else if (free_frames > 0) {
            osd->frames_left = free_frames;
            outstream->write_callback(outstream, 0, free_frames);
        }
    }
}

static void capture_thread_run(void *arg) {
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *)arg;
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoInStreamDummy *isd = &is->backend_data.dummy;

    long frames_consumed = 0;
    double start_time = soundio_os_get_time();
    while (SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(isd->abort_flag)) {
        double now = soundio_os_get_time();
        double time_passed = now - start_time;
        double next_period = start_time +
            ceil_dbl(time_passed / isd->period_duration) * isd->period_duration;
        double relative_time = next_period - now;
        soundio_os_cond_timed_wait(isd->cond, NULL, relative_time);

        if (SOUNDIO_ATOMIC_LOAD(isd->pause_requested)) {
            start_time = now;
            frames_consumed = 0;
            continue;
        }

        int fill_bytes = soundio_ring_buffer_fill_count(&isd->ring_buffer);
        int free_bytes = soundio_ring_buffer_capacity(&isd->ring_buffer) - fill_bytes;
        int fill_frames = fill_bytes / instream->bytes_per_frame;
        int free_frames = free_bytes / instream->bytes_per_frame;

        double total_time = soundio_os_get_time() - start_time;
        long total_frames = total_time * instream->sample_rate;
        int frames_to_kill = total_frames - frames_consumed;
        int write_count = soundio_int_min(frames_to_kill, free_frames);
        int byte_count = write_count * instream->bytes_per_frame;
        soundio_ring_buffer_advance_write_ptr(&isd->ring_buffer, byte_count);
        frames_consumed += write_count;

        if (frames_to_kill > free_frames) {
            instream->overflow_callback(instream);
            frames_consumed = 0;
            start_time = soundio_os_get_time();
        }
        if (fill_frames > 0) {
            isd->frames_left = fill_frames;
            instream->read_callback(instream, 0, fill_frames);
        }
    }
}

static void destroy_dummy(struct SoundIoPrivate *si) {
    struct SoundIoDummy *sid = &si->backend_data.dummy;

    if (sid->cond)
        soundio_os_cond_destroy(sid->cond);

    if (sid->mutex)
        soundio_os_mutex_destroy(sid->mutex);
}

static void flush_events_dummy(struct SoundIoPrivate *si) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoDummy *sid = &si->backend_data.dummy;
    if (sid->devices_emitted)
        return;
    sid->devices_emitted = true;
    soundio->on_devices_change(soundio);
}

static void wait_events_dummy(struct SoundIoPrivate *si) {
    struct SoundIoDummy *sid = &si->backend_data.dummy;
    flush_events_dummy(si);
    soundio_os_cond_wait(sid->cond, NULL);
}

static void wakeup_dummy(struct SoundIoPrivate *si) {
    struct SoundIoDummy *sid = &si->backend_data.dummy;
    soundio_os_cond_signal(sid->cond, NULL);
}

static void force_device_scan_dummy(struct SoundIoPrivate *si) {
    // nothing to do; dummy devices never change
}

static void outstream_destroy_dummy(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamDummy *osd = &os->backend_data.dummy;

    if (osd->thread) {
        SOUNDIO_ATOMIC_FLAG_CLEAR(osd->abort_flag);
        soundio_os_cond_signal(osd->cond, NULL);
        soundio_os_thread_destroy(osd->thread);
        osd->thread = NULL;
    }
    soundio_os_cond_destroy(osd->cond);
    osd->cond = NULL;

    soundio_ring_buffer_deinit(&osd->ring_buffer);
}

static int outstream_open_dummy(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamDummy *osd = &os->backend_data.dummy;
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoDevice *device = outstream->device;

    SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osd->clear_buffer_flag);
    SOUNDIO_ATOMIC_STORE(osd->pause_requested, false);

    if (outstream->software_latency == 0.0) {
        outstream->software_latency = soundio_double_clamp(
                device->software_latency_min, 1.0, device->software_latency_max);
    }

    osd->period_duration = outstream->software_latency / 2.0;

    int err;
    int buffer_size = outstream->bytes_per_frame * outstream->sample_rate * outstream->software_latency;
    if ((err = soundio_ring_buffer_init(&osd->ring_buffer, buffer_size))) {
        outstream_destroy_dummy(si, os);
        return err;
    }
    int actual_capacity = soundio_ring_buffer_capacity(&osd->ring_buffer);
    osd->buffer_frame_count = actual_capacity / outstream->bytes_per_frame;
    outstream->software_latency = osd->buffer_frame_count / (double) outstream->sample_rate;

    osd->cond = soundio_os_cond_create();
    if (!osd->cond) {
        outstream_destroy_dummy(si, os);
        return SoundIoErrorNoMem;
    }

    return 0;
}

static int outstream_pause_dummy(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os, bool pause) {
    struct SoundIoOutStreamDummy *osd = &os->backend_data.dummy;
    SOUNDIO_ATOMIC_STORE(osd->pause_requested, pause);
    return 0;
}

static int outstream_start_dummy(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamDummy *osd = &os->backend_data.dummy;
    struct SoundIo *soundio = &si->pub;
    assert(!osd->thread);
    SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osd->abort_flag);
    int err;
    if ((err = soundio_os_thread_create(playback_thread_run, os,
                    soundio->emit_rtprio_warning, &osd->thread)))
    {
        return err;
    }
    return 0;
}

static int outstream_begin_write_dummy(struct SoundIoPrivate *si,
        struct SoundIoOutStreamPrivate *os, struct SoundIoChannelArea **out_areas, int *frame_count)
{
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoOutStreamDummy *osd = &os->backend_data.dummy;

    if (*frame_count > osd->frames_left)
        return SoundIoErrorInvalid;

    char *write_ptr = soundio_ring_buffer_write_ptr(&osd->ring_buffer);
    for (int ch = 0; ch < outstream->layout.channel_count; ch += 1) {
        osd->areas[ch].ptr = write_ptr + outstream->bytes_per_sample * ch;
        osd->areas[ch].step = outstream->bytes_per_frame;
    }

    osd->write_frame_count = *frame_count;
    *out_areas = osd->areas;
    return 0;
}

static int outstream_end_write_dummy(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamDummy *osd = &os->backend_data.dummy;
    struct SoundIoOutStream *outstream = &os->pub;
    int byte_count = osd->write_frame_count * outstream->bytes_per_frame;
    soundio_ring_buffer_advance_write_ptr(&osd->ring_buffer, byte_count);
    osd->frames_left -= osd->write_frame_count;
    return 0;
}

static int outstream_clear_buffer_dummy(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamDummy *osd = &os->backend_data.dummy;
    SOUNDIO_ATOMIC_FLAG_CLEAR(osd->clear_buffer_flag);
    soundio_os_cond_signal(osd->cond, NULL);
    return 0;
}

static int outstream_get_latency_dummy(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os, double *out_latency) {
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoOutStreamDummy *osd = &os->backend_data.dummy;
    int fill_bytes = soundio_ring_buffer_fill_count(&osd->ring_buffer);

    *out_latency = (fill_bytes / outstream->bytes_per_frame) / (double)outstream->sample_rate;
    return 0;
}

static void instream_destroy_dummy(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamDummy *isd = &is->backend_data.dummy;

    if (isd->thread) {
        SOUNDIO_ATOMIC_FLAG_CLEAR(isd->abort_flag);
        soundio_os_cond_signal(isd->cond, NULL);
        soundio_os_thread_destroy(isd->thread);
        isd->thread = NULL;
    }
    soundio_os_cond_destroy(isd->cond);
    isd->cond = NULL;

    soundio_ring_buffer_deinit(&isd->ring_buffer);
}

static int instream_open_dummy(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamDummy *isd = &is->backend_data.dummy;
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoDevice *device = instream->device;

    SOUNDIO_ATOMIC_STORE(isd->pause_requested, false);

    if (instream->software_latency == 0.0) {
        instream->software_latency = soundio_double_clamp(
                device->software_latency_min, 1.0, device->software_latency_max);
    }

    isd->period_duration = instream->software_latency;

    double target_buffer_duration = isd->period_duration * 4.0;

    int err;
    int buffer_size = instream->bytes_per_frame * instream->sample_rate * target_buffer_duration;
    if ((err = soundio_ring_buffer_init(&isd->ring_buffer, buffer_size))) {
        instream_destroy_dummy(si, is);
        return err;
    }

    int actual_capacity = soundio_ring_buffer_capacity(&isd->ring_buffer);
    isd->buffer_frame_count = actual_capacity / instream->bytes_per_frame;

    isd->cond = soundio_os_cond_create();
    if (!isd->cond) {
        instream_destroy_dummy(si, is);
        return SoundIoErrorNoMem;
    }

    return 0;
}

static int instream_pause_dummy(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is, bool pause) {
    struct SoundIoInStreamDummy *isd = &is->backend_data.dummy;
    SOUNDIO_ATOMIC_STORE(isd->pause_requested, pause);
    return 0;
}

static int instream_start_dummy(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamDummy *isd = &is->backend_data.dummy;
    struct SoundIo *soundio = &si->pub;
    assert(!isd->thread);
    SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(isd->abort_flag);
    int err;
    if ((err = soundio_os_thread_create(capture_thread_run, is,
                    soundio->emit_rtprio_warning, &isd->thread)))
    {
        return err;
    }
    return 0;
}

static int instream_begin_read_dummy(struct SoundIoPrivate *si,
        struct SoundIoInStreamPrivate *is, struct SoundIoChannelArea **out_areas, int *frame_count)
{
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoInStreamDummy *isd = &is->backend_data.dummy;

    assert(*frame_count <= isd->frames_left);

    char *read_ptr = soundio_ring_buffer_read_ptr(&isd->ring_buffer);
    for (int ch = 0; ch < instream->layout.channel_count; ch += 1) {
        isd->areas[ch].ptr = read_ptr + instream->bytes_per_sample * ch;
        isd->areas[ch].step = instream->bytes_per_frame;
    }

    isd->read_frame_count = *frame_count;
    *out_areas = isd->areas;

    return 0;
}

static int instream_end_read_dummy(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamDummy *isd = &is->backend_data.dummy;
    struct SoundIoInStream *instream = &is->pub;
    int byte_count = isd->read_frame_count * instream->bytes_per_frame;
    soundio_ring_buffer_advance_read_ptr(&isd->ring_buffer, byte_count);
    isd->frames_left -= isd->read_frame_count;
    return 0;
}

static int instream_get_latency_dummy(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is, double *out_latency) {
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoInStreamDummy *osd = &is->backend_data.dummy;
    int fill_bytes = soundio_ring_buffer_fill_count(&osd->ring_buffer);

    *out_latency = (fill_bytes / instream->bytes_per_frame) / (double)instream->sample_rate;
    return 0;
}

static int set_all_device_formats(struct SoundIoDevice *device) {
    device->format_count = 18;
    device->formats = ALLOCATE(enum SoundIoFormat, device->format_count);
    if (!device->formats)
        return SoundIoErrorNoMem;

    device->formats[0] = SoundIoFormatFloat32NE;
    device->formats[1] = SoundIoFormatFloat32FE;
    device->formats[2] = SoundIoFormatS32NE;
    device->formats[3] = SoundIoFormatS32FE;
    device->formats[4] = SoundIoFormatU32NE;
    device->formats[5] = SoundIoFormatU32FE;
    device->formats[6] = SoundIoFormatS24NE;
    device->formats[7] = SoundIoFormatS24FE;
    device->formats[8] = SoundIoFormatU24NE;
    device->formats[9] = SoundIoFormatU24FE;
    device->formats[10] = SoundIoFormatFloat64NE;
    device->formats[11] = SoundIoFormatFloat64FE;
    device->formats[12] = SoundIoFormatS16NE;
    device->formats[13] = SoundIoFormatS16FE;
    device->formats[14] = SoundIoFormatU16NE;
    device->formats[15] = SoundIoFormatU16FE;
    device->formats[16] = SoundIoFormatS8;
    device->formats[17] = SoundIoFormatU8;

    return 0;
}

static void set_all_device_sample_rates(struct SoundIoDevice *device) {
    struct SoundIoDevicePrivate *dev = (struct SoundIoDevicePrivate *)device;
    device->sample_rate_count = 1;
    device->sample_rates = &dev->prealloc_sample_rate_range;
    device->sample_rates[0].min = SOUNDIO_MIN_SAMPLE_RATE;
    device->sample_rates[0].max = SOUNDIO_MAX_SAMPLE_RATE;
}

static int set_all_device_channel_layouts(struct SoundIoDevice *device) {
    device->layout_count = soundio_channel_layout_builtin_count();
    device->layouts = ALLOCATE(struct SoundIoChannelLayout, device->layout_count);
    if (!device->layouts)
        return SoundIoErrorNoMem;
    for (int i = 0; i < device->layout_count; i += 1)
        device->layouts[i] = *soundio_channel_layout_get_builtin(i);
    return 0;
}

int soundio_dummy_init(struct SoundIoPrivate *si) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoDummy *sid = &si->backend_data.dummy;

    sid->mutex = soundio_os_mutex_create();
    if (!sid->mutex) {
        destroy_dummy(si);
        return SoundIoErrorNoMem;
    }

    sid->cond = soundio_os_cond_create();
    if (!sid->cond) {
        destroy_dummy(si);
        return SoundIoErrorNoMem;
    }

    assert(!si->safe_devices_info);
    si->safe_devices_info = ALLOCATE(struct SoundIoDevicesInfo, 1);
    if (!si->safe_devices_info) {
        destroy_dummy(si);
        return SoundIoErrorNoMem;
    }

    si->safe_devices_info->default_input_index = 0;
    si->safe_devices_info->default_output_index = 0;

    // create output device
    {
        struct SoundIoDevicePrivate *dev = ALLOCATE(struct SoundIoDevicePrivate, 1);
        if (!dev) {
            destroy_dummy(si);
            return SoundIoErrorNoMem;
        }
        struct SoundIoDevice *device = &dev->pub;

        device->ref_count = 1;
        device->soundio = soundio;
        device->id = strdup("dummy-out");
        device->name = strdup("Dummy Output Device");
        if (!device->id || !device->name) {
            soundio_device_unref(device);
            destroy_dummy(si);
            return SoundIoErrorNoMem;
        }

        int err;
        if ((err = set_all_device_channel_layouts(device))) {
            soundio_device_unref(device);
            destroy_dummy(si);
            return err;
        }
        if ((err = set_all_device_formats(device))) {
            soundio_device_unref(device);
            destroy_dummy(si);
            return err;
        }
        set_all_device_sample_rates(device);

        device->software_latency_current = 0.1;
        device->software_latency_min = 0.01;
        device->software_latency_max = 4.0;

        device->sample_rate_current = 48000;
        device->aim = SoundIoDeviceAimOutput;

        if (SoundIoListDevicePtr_append(&si->safe_devices_info->output_devices, device)) {
            soundio_device_unref(device);
            destroy_dummy(si);
            return SoundIoErrorNoMem;
        }
    }

    // create input device
    {
        struct SoundIoDevicePrivate *dev = ALLOCATE(struct SoundIoDevicePrivate, 1);
        if (!dev) {
            destroy_dummy(si);
            return SoundIoErrorNoMem;
        }
        struct SoundIoDevice *device = &dev->pub;

        device->ref_count = 1;
        device->soundio = soundio;
        device->id = strdup("dummy-in");
        device->name = strdup("Dummy Input Device");
        if (!device->id || !device->name) {
            soundio_device_unref(device);
            destroy_dummy(si);
            return SoundIoErrorNoMem;
        }

        int err;
        if ((err = set_all_device_channel_layouts(device))) {
            soundio_device_unref(device);
            destroy_dummy(si);
            return err;
        }

        if ((err = set_all_device_formats(device))) {
            soundio_device_unref(device);
            destroy_dummy(si);
            return err;
        }
        set_all_device_sample_rates(device);
        device->software_latency_current = 0.1;
        device->software_latency_min = 0.01;
        device->software_latency_max = 4.0;
        device->sample_rate_current = 48000;
        device->aim = SoundIoDeviceAimInput;

        if (SoundIoListDevicePtr_append(&si->safe_devices_info->input_devices, device)) {
            soundio_device_unref(device);
            destroy_dummy(si);
            return SoundIoErrorNoMem;
        }
    }


    si->destroy = destroy_dummy;
    si->flush_events = flush_events_dummy;
    si->wait_events = wait_events_dummy;
    si->wakeup = wakeup_dummy;
    si->force_device_scan = force_device_scan_dummy;

    si->outstream_open = outstream_open_dummy;
    si->outstream_destroy = outstream_destroy_dummy;
    si->outstream_start = outstream_start_dummy;
    si->outstream_begin_write = outstream_begin_write_dummy;
    si->outstream_end_write = outstream_end_write_dummy;
    si->outstream_clear_buffer = outstream_clear_buffer_dummy;
    si->outstream_pause = outstream_pause_dummy;
    si->outstream_get_latency = outstream_get_latency_dummy;

    si->instream_open = instream_open_dummy;
    si->instream_destroy = instream_destroy_dummy;
    si->instream_start = instream_start_dummy;
    si->instream_begin_read = instream_begin_read_dummy;
    si->instream_end_read = instream_end_read_dummy;
    si->instream_pause = instream_pause_dummy;
    si->instream_get_latency = instream_get_latency_dummy;

    return 0;
}
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

// (amalg) #include "soundio_private.h"
// (amalg) #include "util.h"
// (amalg) #include "os.h"
// (amalg) #include "config.h"

#include <string.h>
#include <assert.h>
#include <stdio.h>

static const enum SoundIoBackend available_backends[] = {
#ifdef SOUNDIO_HAVE_JACK
    SoundIoBackendJack,
#endif
#ifdef SOUNDIO_HAVE_PULSEAUDIO
    SoundIoBackendPulseAudio,
#endif
#ifdef SOUNDIO_HAVE_ALSA
    SoundIoBackendAlsa,
#endif
#ifdef SOUNDIO_HAVE_COREAUDIO
    SoundIoBackendCoreAudio,
#endif
#ifdef SOUNDIO_HAVE_WASAPI
    SoundIoBackendWasapi,
#endif
    SoundIoBackendDummy,
};

typedef int (*backend_init_t)(struct SoundIoPrivate *);
static backend_init_t backend_init_fns[] = {
    NULL, // None backend

#ifdef SOUNDIO_HAVE_JACK
    &soundio_jack_init,
#else
    NULL,
#endif

#ifdef SOUNDIO_HAVE_PULSEAUDIO
    &soundio_pulseaudio_init,
#else
    NULL,
#endif

#ifdef SOUNDIO_HAVE_ALSA
    &soundio_alsa_init,
#else
    NULL,
#endif

#ifdef SOUNDIO_HAVE_COREAUDIO
    &soundio_coreaudio_init,
#else
    NULL,
#endif

#ifdef SOUNDIO_HAVE_WASAPI
    soundio_wasapi_init,
#else
    NULL,
#endif

    &soundio_dummy_init,
};

SOUNDIO_MAKE_LIST_DEF(struct SoundIoDevice*, SoundIoListDevicePtr, SOUNDIO_LIST_NOT_STATIC)
SOUNDIO_MAKE_LIST_DEF(struct SoundIoSampleRateRange, SoundIoListSampleRateRange, SOUNDIO_LIST_NOT_STATIC)

const char *soundio_strerror(int error) {
    switch ((enum SoundIoError)error) {
        case SoundIoErrorNone: return "(no error)";
        case SoundIoErrorNoMem: return "out of memory";
        case SoundIoErrorInitAudioBackend: return "unable to initialize audio backend";
        case SoundIoErrorSystemResources: return "system resource not available";
        case SoundIoErrorOpeningDevice: return "unable to open device";
        case SoundIoErrorNoSuchDevice: return "no such device";
        case SoundIoErrorInvalid: return "invalid value";
        case SoundIoErrorBackendUnavailable: return "backend unavailable";
        case SoundIoErrorStreaming: return "unrecoverable streaming failure";
        case SoundIoErrorIncompatibleDevice: return "incompatible device";
        case SoundIoErrorNoSuchClient: return "no such client";
        case SoundIoErrorIncompatibleBackend: return "incompatible backend";
        case SoundIoErrorBackendDisconnected: return "backend disconnected";
        case SoundIoErrorInterrupted: return "interrupted; try again";
        case SoundIoErrorUnderflow: return "buffer underflow";
        case SoundIoErrorEncodingString: return "failed to encode string";
    }
    return "(invalid error)";
}

int soundio_get_bytes_per_sample(enum SoundIoFormat format) {
    switch (format) {
    case SoundIoFormatU8:         return 1;
    case SoundIoFormatS8:         return 1;
    case SoundIoFormatS16LE:      return 2;
    case SoundIoFormatS16BE:      return 2;
    case SoundIoFormatU16LE:      return 2;
    case SoundIoFormatU16BE:      return 2;
    case SoundIoFormatS24LE:      return 4;
    case SoundIoFormatS24BE:      return 4;
    case SoundIoFormatU24LE:      return 4;
    case SoundIoFormatU24BE:      return 4;
    case SoundIoFormatS32LE:      return 4;
    case SoundIoFormatS32BE:      return 4;
    case SoundIoFormatU32LE:      return 4;
    case SoundIoFormatU32BE:      return 4;
    case SoundIoFormatFloat32LE:  return 4;
    case SoundIoFormatFloat32BE:  return 4;
    case SoundIoFormatFloat64LE:  return 8;
    case SoundIoFormatFloat64BE:  return 8;

    case SoundIoFormatInvalid:    return -1;
    }
    return -1;
}

const char * soundio_format_string(enum SoundIoFormat format) {
    switch (format) {
    case SoundIoFormatS8:         return "signed 8-bit";
    case SoundIoFormatU8:         return "unsigned 8-bit";
    case SoundIoFormatS16LE:      return "signed 16-bit LE";
    case SoundIoFormatS16BE:      return "signed 16-bit BE";
    case SoundIoFormatU16LE:      return "unsigned 16-bit LE";
    case SoundIoFormatU16BE:      return "unsigned 16-bit LE";
    case SoundIoFormatS24LE:      return "signed 24-bit LE";
    case SoundIoFormatS24BE:      return "signed 24-bit BE";
    case SoundIoFormatU24LE:      return "unsigned 24-bit LE";
    case SoundIoFormatU24BE:      return "unsigned 24-bit BE";
    case SoundIoFormatS32LE:      return "signed 32-bit LE";
    case SoundIoFormatS32BE:      return "signed 32-bit BE";
    case SoundIoFormatU32LE:      return "unsigned 32-bit LE";
    case SoundIoFormatU32BE:      return "unsigned 32-bit BE";
    case SoundIoFormatFloat32LE:  return "float 32-bit LE";
    case SoundIoFormatFloat32BE:  return "float 32-bit BE";
    case SoundIoFormatFloat64LE:  return "float 64-bit LE";
    case SoundIoFormatFloat64BE:  return "float 64-bit BE";

    case SoundIoFormatInvalid:
        return "(invalid sample format)";
    }
    return "(invalid sample format)";
}


const char *soundio_backend_name(enum SoundIoBackend backend) {
    switch (backend) {
        case SoundIoBackendNone: return "(none)";
        case SoundIoBackendJack: return "JACK";
        case SoundIoBackendPulseAudio: return "PulseAudio";
        case SoundIoBackendAlsa: return "ALSA";
        case SoundIoBackendCoreAudio: return "CoreAudio";
        case SoundIoBackendWasapi: return "WASAPI";
        case SoundIoBackendDummy: return "Dummy";
    }
    return "(invalid backend)";
}

void soundio_destroy(struct SoundIo *soundio) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;

    soundio_disconnect(soundio);

    free(si);
}

static void do_nothing_cb(struct SoundIo *soundio) { }
static void default_msg_callback(const char *msg) { }

static void default_backend_disconnect_cb(struct SoundIo *soundio, int err) {
    soundio_panic("libsoundio: backend disconnected: %s", soundio_strerror(err));
}

static struct SoundIoAtomicFlag rtprio_seen = SOUNDIO_ATOMIC_FLAG_INIT;
static void default_emit_rtprio_warning(void) {
    if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(rtprio_seen)) {
        fprintf(stderr, "warning: unable to set high priority thread: Operation not permitted\n");
        fprintf(stderr, "See "
            "https://github.com/andrewrk/genesis/wiki/warning:-unable-to-set-high-priority-thread:-Operation-not-permitted\n");
    }
}

struct SoundIo *soundio_create(void) {
    int err;
    if ((err = soundio_os_init()))
        return NULL;
    struct SoundIoPrivate *si = ALLOCATE(struct SoundIoPrivate, 1);
    if (!si)
        return NULL;
    struct SoundIo *soundio = &si->pub;
    soundio->on_devices_change = do_nothing_cb;
    soundio->on_backend_disconnect = default_backend_disconnect_cb;
    soundio->on_events_signal = do_nothing_cb;
    soundio->app_name = "SoundIo";
    soundio->emit_rtprio_warning = default_emit_rtprio_warning;
    soundio->jack_info_callback = default_msg_callback;
    soundio->jack_error_callback = default_msg_callback;
    return soundio;
}

int soundio_connect(struct SoundIo *soundio) {
    int err = 0;

    for (int i = 0; i < ARRAY_LENGTH(available_backends); i += 1) {
        enum SoundIoBackend backend = available_backends[i];
        err = soundio_connect_backend(soundio, backend);
        if (!err)
            return 0;
        if (err != SoundIoErrorInitAudioBackend)
            return err;
    }

    return err;
}

int soundio_connect_backend(struct SoundIo *soundio, enum SoundIoBackend backend) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;

    if (soundio->current_backend)
        return SoundIoErrorInvalid;

    if (backend <= 0 || backend > SoundIoBackendDummy)
        return SoundIoErrorInvalid;

    int (*fn)(struct SoundIoPrivate *) = backend_init_fns[backend];

    if (!fn)
        return SoundIoErrorBackendUnavailable;

    int err;
    if ((err = backend_init_fns[backend](si))) {
        soundio_disconnect(soundio);
        return err;
    }
    soundio->current_backend = backend;

    return 0;
}

void soundio_disconnect(struct SoundIo *soundio) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;

    if (!si)
        return;

    if (si->destroy)
        si->destroy(si);
    memset(&si->backend_data, 0, sizeof(union SoundIoBackendData));

    soundio->current_backend = SoundIoBackendNone;

    soundio_destroy_devices_info(si->safe_devices_info);
    si->safe_devices_info = NULL;

    si->destroy = NULL;
    si->flush_events = NULL;
    si->wait_events = NULL;
    si->wakeup = NULL;
    si->force_device_scan = NULL;

    si->outstream_open = NULL;
    si->outstream_destroy = NULL;
    si->outstream_start = NULL;
    si->outstream_begin_write = NULL;
    si->outstream_end_write = NULL;
    si->outstream_clear_buffer = NULL;
    si->outstream_pause = NULL;
    si->outstream_get_latency = NULL;

    si->instream_open = NULL;
    si->instream_destroy = NULL;
    si->instream_start = NULL;
    si->instream_begin_read = NULL;
    si->instream_end_read = NULL;
    si->instream_pause = NULL;
    si->instream_get_latency = NULL;
}

void soundio_flush_events(struct SoundIo *soundio) {
    assert(soundio->current_backend != SoundIoBackendNone);
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    si->flush_events(si);
}

int soundio_input_device_count(struct SoundIo *soundio) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;

    assert(si->safe_devices_info);
    if (!si->safe_devices_info)
        return -1;

    assert(soundio->current_backend != SoundIoBackendNone);
    if (soundio->current_backend == SoundIoBackendNone)
        return -1;

    return si->safe_devices_info->input_devices.length;
}

int soundio_output_device_count(struct SoundIo *soundio) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;

    assert(si->safe_devices_info);
    if (!si->safe_devices_info)
        return -1;

    assert(soundio->current_backend != SoundIoBackendNone);
    if (soundio->current_backend == SoundIoBackendNone)
        return -1;

    return si->safe_devices_info->output_devices.length;
}

int soundio_default_input_device_index(struct SoundIo *soundio) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;

    assert(si->safe_devices_info);
    if (!si->safe_devices_info)
        return -1;

    assert(soundio->current_backend != SoundIoBackendNone);
    if (soundio->current_backend == SoundIoBackendNone)
        return -1;

    return si->safe_devices_info->default_input_index;
}

int soundio_default_output_device_index(struct SoundIo *soundio) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;

    assert(si->safe_devices_info);
    if (!si->safe_devices_info)
        return -1;

    assert(soundio->current_backend != SoundIoBackendNone);
    if (soundio->current_backend == SoundIoBackendNone)
        return -1;

    return si->safe_devices_info->default_output_index;
}

struct SoundIoDevice *soundio_get_input_device(struct SoundIo *soundio, int index) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;

    assert(soundio->current_backend != SoundIoBackendNone);
    if (soundio->current_backend == SoundIoBackendNone)
        return NULL;

    assert(si->safe_devices_info);
    if (!si->safe_devices_info)
        return NULL;

    assert(index >= 0);
    assert(index < si->safe_devices_info->input_devices.length);
    if (index < 0 || index >= si->safe_devices_info->input_devices.length)
        return NULL;

    struct SoundIoDevice *device = SoundIoListDevicePtr_val_at(&si->safe_devices_info->input_devices, index);
    soundio_device_ref(device);
    return device;
}

struct SoundIoDevice *soundio_get_output_device(struct SoundIo *soundio, int index) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;

    assert(soundio->current_backend != SoundIoBackendNone);
    if (soundio->current_backend == SoundIoBackendNone)
        return NULL;

    assert(si->safe_devices_info);
    if (!si->safe_devices_info)
        return NULL;

    assert(index >= 0);
    assert(index < si->safe_devices_info->output_devices.length);
    if (index < 0 || index >= si->safe_devices_info->output_devices.length)
        return NULL;

    struct SoundIoDevice *device = SoundIoListDevicePtr_val_at(&si->safe_devices_info->output_devices, index);
    soundio_device_ref(device);
    return device;
}

void soundio_device_unref(struct SoundIoDevice *device) {
    if (!device)
        return;

    device->ref_count -= 1;
    assert(device->ref_count >= 0);

    if (device->ref_count == 0) {
        struct SoundIoDevicePrivate *dev = (struct SoundIoDevicePrivate *)device;
        if (dev->destruct)
            dev->destruct(dev);

        free(device->id);
        free(device->name);

        if (device->sample_rates != &dev->prealloc_sample_rate_range &&
            device->sample_rates != dev->sample_rates.items)
        {
            free(device->sample_rates);
        }
        SoundIoListSampleRateRange_deinit(&dev->sample_rates);

        if (device->formats != &dev->prealloc_format)
            free(device->formats);

        if (device->layouts != &device->current_layout)
            free(device->layouts);

        free(dev);
    }
}

void soundio_device_ref(struct SoundIoDevice *device) {
    assert(device);
    device->ref_count += 1;
}

void soundio_wait_events(struct SoundIo *soundio) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    si->wait_events(si);
}

void soundio_wakeup(struct SoundIo *soundio) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    si->wakeup(si);
}

void soundio_force_device_scan(struct SoundIo *soundio) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    si->force_device_scan(si);
}

int soundio_outstream_begin_write(struct SoundIoOutStream *outstream,
        struct SoundIoChannelArea **areas, int *frame_count)
{
    struct SoundIo *soundio = outstream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)outstream;
    if (*frame_count <= 0)
        return SoundIoErrorInvalid;
    return si->outstream_begin_write(si, os, areas, frame_count);
}

int soundio_outstream_end_write(struct SoundIoOutStream *outstream) {
    struct SoundIo *soundio = outstream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)outstream;
    return si->outstream_end_write(si, os);
}

static void default_outstream_error_callback(struct SoundIoOutStream *os, int err) {
    soundio_panic("libsoundio: %s", soundio_strerror(err));
}

static void default_underflow_callback(struct SoundIoOutStream *outstream) { }

struct SoundIoOutStream *soundio_outstream_create(struct SoundIoDevice *device) {
    struct SoundIoOutStreamPrivate *os = ALLOCATE(struct SoundIoOutStreamPrivate, 1);
    struct SoundIoOutStream *outstream = &os->pub;

    if (!os)
        return NULL;
    if (!device)
        return NULL;

    outstream->device = device;
    soundio_device_ref(device);

    outstream->error_callback = default_outstream_error_callback;
    outstream->underflow_callback = default_underflow_callback;

    return outstream;
}

int soundio_outstream_open(struct SoundIoOutStream *outstream) {
    struct SoundIoDevice *device = outstream->device;

    if (device->aim != SoundIoDeviceAimOutput)
        return SoundIoErrorInvalid;

    if (device->probe_error)
        return device->probe_error;

    if (outstream->layout.channel_count > SOUNDIO_MAX_CHANNELS)
        return SoundIoErrorInvalid;

    if (outstream->format == SoundIoFormatInvalid) {
        outstream->format = soundio_device_supports_format(device, SoundIoFormatFloat32NE) ?
            SoundIoFormatFloat32NE : device->formats[0];
    }

    if (outstream->format <= SoundIoFormatInvalid)
        return SoundIoErrorInvalid;

    if (!outstream->layout.channel_count) {
        const struct SoundIoChannelLayout *stereo = soundio_channel_layout_get_builtin(SoundIoChannelLayoutIdStereo);
        outstream->layout = soundio_device_supports_layout(device, stereo) ? *stereo : device->layouts[0];
    }

    if (!outstream->sample_rate)
        outstream->sample_rate = soundio_device_nearest_sample_rate(device, 48000);

    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)outstream;
    outstream->bytes_per_frame = soundio_get_bytes_per_frame(outstream->format, outstream->layout.channel_count);
    outstream->bytes_per_sample = soundio_get_bytes_per_sample(outstream->format);

    struct SoundIo *soundio = device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    return si->outstream_open(si, os);
}

void soundio_outstream_destroy(struct SoundIoOutStream *outstream) {
    if (!outstream)
        return;

    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)outstream;
    struct SoundIo *soundio = outstream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;

    if (si->outstream_destroy)
        si->outstream_destroy(si, os);

    soundio_device_unref(outstream->device);
    free(os);
}

int soundio_outstream_start(struct SoundIoOutStream *outstream) {
    struct SoundIo *soundio = outstream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)outstream;
    return si->outstream_start(si, os);
}

int soundio_outstream_pause(struct SoundIoOutStream *outstream, bool pause) {
    struct SoundIo *soundio = outstream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)outstream;
    return si->outstream_pause(si, os, pause);
}

int soundio_outstream_clear_buffer(struct SoundIoOutStream *outstream) {
    struct SoundIo *soundio = outstream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)outstream;
    return si->outstream_clear_buffer(si, os);
}

int soundio_outstream_get_latency(struct SoundIoOutStream *outstream, double *out_latency) {
    struct SoundIo *soundio = outstream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)outstream;
    return si->outstream_get_latency(si, os, out_latency);
}

static void default_instream_error_callback(struct SoundIoInStream *is, int err) {
    soundio_panic("libsoundio: %s", soundio_strerror(err));
}

static void default_overflow_callback(struct SoundIoInStream *instream) { }

struct SoundIoInStream *soundio_instream_create(struct SoundIoDevice *device) {
    struct SoundIoInStreamPrivate *is = ALLOCATE(struct SoundIoInStreamPrivate, 1);
    struct SoundIoInStream *instream = &is->pub;

    if (!is)
        return NULL;
    if (!device)
        return NULL;

    instream->device = device;
    soundio_device_ref(device);

    instream->error_callback = default_instream_error_callback;
    instream->overflow_callback = default_overflow_callback;

    return instream;
}

int soundio_instream_open(struct SoundIoInStream *instream) {
    struct SoundIoDevice *device = instream->device;
    if (device->aim != SoundIoDeviceAimInput)
        return SoundIoErrorInvalid;

    if (instream->format <= SoundIoFormatInvalid)
        return SoundIoErrorInvalid;

    if (instream->layout.channel_count > SOUNDIO_MAX_CHANNELS)
        return SoundIoErrorInvalid;

    if (device->probe_error)
        return device->probe_error;

    if (instream->format == SoundIoFormatInvalid) {
        instream->format = soundio_device_supports_format(device, SoundIoFormatFloat32NE) ?
            SoundIoFormatFloat32NE : device->formats[0];
    }

    if (!instream->layout.channel_count) {
        const struct SoundIoChannelLayout *stereo = soundio_channel_layout_get_builtin(SoundIoChannelLayoutIdStereo);
        instream->layout = soundio_device_supports_layout(device, stereo) ? *stereo : device->layouts[0];
    }

    if (!instream->sample_rate)
        instream->sample_rate = soundio_device_nearest_sample_rate(device, 48000);


    instream->bytes_per_frame = soundio_get_bytes_per_frame(instream->format, instream->layout.channel_count);
    instream->bytes_per_sample = soundio_get_bytes_per_sample(instream->format);
    struct SoundIo *soundio = device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *)instream;
    return si->instream_open(si, is);
}

int soundio_instream_start(struct SoundIoInStream *instream) {
    struct SoundIo *soundio = instream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *)instream;
    return si->instream_start(si, is);
}

void soundio_instream_destroy(struct SoundIoInStream *instream) {
    if (!instream)
        return;

    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *)instream;
    struct SoundIo *soundio = instream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;

    if (si->instream_destroy)
        si->instream_destroy(si, is);

    soundio_device_unref(instream->device);
    free(is);
}

int soundio_instream_pause(struct SoundIoInStream *instream, bool pause) {
    struct SoundIo *soundio = instream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *)instream;
    return si->instream_pause(si, is, pause);
}

int soundio_instream_begin_read(struct SoundIoInStream *instream,
        struct SoundIoChannelArea **areas, int *frame_count)
{
    struct SoundIo *soundio = instream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *)instream;
    return si->instream_begin_read(si, is, areas, frame_count);
}

int soundio_instream_end_read(struct SoundIoInStream *instream) {
    struct SoundIo *soundio = instream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *)instream;
    return si->instream_end_read(si, is);
}

int soundio_instream_get_latency(struct SoundIoInStream *instream, double *out_latency) {
    struct SoundIo *soundio = instream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *)instream;
    return si->instream_get_latency(si, is, out_latency);
}

void soundio_destroy_devices_info(struct SoundIoDevicesInfo *devices_info) {
    if (!devices_info)
        return;

    for (int i = 0; i < devices_info->input_devices.length; i += 1)
        soundio_device_unref(SoundIoListDevicePtr_val_at(&devices_info->input_devices, i));
    for (int i = 0; i < devices_info->output_devices.length; i += 1)
        soundio_device_unref(SoundIoListDevicePtr_val_at(&devices_info->output_devices, i));

    SoundIoListDevicePtr_deinit(&devices_info->input_devices);
    SoundIoListDevicePtr_deinit(&devices_info->output_devices);

    free(devices_info);
}

bool soundio_have_backend(enum SoundIoBackend backend) {
    assert(backend > 0);
    assert(backend <= SoundIoBackendDummy);
    return backend_init_fns[backend];
}

int soundio_backend_count(struct SoundIo *soundio) {
    return ARRAY_LENGTH(available_backends);
}

enum SoundIoBackend soundio_get_backend(struct SoundIo *soundio, int index) {
    return available_backends[index];
}

static bool layout_contains(const struct SoundIoChannelLayout *available_layouts, int available_layouts_count,
        const struct SoundIoChannelLayout *target_layout)
{
    for (int i = 0; i < available_layouts_count; i += 1) {
        const struct SoundIoChannelLayout *available_layout = &available_layouts[i];
        if (soundio_channel_layout_equal(target_layout, available_layout))
            return true;
    }
    return false;
}

const struct SoundIoChannelLayout *soundio_best_matching_channel_layout(
        const struct SoundIoChannelLayout *preferred_layouts, int preferred_layouts_count,
        const struct SoundIoChannelLayout *available_layouts, int available_layouts_count)
{
    for (int i = 0; i < preferred_layouts_count; i += 1) {
        const struct SoundIoChannelLayout *preferred_layout = &preferred_layouts[i];
        if (layout_contains(available_layouts, available_layouts_count, preferred_layout))
            return preferred_layout;
    }
    return NULL;
}

static int compare_layouts(const void *a, const void *b) {
    const struct SoundIoChannelLayout *layout_a = *((struct SoundIoChannelLayout **)a);
    const struct SoundIoChannelLayout *layout_b = *((struct SoundIoChannelLayout **)b);
    if (layout_a->channel_count > layout_b->channel_count)
        return -1;
    else if (layout_a->channel_count < layout_b->channel_count)
        return 1;
    else
        return 0;
}

void soundio_sort_channel_layouts(struct SoundIoChannelLayout *layouts, int layouts_count) {
    if (!layouts)
        return;

    qsort(layouts, layouts_count, sizeof(struct SoundIoChannelLayout), compare_layouts);
}

void soundio_device_sort_channel_layouts(struct SoundIoDevice *device) {
    soundio_sort_channel_layouts(device->layouts, device->layout_count);
}

bool soundio_device_supports_format(struct SoundIoDevice *device, enum SoundIoFormat format) {
    for (int i = 0; i < device->format_count; i += 1) {
        if (device->formats[i] == format)
            return true;
    }
    return false;
}

bool soundio_device_supports_layout(struct SoundIoDevice *device,
        const struct SoundIoChannelLayout *layout)
{
    for (int i = 0; i < device->layout_count; i += 1) {
        if (soundio_channel_layout_equal(&device->layouts[i], layout))
            return true;
    }
    return false;
}

bool soundio_device_supports_sample_rate(struct SoundIoDevice *device, int sample_rate) {
    for (int i = 0; i < device->sample_rate_count; i += 1) {
        struct SoundIoSampleRateRange *range = &device->sample_rates[i];
        if (sample_rate >= range->min && sample_rate <= range->max)
            return true;
    }
    return false;
}

static int abs_diff_int(int a, int b) {
    int x = a - b;
    return (x >= 0) ? x : -x;
}

int soundio_device_nearest_sample_rate(struct SoundIoDevice *device, int sample_rate) {
    int best_rate = -1;
    int best_delta = -1;
    for (int i = 0; i < device->sample_rate_count; i += 1) {
        struct SoundIoSampleRateRange *range = &device->sample_rates[i];
        int candidate_rate = soundio_int_clamp(range->min, sample_rate, range->max);
        if (candidate_rate == sample_rate)
            return candidate_rate;

        int delta = abs_diff_int(candidate_rate, sample_rate);
        bool best_rate_too_small = best_rate < sample_rate;
        bool candidate_rate_too_small = candidate_rate < sample_rate;
        if (best_rate == -1 ||
            (best_rate_too_small && !candidate_rate_too_small) ||
            ((best_rate_too_small || !candidate_rate_too_small) && delta < best_delta))
        {
            best_rate = candidate_rate;
            best_delta = delta;
        }
    }
    return best_rate;
}

bool soundio_device_equal(
        const struct SoundIoDevice *a,
        const struct SoundIoDevice *b)
{
    return a->is_raw == b->is_raw && a->aim == b->aim && strcmp(a->id, b->id) == 0;
}

const char *soundio_version_string(void) {
    return SOUNDIO_VERSION_STRING;
}

int soundio_version_major(void) {
    return SOUNDIO_VERSION_MAJOR;
}

int soundio_version_minor(void) {
    return SOUNDIO_VERSION_MINOR;
}

int soundio_version_patch(void) {
    return SOUNDIO_VERSION_PATCH;
}
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

// (amalg) #include "soundio_private.h"

#include <stdio.h>

static struct SoundIoChannelLayout builtin_channel_layouts[] = {
    {
        "Mono",
        1,
        {
            SoundIoChannelIdFrontCenter,
        },
    },
    {
        "Stereo",
        2,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
        },
    },
    {
        "2.1",
        3,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdLfe,
        },
    },
    {
        "3.0",
        3,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
        }
    },
    {
        "3.0 (back)",
        3,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdBackCenter,
        }
    },
    {
        "3.1",
        4,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdLfe,
        }
    },
    {
        "4.0",
        4,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdBackCenter,
        }
    },
    {
        "Quad",
        4,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdBackLeft,
            SoundIoChannelIdBackRight,
        },
    },
    {
        "Quad (side)",
        4,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdSideLeft,
            SoundIoChannelIdSideRight,
        }
    },
    {
        "4.1",
        5,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdBackCenter,
            SoundIoChannelIdLfe,
        }
    },
    {
        "5.0 (back)",
        5,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdBackLeft,
            SoundIoChannelIdBackRight,
        }
    },
    {
        "5.0 (side)",
        5,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdSideLeft,
            SoundIoChannelIdSideRight,
        }
    },
    {
        "5.1",
        6,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdSideLeft,
            SoundIoChannelIdSideRight,
            SoundIoChannelIdLfe,
        }
    },
    {
        "5.1 (back)",
        6,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdBackLeft,
            SoundIoChannelIdBackRight,
            SoundIoChannelIdLfe,
        }
    },
    {
        "6.0 (side)",
        6,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdSideLeft,
            SoundIoChannelIdSideRight,
            SoundIoChannelIdBackCenter,
        }
    },
    {
        "6.0 (front)",
        6,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdSideLeft,
            SoundIoChannelIdSideRight,
            SoundIoChannelIdFrontLeftCenter,
            SoundIoChannelIdFrontRightCenter,
        }
    },
    {
        "Hexagonal",
        6,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdBackLeft,
            SoundIoChannelIdBackRight,
            SoundIoChannelIdBackCenter,
        }
    },
    {
        "6.1",
        7,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdSideLeft,
            SoundIoChannelIdSideRight,
            SoundIoChannelIdBackCenter,
            SoundIoChannelIdLfe,
        }
    },
    {
        "6.1 (back)",
        7,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdBackLeft,
            SoundIoChannelIdBackRight,
            SoundIoChannelIdBackCenter,
            SoundIoChannelIdLfe,
        }
    },
    {
        "6.1 (front)",
        7,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdSideLeft,
            SoundIoChannelIdSideRight,
            SoundIoChannelIdFrontLeftCenter,
            SoundIoChannelIdFrontRightCenter,
            SoundIoChannelIdLfe,
        }
    },
    {
        "7.0",
        7,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdSideLeft,
            SoundIoChannelIdSideRight,
            SoundIoChannelIdBackLeft,
            SoundIoChannelIdBackRight,
        }
    },
    {
        "7.0 (front)",
        7,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdSideLeft,
            SoundIoChannelIdSideRight,
            SoundIoChannelIdFrontLeftCenter,
            SoundIoChannelIdFrontRightCenter,
        }
    },
    {
        "7.1",
        8,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdSideLeft,
            SoundIoChannelIdSideRight,
            SoundIoChannelIdBackLeft,
            SoundIoChannelIdBackRight,
            SoundIoChannelIdLfe,
        }
    },
    {
        "7.1 (wide)",
        8,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdSideLeft,
            SoundIoChannelIdSideRight,
            SoundIoChannelIdFrontLeftCenter,
            SoundIoChannelIdFrontRightCenter,
            SoundIoChannelIdLfe,
        }
    },
    {
        "7.1 (wide) (back)",
        8,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdBackLeft,
            SoundIoChannelIdBackRight,
            SoundIoChannelIdFrontLeftCenter,
            SoundIoChannelIdFrontRightCenter,
            SoundIoChannelIdLfe,
        }
    },
    {
        "Octagonal",
        8,
        {
            SoundIoChannelIdFrontLeft,
            SoundIoChannelIdFrontRight,
            SoundIoChannelIdFrontCenter,
            SoundIoChannelIdSideLeft,
            SoundIoChannelIdSideRight,
            SoundIoChannelIdBackLeft,
            SoundIoChannelIdBackRight,
            SoundIoChannelIdBackCenter,
        }
    },
};

#define CHANNEL_NAME_ALIAS_COUNT 3
typedef const char *channel_names_t[CHANNEL_NAME_ALIAS_COUNT];
static channel_names_t channel_names[] = {
    {"(Invalid Channel)", NULL, NULL},
    {"Front Left", "FL", "front-left"},
    {"Front Right", "FR", "front-right"},
    {"Front Center", "FC", "front-center"},
    {"LFE", "LFE", "lfe"},
    {"Back Left", "BL", "rear-left"},
    {"Back Right", "BR", "rear-right"},
    {"Front Left Center", "FLC", "front-left-of-center"},
    {"Front Right Center", "FRC", "front-right-of-center"},
    {"Back Center", "BC", "rear-center"},
    {"Side Left", "SL", "side-left"},
    {"Side Right", "SR", "side-right"},
    {"Top Center", "TC", "top-center"},
    {"Top Front Left", "TFL", "top-front-left"},
    {"Top Front Center", "TFC", "top-front-center"},
    {"Top Front Right", "TFR", "top-front-right"},
    {"Top Back Left", "TBL", "top-rear-left"},
    {"Top Back Center", "TBC", "top-rear-center"},
    {"Top Back Right", "TBR", "top-rear-right"},
    {"Back Left Center", NULL, NULL},
    {"Back Right Center", NULL, NULL},
    {"Front Left Wide", NULL, NULL},
    {"Front Right Wide", NULL, NULL},
    {"Front Left High", NULL, NULL},
    {"Front Center High", NULL, NULL},
    {"Front Right High", NULL, NULL},
    {"Top Front Left Center", NULL, NULL},
    {"Top Front Right Center", NULL, NULL},
    {"Top Side Left", NULL, NULL},
    {"Top Side Right", NULL, NULL},
    {"Left LFE", NULL, NULL},
    {"Right LFE", NULL, NULL},
    {"LFE 2", NULL, NULL},
    {"Bottom Center", NULL, NULL},
    {"Bottom Left Center", NULL, NULL},
    {"Bottom Right Center", NULL, NULL},
    {"Mid/Side Mid", NULL, NULL},
    {"Mid/Side Side", NULL, NULL},
    {"Ambisonic W", NULL, NULL},
    {"Ambisonic X", NULL, NULL},
    {"Ambisonic Y", NULL, NULL},
    {"Ambisonic Z", NULL, NULL},
    {"X-Y X", NULL, NULL},
    {"X-Y Y", NULL, NULL},
    {"Headphones Left", NULL, NULL},
    {"Headphones Right", NULL, NULL},
    {"Click Track", NULL, NULL},
    {"Foreign Language", NULL, NULL},
    {"Hearing Impaired", NULL, NULL},
    {"Narration", NULL, NULL},
    {"Haptic", NULL, NULL},
    {"Dialog Centric Mix", NULL, NULL},
    {"Aux", NULL, NULL},
    {"Aux 0", NULL, NULL},
    {"Aux 1", NULL, NULL},
    {"Aux 2", NULL, NULL},
    {"Aux 3", NULL, NULL},
    {"Aux 4", NULL, NULL},
    {"Aux 5", NULL, NULL},
    {"Aux 6", NULL, NULL},
    {"Aux 7", NULL, NULL},
    {"Aux 8", NULL, NULL},
    {"Aux 9", NULL, NULL},
    {"Aux 10", NULL, NULL},
    {"Aux 11", NULL, NULL},
    {"Aux 12", NULL, NULL},
    {"Aux 13", NULL, NULL},
    {"Aux 14", NULL, NULL},
    {"Aux 15", NULL, NULL},
};

const char *soundio_get_channel_name(enum SoundIoChannelId id) {
    if (id >= ARRAY_LENGTH(channel_names))
        return "(Invalid Channel)";
    else
        return channel_names[id][0];
}

bool soundio_channel_layout_equal(
        const struct SoundIoChannelLayout *a,
        const struct SoundIoChannelLayout *b)
{
    if (a->channel_count != b->channel_count)
        return false;

    for (int i = 0; i < a->channel_count; i += 1) {
        if (a->channels[i] != b->channels[i])
            return false;
    }

    return true;
}

int soundio_channel_layout_builtin_count(void) {
    return ARRAY_LENGTH(builtin_channel_layouts);
}

const struct SoundIoChannelLayout *soundio_channel_layout_get_builtin(int index) {
    assert(index >= 0);
    assert(index <= ARRAY_LENGTH(builtin_channel_layouts));
    return &builtin_channel_layouts[index];
}

int soundio_channel_layout_find_channel(
        const struct SoundIoChannelLayout *layout, enum SoundIoChannelId channel)
{
    for (int i = 0; i < layout->channel_count; i += 1) {
        if (layout->channels[i] == channel)
            return i;
    }
    return -1;
}

bool soundio_channel_layout_detect_builtin(struct SoundIoChannelLayout *layout) {
    for (int i = 0; i < ARRAY_LENGTH(builtin_channel_layouts); i += 1) {
        const struct SoundIoChannelLayout *builtin_layout = &builtin_channel_layouts[i];
        if (soundio_channel_layout_equal(builtin_layout, layout)) {
            layout->name = builtin_layout->name;
            return true;
        }
    }
    layout->name = NULL;
    return false;
}

const struct SoundIoChannelLayout *soundio_channel_layout_get_default(int channel_count) {
    switch (channel_count) {
        case 1: return soundio_channel_layout_get_builtin(SoundIoChannelLayoutIdMono);
        case 2: return soundio_channel_layout_get_builtin(SoundIoChannelLayoutIdStereo);
        case 3: return soundio_channel_layout_get_builtin(SoundIoChannelLayoutId3Point0);
        case 4: return soundio_channel_layout_get_builtin(SoundIoChannelLayoutId4Point0);
        case 5: return soundio_channel_layout_get_builtin(SoundIoChannelLayoutId5Point0Back);
        case 6: return soundio_channel_layout_get_builtin(SoundIoChannelLayoutId5Point1Back);
        case 7: return soundio_channel_layout_get_builtin(SoundIoChannelLayoutId6Point1);
        case 8: return soundio_channel_layout_get_builtin(SoundIoChannelLayoutId7Point1);
    }
    return NULL;
}

enum SoundIoChannelId soundio_parse_channel_id(const char *str, int str_len) {
    for (int id = 0; id < ARRAY_LENGTH(channel_names); id += 1) {
        for (int i = 0; i < CHANNEL_NAME_ALIAS_COUNT; i += 1) {
            const char *alias = channel_names[id][i];
            if (!alias)
                break;
            int alias_len = strlen(alias);
            if (soundio_streql(alias, alias_len, str, str_len))
                return (enum SoundIoChannelId)id;
        }
    }
    return SoundIoChannelIdInvalid;
}

#if SOUNDIO_HAVE_JACK
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

// (amalg) #include "jack.h"
// (amalg) #include "soundio_private.h"
// (amalg) #include "list.h"

#include <stdio.h>

static struct SoundIoAtomicFlag global_msg_callback_flag = SOUNDIO_ATOMIC_FLAG_INIT;

struct SoundIoJackPort {
    const char *full_name;
    int full_name_len;
    const char *name;
    int name_len;
    enum SoundIoChannelId channel_id;
    jack_latency_range_t latency_range;
};

struct SoundIoJackClient {
    const char *name;
    int name_len;
    bool is_physical;
    enum SoundIoDeviceAim aim;
    int port_count;
    struct SoundIoJackPort ports[SOUNDIO_MAX_CHANNELS];
};

SOUNDIO_MAKE_LIST_STRUCT(struct SoundIoJackClient, SoundIoListJackClient, SOUNDIO_LIST_STATIC)
SOUNDIO_MAKE_LIST_DEF(struct SoundIoJackClient, SoundIoListJackClient, SOUNDIO_LIST_STATIC)

static void split_str(const char *input_str, int input_str_len, char c,
        const char **out_1, int *out_len_1, const char **out_2, int *out_len_2)
{
    *out_1 = input_str;
    while (*input_str) {
        if (*input_str == c) {
            *out_len_1 = input_str - *out_1;
            *out_2 = input_str + 1;
            *out_len_2 = input_str_len - 1 - *out_len_1;
            return;
        }
        input_str += 1;
    }
}

static struct SoundIoJackClient *find_or_create_client(struct SoundIoListJackClient *clients,
        enum SoundIoDeviceAim aim, bool is_physical, const char *client_name, int client_name_len)
{
    for (int i = 0; i < clients->length; i += 1) {
        struct SoundIoJackClient *client = SoundIoListJackClient_ptr_at(clients, i);
        if (client->is_physical == is_physical &&
            client->aim == aim &&
            soundio_streql(client->name, client->name_len, client_name, client_name_len))
        {
            return client;
        }
    }
    int err;
    if ((err = SoundIoListJackClient_add_one(clients)))
        return NULL;
    struct SoundIoJackClient *client = SoundIoListJackClient_last_ptr(clients);
    client->is_physical = is_physical;
    client->aim = aim;
    client->name = client_name;
    client->name_len = client_name_len;
    client->port_count = 0;
    return client;
}

static void destruct_device(struct SoundIoDevicePrivate *dp) {
    struct SoundIoDeviceJack *dj = &dp->backend_data.jack;
    for (int i = 0; i < dj->port_count; i += 1) {
        struct SoundIoDeviceJackPort *djp = &dj->ports[i];
        free(djp->full_name);
    }
    free(dj->ports);
}

static int refresh_devices_bare(struct SoundIoPrivate *si) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoJack *sij = &si->backend_data.jack;

    if (sij->is_shutdown)
        return SoundIoErrorBackendDisconnected;


    struct SoundIoDevicesInfo *devices_info = ALLOCATE(struct SoundIoDevicesInfo, 1);
    if (!devices_info)
        return SoundIoErrorNoMem;

    devices_info->default_output_index = -1;
    devices_info->default_input_index = -1;
    const char **port_names = jack_get_ports(sij->client, NULL, NULL, 0);
    if (!port_names) {
        soundio_destroy_devices_info(devices_info);
        return SoundIoErrorNoMem;
    }

    struct SoundIoListJackClient clients = {0};
    const char **port_name_ptr = port_names;
    for (; *port_name_ptr; port_name_ptr += 1) {
        const char *client_and_port_name = *port_name_ptr;
        int client_and_port_name_len = strlen(client_and_port_name);
        jack_port_t *jport = jack_port_by_name(sij->client, client_and_port_name);
        if (!jport) {
            // This refresh devices scan is already outdated. Just give up and
            // let refresh_devices be called again.
            jack_free(port_names);
            soundio_destroy_devices_info(devices_info);
            return SoundIoErrorInterrupted;
        }

        int flags = jack_port_flags(jport);
        const char *port_type = jack_port_type(jport);
        if (strcmp(port_type, JACK_DEFAULT_AUDIO_TYPE) != 0) {
            // we don't know how to support such a port
            continue;
        }

        enum SoundIoDeviceAim aim = (flags & JackPortIsInput) ?
            SoundIoDeviceAimOutput : SoundIoDeviceAimInput;
        bool is_physical = flags & JackPortIsPhysical;

        const char *client_name = NULL;
        const char *port_name = NULL;
        int client_name_len;
        int port_name_len;
        split_str(client_and_port_name, client_and_port_name_len, ':',
                &client_name, &client_name_len, &port_name, &port_name_len);
        if (!client_name || !port_name) {
            // device does not have colon, skip it
            continue;
        }
        struct SoundIoJackClient *client = find_or_create_client(&clients, aim, is_physical,
                client_name, client_name_len);
        if (!client) {
            jack_free(port_names);
            soundio_destroy_devices_info(devices_info);
            return SoundIoErrorNoMem;
        }
        if (client->port_count >= SOUNDIO_MAX_CHANNELS) {
            // we hit the channel limit, skip the leftovers
            continue;
        }
        struct SoundIoJackPort *port = &client->ports[client->port_count++];
        port->full_name = client_and_port_name;
        port->full_name_len = client_and_port_name_len;
        port->name = port_name;
        port->name_len = port_name_len;
        port->channel_id = soundio_parse_channel_id(port_name, port_name_len);

        jack_latency_callback_mode_t latency_mode = (aim == SoundIoDeviceAimOutput) ?
            JackPlaybackLatency : JackCaptureLatency;
        jack_port_get_latency_range(jport, latency_mode, &port->latency_range);
    }

    for (int i = 0; i < clients.length; i += 1) {
        struct SoundIoJackClient *client = SoundIoListJackClient_ptr_at(&clients, i);
        if (client->port_count <= 0)
            continue;

        struct SoundIoDevicePrivate *dev = ALLOCATE(struct SoundIoDevicePrivate, 1);
        if (!dev) {
            jack_free(port_names);
            soundio_destroy_devices_info(devices_info);
            return SoundIoErrorNoMem;
        }
        struct SoundIoDevice *device = &dev->pub;
        struct SoundIoDeviceJack *dj = &dev->backend_data.jack;
        int description_len = client->name_len + 3 + 2 * client->port_count;
        for (int port_index = 0; port_index < client->port_count; port_index += 1) {
            struct SoundIoJackPort *port = &client->ports[port_index];

            description_len += port->name_len;
        }

        dev->destruct = destruct_device;

        device->ref_count = 1;
        device->soundio = soundio;
        device->is_raw = false;
        device->aim = client->aim;
        device->id = soundio_str_dupe(client->name, client->name_len);
        device->name = ALLOCATE(char, description_len);
        device->current_format = SoundIoFormatFloat32NE;
        device->sample_rate_count = 1;
        device->sample_rates = &dev->prealloc_sample_rate_range;
        device->sample_rates[0].min = sij->sample_rate;
        device->sample_rates[0].max = sij->sample_rate;
        device->sample_rate_current = sij->sample_rate;

        device->software_latency_current = sij->period_size / (double) sij->sample_rate;
        device->software_latency_min = sij->period_size / (double) sij->sample_rate;
        device->software_latency_max = sij->period_size / (double) sij->sample_rate;

        dj->port_count = client->port_count;
        dj->ports = ALLOCATE(struct SoundIoDeviceJackPort, dj->port_count);

        if (!device->id || !device->name || !dj->ports) {
            jack_free(port_names);
            soundio_device_unref(device);
            soundio_destroy_devices_info(devices_info);
            return SoundIoErrorNoMem;
        }

        for (int port_index = 0; port_index < client->port_count; port_index += 1) {
            struct SoundIoJackPort *port = &client->ports[port_index];
            struct SoundIoDeviceJackPort *djp = &dj->ports[port_index];
            djp->full_name = soundio_str_dupe(port->full_name, port->full_name_len);
            djp->full_name_len = port->full_name_len;
            djp->channel_id = port->channel_id;
            djp->latency_range = port->latency_range;

            if (!djp->full_name) {
                jack_free(port_names);
                soundio_device_unref(device);
                soundio_destroy_devices_info(devices_info);
                return SoundIoErrorNoMem;
            }
        }

        memcpy(device->name, client->name, client->name_len);
        memcpy(&device->name[client->name_len], ": ", 2);
        int index = client->name_len + 2;
        for (int port_index = 0; port_index < client->port_count; port_index += 1) {
            struct SoundIoJackPort *port = &client->ports[port_index];
            memcpy(&device->name[index], port->name, port->name_len);
            index += port->name_len;
            if (port_index + 1 < client->port_count) {
                memcpy(&device->name[index], ", ", 2);
                index += 2;
            }
        }

        device->current_layout.channel_count = client->port_count;
        bool any_invalid = false;
        for (int port_index = 0; port_index < client->port_count; port_index += 1) {
            struct SoundIoJackPort *port = &client->ports[port_index];
            device->current_layout.channels[port_index] = port->channel_id;
            any_invalid = any_invalid || (port->channel_id == SoundIoChannelIdInvalid);
        }
        if (any_invalid) {
            const struct SoundIoChannelLayout *layout = soundio_channel_layout_get_default(client->port_count);
            if (layout)
                device->current_layout = *layout;
        } else {
            soundio_channel_layout_detect_builtin(&device->current_layout);
        }

        device->layout_count = 1;
        device->layouts = &device->current_layout;
        device->format_count = 1;
        device->formats = &dev->prealloc_format;
        device->formats[0] = device->current_format;

        struct SoundIoListDevicePtr *device_list;
        if (device->aim == SoundIoDeviceAimOutput) {
            device_list = &devices_info->output_devices;
            if (devices_info->default_output_index < 0 && client->is_physical)
                devices_info->default_output_index = device_list->length;
        } else {
            assert(device->aim == SoundIoDeviceAimInput);
            device_list = &devices_info->input_devices;
            if (devices_info->default_input_index < 0 && client->is_physical)
                devices_info->default_input_index = device_list->length;
        }

        if (SoundIoListDevicePtr_append(device_list, device)) {
            soundio_device_unref(device);
            soundio_destroy_devices_info(devices_info);
            return SoundIoErrorNoMem;
        }

    }
    jack_free(port_names);

    soundio_destroy_devices_info(si->safe_devices_info);
    si->safe_devices_info = devices_info;

    return 0;
}

static int jack_refresh_devices(struct SoundIoPrivate *si) {
    int err = SoundIoErrorInterrupted;
    while (err == SoundIoErrorInterrupted)
        err = refresh_devices_bare(si);
    return err;
}

static void jack_my_flush_events(struct SoundIoPrivate *si, bool wait) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoJack *sij = &si->backend_data.jack;
    int err;

    bool cb_shutdown = false;

    soundio_os_mutex_lock(sij->mutex);

    if (wait)
        soundio_os_cond_wait(sij->cond, sij->mutex);

    if (sij->is_shutdown && !sij->emitted_shutdown_cb) {
        sij->emitted_shutdown_cb = true;
        cb_shutdown = true;
    }

    soundio_os_mutex_unlock(sij->mutex);

    if (cb_shutdown) {
        soundio->on_backend_disconnect(soundio, SoundIoErrorBackendDisconnected);
    } else {
        if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(sij->refresh_devices_flag)) {
            if ((err = jack_refresh_devices(si))) {
                SOUNDIO_ATOMIC_FLAG_CLEAR(sij->refresh_devices_flag);
            } else {
                soundio->on_devices_change(soundio);
            }
        }
    }
}

static void flush_events_jack(struct SoundIoPrivate *si) {
    jack_my_flush_events(si, false);
}

static void wait_events_jack(struct SoundIoPrivate *si) {
    jack_my_flush_events(si, false);
    jack_my_flush_events(si, true);
}

static void wakeup_jack(struct SoundIoPrivate *si) {
    struct SoundIoJack *sij = &si->backend_data.jack;
    soundio_os_mutex_lock(sij->mutex);
    soundio_os_cond_signal(sij->cond, sij->mutex);
    soundio_os_mutex_unlock(sij->mutex);
}

static void force_device_scan_jack(struct SoundIoPrivate *si) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoJack *sij = &si->backend_data.jack;
    SOUNDIO_ATOMIC_FLAG_CLEAR(sij->refresh_devices_flag);
    soundio_os_mutex_lock(sij->mutex);
    soundio_os_cond_signal(sij->cond, sij->mutex);
    soundio->on_events_signal(soundio);
    soundio_os_mutex_unlock(sij->mutex);
}

static int outstream_process_callback(jack_nframes_t nframes, void *arg) {
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)arg;
    struct SoundIoOutStreamJack *osj = &os->backend_data.jack;
    struct SoundIoOutStream *outstream = &os->pub;
    osj->frames_left = nframes;
    for (int ch = 0; ch < outstream->layout.channel_count; ch += 1) {
        struct SoundIoOutStreamJackPort *osjp = &osj->ports[ch];
        osj->areas[ch].ptr = (char*)jack_port_get_buffer(osjp->source_port, nframes);
        osj->areas[ch].step = outstream->bytes_per_sample;
    }
    outstream->write_callback(outstream, osj->frames_left, osj->frames_left);
    return 0;
}

static void outstream_destroy_jack(struct SoundIoPrivate *is, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamJack *osj = &os->backend_data.jack;

    jack_client_close(osj->client);
    osj->client = NULL;
}

static struct SoundIoDeviceJackPort *find_port_matching_channel(struct SoundIoDevice *device, enum SoundIoChannelId id) {
    struct SoundIoDevicePrivate *dev = (struct SoundIoDevicePrivate *)device;
    struct SoundIoDeviceJack *dj = &dev->backend_data.jack;

    for (int ch = 0; ch < device->current_layout.channel_count; ch += 1) {
        enum SoundIoChannelId chan_id = device->current_layout.channels[ch];
        if (chan_id == id)
            return &dj->ports[ch];
    }

    return NULL;
}

static int outstream_xrun_callback(void *arg) {
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)arg;
    struct SoundIoOutStream *outstream = &os->pub;
    outstream->underflow_callback(outstream);
    return 0;
}

static int outstream_buffer_size_callback(jack_nframes_t nframes, void *arg) {
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)arg;
    struct SoundIoOutStreamJack *osj = &os->backend_data.jack;
    struct SoundIoOutStream *outstream = &os->pub;
    if ((jack_nframes_t)osj->period_size == nframes) {
        return 0;
    } else {
        outstream->error_callback(outstream, SoundIoErrorStreaming);
        return -1;
    }
}

static int outstream_sample_rate_callback(jack_nframes_t nframes, void *arg) {
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)arg;
    struct SoundIoOutStream *outstream = &os->pub;
    if (nframes == (jack_nframes_t)outstream->sample_rate) {
        return 0;
    } else {
        outstream->error_callback(outstream, SoundIoErrorStreaming);
        return -1;
    }
}

static void outstream_shutdown_callback(void *arg) {
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)arg;
    struct SoundIoOutStream *outstream = &os->pub;
    outstream->error_callback(outstream, SoundIoErrorStreaming);
}

static inline jack_nframes_t nframes_max(jack_nframes_t a, jack_nframes_t b) {
    return (a >= b) ? a : b;
}

static int outstream_open_jack(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoJack *sij = &si->backend_data.jack;
    struct SoundIoOutStreamJack *osj = &os->backend_data.jack;
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoDevice *device = outstream->device;
    struct SoundIoDevicePrivate *dev = (struct SoundIoDevicePrivate *)device;
    struct SoundIoDeviceJack *dj = &dev->backend_data.jack;

    if (sij->is_shutdown)
        return SoundIoErrorBackendDisconnected;

    if (!outstream->name)
        outstream->name = "SoundIoOutStream";

    outstream->software_latency = device->software_latency_current;
    osj->period_size = sij->period_size;

    jack_status_t status;
    osj->client = jack_client_open(outstream->name, JackNoStartServer, &status);
    if (!osj->client) {
        outstream_destroy_jack(si, os);
        assert(!(status & JackInvalidOption));
        if (status & JackShmFailure)
            return SoundIoErrorSystemResources;
        if (status & JackNoSuchClient)
            return SoundIoErrorNoSuchClient;
        return SoundIoErrorOpeningDevice;
    }

    int err;
    if ((err = jack_set_process_callback(osj->client, outstream_process_callback, os))) {
        outstream_destroy_jack(si, os);
        return SoundIoErrorOpeningDevice;
    }
    if ((err = jack_set_buffer_size_callback(osj->client, outstream_buffer_size_callback, os))) {
        outstream_destroy_jack(si, os);
        return SoundIoErrorOpeningDevice;
    }
    if ((err = jack_set_sample_rate_callback(osj->client, outstream_sample_rate_callback, os))) {
        outstream_destroy_jack(si, os);
        return SoundIoErrorOpeningDevice;
    }
    if ((err = jack_set_xrun_callback(osj->client, outstream_xrun_callback, os))) {
        outstream_destroy_jack(si, os);
        return SoundIoErrorOpeningDevice;
    }
    jack_on_shutdown(osj->client, outstream_shutdown_callback, os);


    jack_nframes_t max_port_latency = 0;

    // register ports and map channels
    int connected_count = 0;
    for (int ch = 0; ch < outstream->layout.channel_count; ch += 1) {
        enum SoundIoChannelId my_channel_id = outstream->layout.channels[ch];
        const char *channel_name = soundio_get_channel_name(my_channel_id);
        unsigned long flags = JackPortIsOutput;
        if (!outstream->non_terminal_hint)
            flags |= JackPortIsTerminal;
        jack_port_t *jport = jack_port_register(osj->client, channel_name, JACK_DEFAULT_AUDIO_TYPE, flags, 0);
        if (!jport) {
            outstream_destroy_jack(si, os);
            return SoundIoErrorOpeningDevice;
        }
        struct SoundIoOutStreamJackPort *osjp = &osj->ports[ch];
        osjp->source_port = jport;
        // figure out which dest port this connects to
        struct SoundIoDeviceJackPort *djp = find_port_matching_channel(device, my_channel_id);
        if (djp) {
            osjp->dest_port_name = djp->full_name;
            osjp->dest_port_name_len = djp->full_name_len;
            connected_count += 1;
            max_port_latency = nframes_max(max_port_latency, djp->latency_range.max);
        }
    }
    // If nothing got connected, channel layouts aren't working. Just send the
    // data in the order of the ports.
    if (connected_count == 0) {
        max_port_latency = 0;
        outstream->layout_error = SoundIoErrorIncompatibleDevice;

        int ch_count = soundio_int_min(outstream->layout.channel_count, dj->port_count);
        for (int ch = 0; ch < ch_count; ch += 1) {
            struct SoundIoOutStreamJackPort *osjp = &osj->ports[ch];
            struct SoundIoDeviceJackPort *djp = &dj->ports[ch];
            osjp->dest_port_name = djp->full_name;
            osjp->dest_port_name_len = djp->full_name_len;
            max_port_latency = nframes_max(max_port_latency, djp->latency_range.max);
        }
    }

    osj->hardware_latency = max_port_latency / (double)outstream->sample_rate;

    return 0;
}

static int outstream_pause_jack(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os, bool pause) {
    struct SoundIoJack *sij = &si->backend_data.jack;

    if (sij->is_shutdown)
        return SoundIoErrorBackendDisconnected;

    return SoundIoErrorIncompatibleBackend;
}

static int outstream_start_jack(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamJack *osj = &os->backend_data.jack;
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoJack *sij = &si->backend_data.jack;
    int err;

    if (sij->is_shutdown)
        return SoundIoErrorBackendDisconnected;

    if ((err = jack_activate(osj->client)))
        return SoundIoErrorStreaming;

    for (int ch = 0; ch < outstream->layout.channel_count; ch += 1) {
        struct SoundIoOutStreamJackPort *osjp = &osj->ports[ch];
        const char *dest_port_name = osjp->dest_port_name;
        // allow unconnected ports
        if (!dest_port_name)
            continue;
        const char *source_port_name = jack_port_name(osjp->source_port);
        if ((err = jack_connect(osj->client, source_port_name, dest_port_name)))
            return SoundIoErrorStreaming;
    }

    return 0;
}

static int outstream_begin_write_jack(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os,
        struct SoundIoChannelArea **out_areas, int *frame_count)
{
    struct SoundIoOutStreamJack *osj = &os->backend_data.jack;

    if (*frame_count != osj->frames_left)
        return SoundIoErrorInvalid;

    *out_areas = osj->areas;

    return 0;
}

static int outstream_end_write_jack(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamJack *osj = &os->backend_data.jack;
    osj->frames_left = 0;
    return 0;
}

static int outstream_clear_buffer_jack(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    return SoundIoErrorIncompatibleBackend;
}

static int outstream_get_latency_jack(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os,
        double *out_latency)
{
    struct SoundIoOutStreamJack *osj = &os->backend_data.jack;
    *out_latency = osj->hardware_latency;
    return 0;
}


static void instream_destroy_jack(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamJack *isj = &is->backend_data.jack;

    jack_client_close(isj->client);
    isj->client = NULL;
}

static int instream_xrun_callback(void *arg) {
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *)arg;
    struct SoundIoInStream *instream = &is->pub;
    instream->overflow_callback(instream);
    return 0;
}

static int instream_buffer_size_callback(jack_nframes_t nframes, void *arg) {
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *)arg;
    struct SoundIoInStreamJack *isj = &is->backend_data.jack;
    struct SoundIoInStream *instream = &is->pub;

    if ((jack_nframes_t)isj->period_size == nframes) {
        return 0;
    } else {
        instream->error_callback(instream, SoundIoErrorStreaming);
        return -1;
    }
}

static int instream_sample_rate_callback(jack_nframes_t nframes, void *arg) {
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *)arg;
    struct SoundIoInStream *instream = &is->pub;
    if (nframes == (jack_nframes_t)instream->sample_rate) {
        return 0;
    } else {
        instream->error_callback(instream, SoundIoErrorStreaming);
        return -1;
    }
}

static void instream_shutdown_callback(void *arg) {
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *)arg;
    struct SoundIoInStream *instream = &is->pub;
    instream->error_callback(instream, SoundIoErrorStreaming);
}

static int instream_process_callback(jack_nframes_t nframes, void *arg) {
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *)arg;
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoInStreamJack *isj = &is->backend_data.jack;
    isj->frames_left = nframes;
    for (int ch = 0; ch < instream->layout.channel_count; ch += 1) {
        struct SoundIoInStreamJackPort *isjp = &isj->ports[ch];
        isj->areas[ch].ptr = (char*)jack_port_get_buffer(isjp->dest_port, nframes);
        isj->areas[ch].step = instream->bytes_per_sample;
    }
    instream->read_callback(instream, isj->frames_left, isj->frames_left);
    return 0;
}

static int instream_open_jack(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoInStreamJack *isj = &is->backend_data.jack;
    struct SoundIoJack *sij = &si->backend_data.jack;
    struct SoundIoDevice *device = instream->device;
    struct SoundIoDevicePrivate *dev = (struct SoundIoDevicePrivate *)device;
    struct SoundIoDeviceJack *dj = &dev->backend_data.jack;

    if (sij->is_shutdown)
        return SoundIoErrorBackendDisconnected;

    if (!instream->name)
        instream->name = "SoundIoInStream";

    instream->software_latency = device->software_latency_current;
    isj->period_size = sij->period_size;

    jack_status_t status;
    isj->client = jack_client_open(instream->name, JackNoStartServer, &status);
    if (!isj->client) {
        instream_destroy_jack(si, is);
        assert(!(status & JackInvalidOption));
        if (status & JackShmFailure)
            return SoundIoErrorSystemResources;
        if (status & JackNoSuchClient)
            return SoundIoErrorNoSuchClient;
        return SoundIoErrorOpeningDevice;
    }

    int err;
    if ((err = jack_set_process_callback(isj->client, instream_process_callback, is))) {
        instream_destroy_jack(si, is);
        return SoundIoErrorOpeningDevice;
    }
    if ((err = jack_set_buffer_size_callback(isj->client, instream_buffer_size_callback, is))) {
        instream_destroy_jack(si, is);
        return SoundIoErrorOpeningDevice;
    }
    if ((err = jack_set_sample_rate_callback(isj->client, instream_sample_rate_callback, is))) {
        instream_destroy_jack(si, is);
        return SoundIoErrorOpeningDevice;
    }
    if ((err = jack_set_xrun_callback(isj->client, instream_xrun_callback, is))) {
        instream_destroy_jack(si, is);
        return SoundIoErrorOpeningDevice;
    }
    jack_on_shutdown(isj->client, instream_shutdown_callback, is);

    jack_nframes_t max_port_latency = 0;

    // register ports and map channels
    int connected_count = 0;
    for (int ch = 0; ch < instream->layout.channel_count; ch += 1) {
        enum SoundIoChannelId my_channel_id = instream->layout.channels[ch];
        const char *channel_name = soundio_get_channel_name(my_channel_id);
        unsigned long flags = JackPortIsInput;
        if (!instream->non_terminal_hint)
            flags |= JackPortIsTerminal;
        jack_port_t *jport = jack_port_register(isj->client, channel_name, JACK_DEFAULT_AUDIO_TYPE, flags, 0);
        if (!jport) {
            instream_destroy_jack(si, is);
            return SoundIoErrorOpeningDevice;
        }
        struct SoundIoInStreamJackPort *isjp = &isj->ports[ch];
        isjp->dest_port = jport;
        // figure out which source port this connects to
        struct SoundIoDeviceJackPort *djp = find_port_matching_channel(device, my_channel_id);
        if (djp) {
            isjp->source_port_name = djp->full_name;
            isjp->source_port_name_len = djp->full_name_len;
            connected_count += 1;
            max_port_latency = nframes_max(max_port_latency, djp->latency_range.max);
        }
    }
    // If nothing got connected, channel layouts aren't working. Just send the
    // data in the order of the ports.
    if (connected_count == 0) {
        max_port_latency = 0;
        instream->layout_error = SoundIoErrorIncompatibleDevice;

        int ch_count = soundio_int_min(instream->layout.channel_count, dj->port_count);
        for (int ch = 0; ch < ch_count; ch += 1) {
            struct SoundIoInStreamJackPort *isjp = &isj->ports[ch];
            struct SoundIoDeviceJackPort *djp = &dj->ports[ch];
            isjp->source_port_name = djp->full_name;
            isjp->source_port_name_len = djp->full_name_len;
            max_port_latency = nframes_max(max_port_latency, djp->latency_range.max);
        }
    }

    isj->hardware_latency = max_port_latency / (double)instream->sample_rate;

    return 0;
}

static int instream_pause_jack(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is, bool pause) {
    struct SoundIoJack *sij = &si->backend_data.jack;

    if (sij->is_shutdown)
        return SoundIoErrorBackendDisconnected;

    return SoundIoErrorIncompatibleBackend;
}

static int instream_start_jack(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamJack *isj = &is->backend_data.jack;
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoJack *sij = &si->backend_data.jack;
    int err;

    if (sij->is_shutdown)
        return SoundIoErrorBackendDisconnected;

    if ((err = jack_activate(isj->client)))
        return SoundIoErrorStreaming;

    for (int ch = 0; ch < instream->layout.channel_count; ch += 1) {
        struct SoundIoInStreamJackPort *isjp = &isj->ports[ch];
        const char *source_port_name = isjp->source_port_name;
        // allow unconnected ports
        if (!source_port_name)
            continue;
        const char *dest_port_name = jack_port_name(isjp->dest_port);
        if ((err = jack_connect(isj->client, source_port_name, dest_port_name)))
            return SoundIoErrorStreaming;
    }

    return 0;
}

static int instream_begin_read_jack(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is,
        struct SoundIoChannelArea **out_areas, int *frame_count)
{
    struct SoundIoInStreamJack *isj = &is->backend_data.jack;

    if (*frame_count != isj->frames_left)
        return SoundIoErrorInvalid;

    *out_areas = isj->areas;

    return 0;
}

static int instream_end_read_jack(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamJack *isj = &is->backend_data.jack;
    isj->frames_left = 0;
    return 0;
}

static int instream_get_latency_jack(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is,
        double *out_latency)
{
    struct SoundIoInStreamJack *isj = &is->backend_data.jack;
    *out_latency = isj->hardware_latency;
    return 0;
}

static void notify_devices_change(struct SoundIoPrivate *si) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoJack *sij = &si->backend_data.jack;
    SOUNDIO_ATOMIC_FLAG_CLEAR(sij->refresh_devices_flag);
    soundio_os_mutex_lock(sij->mutex);
    soundio_os_cond_signal(sij->cond, sij->mutex);
    soundio->on_events_signal(soundio);
    soundio_os_mutex_unlock(sij->mutex);
}

static int buffer_size_callback(jack_nframes_t nframes, void *arg) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)arg;
    struct SoundIoJack *sij = &si->backend_data.jack;
    sij->period_size = nframes;
    notify_devices_change(si);
    return 0;
}

static int sample_rate_callback(jack_nframes_t nframes, void *arg) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)arg;
    struct SoundIoJack *sij = &si->backend_data.jack;
    sij->sample_rate = nframes;
    notify_devices_change(si);
    return 0;
}

static void port_registration_callback(jack_port_id_t port_id, int reg, void *arg) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)arg;
    notify_devices_change(si);
}

static void port_rename_calllback(jack_port_id_t port_id,
        const char *old_name, const char *new_name, void *arg)
{
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)arg;
    notify_devices_change(si);
}

static void shutdown_callback(void *arg) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)arg;
    struct SoundIo *soundio = &si->pub;
    struct SoundIoJack *sij = &si->backend_data.jack;
    soundio_os_mutex_lock(sij->mutex);
    sij->is_shutdown = true;
    soundio_os_cond_signal(sij->cond, sij->mutex);
    soundio->on_events_signal(soundio);
    soundio_os_mutex_unlock(sij->mutex);
}

static void destroy_jack(struct SoundIoPrivate *si) {
    struct SoundIoJack *sij = &si->backend_data.jack;

    if (sij->client)
        jack_client_close(sij->client);

    if (sij->cond)
        soundio_os_cond_destroy(sij->cond);

    if (sij->mutex)
        soundio_os_mutex_destroy(sij->mutex);
}

int soundio_jack_init(struct SoundIoPrivate *si) {
    struct SoundIoJack *sij = &si->backend_data.jack;
    struct SoundIo *soundio = &si->pub;

    if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(global_msg_callback_flag)) {
        if (soundio->jack_error_callback)
            jack_set_error_function(soundio->jack_error_callback);
        if (soundio->jack_info_callback)
            jack_set_info_function(soundio->jack_info_callback);
        SOUNDIO_ATOMIC_FLAG_CLEAR(global_msg_callback_flag);
    }

    sij->mutex = soundio_os_mutex_create();
    if (!sij->mutex) {
        destroy_jack(si);
        return SoundIoErrorNoMem;
    }

    sij->cond = soundio_os_cond_create();
    if (!sij->cond) {
        destroy_jack(si);
        return SoundIoErrorNoMem;
    }

    // We pass JackNoStartServer due to
    // https://github.com/jackaudio/jack2/issues/138
    jack_status_t status;
    sij->client = jack_client_open(soundio->app_name, JackNoStartServer, &status);
    if (!sij->client) {
        destroy_jack(si);
        assert(!(status & JackInvalidOption));
        if (status & JackShmFailure)
            return SoundIoErrorSystemResources;
        if (status & JackNoSuchClient)
            return SoundIoErrorNoSuchClient;

        return SoundIoErrorInitAudioBackend;
    }

    int err;
    if ((err = jack_set_buffer_size_callback(sij->client, buffer_size_callback, si))) {
        destroy_jack(si);
        return SoundIoErrorInitAudioBackend;
    }
    if ((err = jack_set_sample_rate_callback(sij->client, sample_rate_callback, si))) {
        destroy_jack(si);
        return SoundIoErrorInitAudioBackend;
    }
    if ((err = jack_set_port_registration_callback(sij->client, port_registration_callback, si))) {
        destroy_jack(si);
        return SoundIoErrorInitAudioBackend;
    }
    if ((err = jack_set_port_rename_callback(sij->client, port_rename_calllback, si))) {
        destroy_jack(si);
        return SoundIoErrorInitAudioBackend;
    }
    jack_on_shutdown(sij->client, shutdown_callback, si);

    SOUNDIO_ATOMIC_FLAG_CLEAR(sij->refresh_devices_flag);
    sij->period_size = jack_get_buffer_size(sij->client);
    sij->sample_rate = jack_get_sample_rate(sij->client);

    if ((err = jack_activate(sij->client))) {
        destroy_jack(si);
        return SoundIoErrorInitAudioBackend;
    }

    if ((err = jack_refresh_devices(si))) {
        destroy_jack(si);
        return err;
    }

    si->destroy = destroy_jack;
    si->flush_events = flush_events_jack;
    si->wait_events = wait_events_jack;
    si->wakeup = wakeup_jack;
    si->force_device_scan = force_device_scan_jack;

    si->outstream_open = outstream_open_jack;
    si->outstream_destroy = outstream_destroy_jack;
    si->outstream_start = outstream_start_jack;
    si->outstream_begin_write = outstream_begin_write_jack;
    si->outstream_end_write = outstream_end_write_jack;
    si->outstream_clear_buffer = outstream_clear_buffer_jack;
    si->outstream_pause = outstream_pause_jack;
    si->outstream_get_latency = outstream_get_latency_jack;

    si->instream_open = instream_open_jack;
    si->instream_destroy = instream_destroy_jack;
    si->instream_start = instream_start_jack;
    si->instream_begin_read = instream_begin_read_jack;
    si->instream_end_read = instream_end_read_jack;
    si->instream_pause = instream_pause_jack;
    si->instream_get_latency = instream_get_latency_jack;

    return 0;
}
#endif
#if SOUNDIO_HAVE_ALSA
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#define _GNU_SOURCE
// (amalg) #include "alsa.h"
// (amalg) #include "soundio_private.h"

#include <sys/inotify.h>
#include <fcntl.h>
#include <unistd.h>

static snd_pcm_stream_t stream_types[] = {SND_PCM_STREAM_PLAYBACK, SND_PCM_STREAM_CAPTURE};

static snd_pcm_access_t prioritized_access_types[] = {
    SND_PCM_ACCESS_MMAP_INTERLEAVED,
    SND_PCM_ACCESS_MMAP_NONINTERLEAVED,
    SND_PCM_ACCESS_MMAP_COMPLEX,
    SND_PCM_ACCESS_RW_INTERLEAVED,
    SND_PCM_ACCESS_RW_NONINTERLEAVED,
};

SOUNDIO_MAKE_LIST_DEF(struct SoundIoAlsaPendingFile, SoundIoListAlsaPendingFile, SOUNDIO_LIST_STATIC)

static void wakeup_device_poll(struct SoundIoAlsa *sia) {
    ssize_t amt = write(sia->notify_pipe_fd[1], "a", 1);
    if (amt == -1) {
        assert(errno != EBADF);
        assert(errno != EIO);
        assert(errno != ENOSPC);
        assert(errno != EPERM);
        assert(errno != EPIPE);
    }
}

static void wakeup_outstream_poll(struct SoundIoOutStreamAlsa *osa) {
    ssize_t amt = write(osa->poll_exit_pipe_fd[1], "a", 1);
    if (amt == -1) {
        assert(errno != EBADF);
        assert(errno != EIO);
        assert(errno != ENOSPC);
        assert(errno != EPERM);
        assert(errno != EPIPE);
    }
}

static void destroy_alsa(struct SoundIoPrivate *si) {
    struct SoundIoAlsa *sia = &si->backend_data.alsa;

    if (sia->thread) {
        SOUNDIO_ATOMIC_FLAG_CLEAR(sia->abort_flag);
        wakeup_device_poll(sia);
        soundio_os_thread_destroy(sia->thread);
    }

    SoundIoListAlsaPendingFile_deinit(&sia->pending_files);

    if (sia->cond)
        soundio_os_cond_destroy(sia->cond);

    if (sia->mutex)
        soundio_os_mutex_destroy(sia->mutex);

    soundio_destroy_devices_info(sia->ready_devices_info);



    close(sia->notify_pipe_fd[0]);
    close(sia->notify_pipe_fd[1]);
    close(sia->notify_fd);
}

static inline snd_pcm_uframes_t ceil_dbl_to_uframes(double x) {
    const double truncation = (snd_pcm_uframes_t)x;
    return truncation + (truncation < x);
}

static char * str_partition_on_char(char *str, char c) {
    if (!str)
        return NULL;
    while (*str) {
        if (*str == c) {
            *str = 0;
            return str + 1;
        }
        str += 1;
    }
    return NULL;
}

static snd_pcm_stream_t aim_to_stream(enum SoundIoDeviceAim aim) {
    switch (aim) {
        case SoundIoDeviceAimOutput: return SND_PCM_STREAM_PLAYBACK;
        case SoundIoDeviceAimInput: return SND_PCM_STREAM_CAPTURE;
    }
    assert(0); // Invalid aim
    return SND_PCM_STREAM_PLAYBACK;
}

static enum SoundIoChannelId from_alsa_chmap_pos(unsigned int pos) {
    switch ((enum snd_pcm_chmap_position)pos) {
        case SND_CHMAP_UNKNOWN: return SoundIoChannelIdInvalid;
        case SND_CHMAP_NA:      return SoundIoChannelIdInvalid;
        case SND_CHMAP_MONO:    return SoundIoChannelIdFrontCenter;
        case SND_CHMAP_FL:      return SoundIoChannelIdFrontLeft; // front left
        case SND_CHMAP_FR:      return SoundIoChannelIdFrontRight; // front right
        case SND_CHMAP_RL:      return SoundIoChannelIdBackLeft; // rear left
        case SND_CHMAP_RR:      return SoundIoChannelIdBackRight; // rear right
        case SND_CHMAP_FC:      return SoundIoChannelIdFrontCenter; // front center
        case SND_CHMAP_LFE:     return SoundIoChannelIdLfe; // LFE
        case SND_CHMAP_SL:      return SoundIoChannelIdSideLeft; // side left
        case SND_CHMAP_SR:      return SoundIoChannelIdSideRight; // side right
        case SND_CHMAP_RC:      return SoundIoChannelIdBackCenter; // rear center
        case SND_CHMAP_FLC:     return SoundIoChannelIdFrontLeftCenter; // front left center
        case SND_CHMAP_FRC:     return SoundIoChannelIdFrontRightCenter; // front right center
        case SND_CHMAP_RLC:     return SoundIoChannelIdBackLeftCenter; // rear left center
        case SND_CHMAP_RRC:     return SoundIoChannelIdBackRightCenter; // rear right center
        case SND_CHMAP_FLW:     return SoundIoChannelIdFrontLeftWide; // front left wide
        case SND_CHMAP_FRW:     return SoundIoChannelIdFrontRightWide; // front right wide
        case SND_CHMAP_FLH:     return SoundIoChannelIdFrontLeftHigh; // front left high
        case SND_CHMAP_FCH:     return SoundIoChannelIdFrontCenterHigh; // front center high
        case SND_CHMAP_FRH:     return SoundIoChannelIdFrontRightHigh; // front right high
        case SND_CHMAP_TC:      return SoundIoChannelIdTopCenter; // top center
        case SND_CHMAP_TFL:     return SoundIoChannelIdTopFrontLeft; // top front left
        case SND_CHMAP_TFR:     return SoundIoChannelIdTopFrontRight; // top front right
        case SND_CHMAP_TFC:     return SoundIoChannelIdTopFrontCenter; // top front center
        case SND_CHMAP_TRL:     return SoundIoChannelIdTopBackLeft; // top rear left
        case SND_CHMAP_TRR:     return SoundIoChannelIdTopBackRight; // top rear right
        case SND_CHMAP_TRC:     return SoundIoChannelIdTopBackCenter; // top rear center
        case SND_CHMAP_TFLC:    return SoundIoChannelIdTopFrontLeftCenter; // top front left center
        case SND_CHMAP_TFRC:    return SoundIoChannelIdTopFrontRightCenter; // top front right center
        case SND_CHMAP_TSL:     return SoundIoChannelIdTopSideLeft; // top side left
        case SND_CHMAP_TSR:     return SoundIoChannelIdTopSideRight; // top side right
        case SND_CHMAP_LLFE:    return SoundIoChannelIdLeftLfe; // left LFE
        case SND_CHMAP_RLFE:    return SoundIoChannelIdRightLfe; // right LFE
        case SND_CHMAP_BC:      return SoundIoChannelIdBottomCenter; // bottom center
        case SND_CHMAP_BLC:     return SoundIoChannelIdBottomLeftCenter; // bottom left center
        case SND_CHMAP_BRC:     return SoundIoChannelIdBottomRightCenter; // bottom right center
    }
    return SoundIoChannelIdInvalid;
}

static int to_alsa_chmap_pos(enum SoundIoChannelId channel_id) {
    switch (channel_id) {
        case SoundIoChannelIdFrontLeft:             return SND_CHMAP_FL;
        case SoundIoChannelIdFrontRight:            return SND_CHMAP_FR;
        case SoundIoChannelIdBackLeft:              return SND_CHMAP_RL;
        case SoundIoChannelIdBackRight:             return SND_CHMAP_RR;
        case SoundIoChannelIdFrontCenter:           return SND_CHMAP_FC;
        case SoundIoChannelIdLfe:                   return SND_CHMAP_LFE;
        case SoundIoChannelIdSideLeft:              return SND_CHMAP_SL;
        case SoundIoChannelIdSideRight:             return SND_CHMAP_SR;
        case SoundIoChannelIdBackCenter:            return SND_CHMAP_RC;
        case SoundIoChannelIdFrontLeftCenter:       return SND_CHMAP_FLC;
        case SoundIoChannelIdFrontRightCenter:      return SND_CHMAP_FRC;
        case SoundIoChannelIdBackLeftCenter:        return SND_CHMAP_RLC;
        case SoundIoChannelIdBackRightCenter:       return SND_CHMAP_RRC;
        case SoundIoChannelIdFrontLeftWide:         return SND_CHMAP_FLW;
        case SoundIoChannelIdFrontRightWide:        return SND_CHMAP_FRW;
        case SoundIoChannelIdFrontLeftHigh:         return SND_CHMAP_FLH;
        case SoundIoChannelIdFrontCenterHigh:       return SND_CHMAP_FCH;
        case SoundIoChannelIdFrontRightHigh:        return SND_CHMAP_FRH;
        case SoundIoChannelIdTopCenter:             return SND_CHMAP_TC;
        case SoundIoChannelIdTopFrontLeft:          return SND_CHMAP_TFL;
        case SoundIoChannelIdTopFrontRight:         return SND_CHMAP_TFR;
        case SoundIoChannelIdTopFrontCenter:        return SND_CHMAP_TFC;
        case SoundIoChannelIdTopBackLeft:           return SND_CHMAP_TRL;
        case SoundIoChannelIdTopBackRight:          return SND_CHMAP_TRR;
        case SoundIoChannelIdTopBackCenter:         return SND_CHMAP_TRC;
        case SoundIoChannelIdTopFrontLeftCenter:    return SND_CHMAP_TFLC;
        case SoundIoChannelIdTopFrontRightCenter:   return SND_CHMAP_TFRC;
        case SoundIoChannelIdTopSideLeft:           return SND_CHMAP_TSL;
        case SoundIoChannelIdTopSideRight:          return SND_CHMAP_TSR;
        case SoundIoChannelIdLeftLfe:               return SND_CHMAP_LLFE;
        case SoundIoChannelIdRightLfe:              return SND_CHMAP_RLFE;
        case SoundIoChannelIdBottomCenter:          return SND_CHMAP_BC;
        case SoundIoChannelIdBottomLeftCenter:      return SND_CHMAP_BLC;
        case SoundIoChannelIdBottomRightCenter:     return SND_CHMAP_BRC;

        default:
            return SND_CHMAP_UNKNOWN;
    }
}

static void get_channel_layout(struct SoundIoChannelLayout *dest, snd_pcm_chmap_t *chmap) {
    int channel_count = soundio_int_min(SOUNDIO_MAX_CHANNELS, chmap->channels);
    dest->channel_count = channel_count;
    for (int i = 0; i < channel_count; i += 1) {
        dest->channels[i] = from_alsa_chmap_pos(chmap->pos[i]);
    }
    soundio_channel_layout_detect_builtin(dest);
}

static int handle_channel_maps(struct SoundIoDevice *device, snd_pcm_chmap_query_t **maps) {
    if (!maps)
        return 0;

    snd_pcm_chmap_query_t **p;
    snd_pcm_chmap_query_t *v;

    // one iteration to count
    int layout_count = 0;
    for (p = maps; (v = *p) && layout_count < SOUNDIO_MAX_CHANNELS; p += 1, layout_count += 1) { }
    device->layouts = ALLOCATE(struct SoundIoChannelLayout, layout_count);
    if (!device->layouts) {
        snd_pcm_free_chmaps(maps);
        return SoundIoErrorNoMem;
    }
    device->layout_count = layout_count;

    // iterate again to collect data
    int layout_index;
    for (p = maps, layout_index = 0;
        (v = *p) && layout_index < layout_count;
        p += 1, layout_index += 1)
    {
        get_channel_layout(&device->layouts[layout_index], &v->map);
    }
    snd_pcm_free_chmaps(maps);

    return 0;
}

static snd_pcm_format_t to_alsa_fmt(enum SoundIoFormat fmt) {
    switch (fmt) {
    case SoundIoFormatS8:           return SND_PCM_FORMAT_S8;
    case SoundIoFormatU8:           return SND_PCM_FORMAT_U8;
    case SoundIoFormatS16LE:        return SND_PCM_FORMAT_S16_LE;
    case SoundIoFormatS16BE:        return SND_PCM_FORMAT_S16_BE;
    case SoundIoFormatU16LE:        return SND_PCM_FORMAT_U16_LE;
    case SoundIoFormatU16BE:        return SND_PCM_FORMAT_U16_BE;
    case SoundIoFormatS24LE:        return SND_PCM_FORMAT_S24_LE;
    case SoundIoFormatS24BE:        return SND_PCM_FORMAT_S24_BE;
    case SoundIoFormatU24LE:        return SND_PCM_FORMAT_U24_LE;
    case SoundIoFormatU24BE:        return SND_PCM_FORMAT_U24_BE;
    case SoundIoFormatS32LE:        return SND_PCM_FORMAT_S32_LE;
    case SoundIoFormatS32BE:        return SND_PCM_FORMAT_S32_BE;
    case SoundIoFormatU32LE:        return SND_PCM_FORMAT_U32_LE;
    case SoundIoFormatU32BE:        return SND_PCM_FORMAT_U32_BE;
    case SoundIoFormatFloat32LE:    return SND_PCM_FORMAT_FLOAT_LE;
    case SoundIoFormatFloat32BE:    return SND_PCM_FORMAT_FLOAT_BE;
    case SoundIoFormatFloat64LE:    return SND_PCM_FORMAT_FLOAT64_LE;
    case SoundIoFormatFloat64BE:    return SND_PCM_FORMAT_FLOAT64_BE;

    case SoundIoFormatInvalid:
        return SND_PCM_FORMAT_UNKNOWN;
    }
    return SND_PCM_FORMAT_UNKNOWN;
}

static void test_fmt_mask(struct SoundIoDevice *device, const snd_pcm_format_mask_t *fmt_mask,
        enum SoundIoFormat fmt)
{
    if (snd_pcm_format_mask_test(fmt_mask, to_alsa_fmt(fmt))) {
        device->formats[device->format_count] = fmt;
        device->format_count += 1;
    }
}

static int set_access(snd_pcm_t *handle, snd_pcm_hw_params_t *hwparams, snd_pcm_access_t *out_access) {
    for (int i = 0; i < ARRAY_LENGTH(prioritized_access_types); i += 1) {
        snd_pcm_access_t access = prioritized_access_types[i];
        int err = snd_pcm_hw_params_set_access(handle, hwparams, access);
        if (err >= 0) {
            if (out_access)
                *out_access = access;
            return 0;
        }
    }
    return SoundIoErrorOpeningDevice;
}

// this function does not override device->formats, so if you want it to, deallocate and set it to NULL
static int probe_open_device(struct SoundIoDevice *device, snd_pcm_t *handle, int resample,
        int *out_channels_min, int *out_channels_max)
{
    struct SoundIoDevicePrivate *dev = (struct SoundIoDevicePrivate *)device;
    int err;

    snd_pcm_hw_params_t *hwparams;
    snd_pcm_hw_params_alloca(&hwparams);

    if ((err = snd_pcm_hw_params_any(handle, hwparams)) < 0)
        return SoundIoErrorOpeningDevice;

    if ((err = snd_pcm_hw_params_set_rate_resample(handle, hwparams, resample)) < 0)
        return SoundIoErrorOpeningDevice;

    if ((err = set_access(handle, hwparams, NULL)))
        return err;

    unsigned int channels_min;
    unsigned int channels_max;

    if ((err = snd_pcm_hw_params_get_channels_min(hwparams, &channels_min)) < 0)
        return SoundIoErrorOpeningDevice;
    if ((err = snd_pcm_hw_params_set_channels_last(handle, hwparams, &channels_max)) < 0)
        return SoundIoErrorOpeningDevice;

    *out_channels_min = channels_min;
    *out_channels_max = channels_max;

    unsigned int rate_min;
    unsigned int rate_max;

    if ((err = snd_pcm_hw_params_get_rate_min(hwparams, &rate_min, NULL)) < 0)
        return SoundIoErrorOpeningDevice;

    if ((err = snd_pcm_hw_params_set_rate_last(handle, hwparams, &rate_max, NULL)) < 0)
        return SoundIoErrorOpeningDevice;

    device->sample_rate_count = 1;
    device->sample_rates = &dev->prealloc_sample_rate_range;
    device->sample_rates[0].min = rate_min;
    device->sample_rates[0].max = rate_max;

    double one_over_actual_rate = 1.0 / (double)rate_max;

    // Purposefully leave the parameters with the highest rate, highest channel count.

    snd_pcm_uframes_t min_frames;
    snd_pcm_uframes_t max_frames;


    if ((err = snd_pcm_hw_params_get_buffer_size_min(hwparams, &min_frames)) < 0)
        return SoundIoErrorOpeningDevice;
    if ((err = snd_pcm_hw_params_get_buffer_size_max(hwparams, &max_frames)) < 0)
        return SoundIoErrorOpeningDevice;

    device->software_latency_min = min_frames * one_over_actual_rate;
    device->software_latency_max = max_frames * one_over_actual_rate;

    if ((err = snd_pcm_hw_params_set_buffer_size_first(handle, hwparams, &min_frames)) < 0)
        return SoundIoErrorOpeningDevice;


    snd_pcm_format_mask_t *fmt_mask;
    snd_pcm_format_mask_alloca(&fmt_mask);
    snd_pcm_format_mask_none(fmt_mask);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_S8);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_U8);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_S16_LE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_S16_BE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_U16_LE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_U16_BE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_S24_LE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_S24_BE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_U24_LE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_U24_BE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_S32_LE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_S32_BE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_U32_LE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_U32_BE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_FLOAT_LE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_FLOAT_BE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_FLOAT64_LE);
    snd_pcm_format_mask_set(fmt_mask, SND_PCM_FORMAT_FLOAT64_BE);

    if ((err = snd_pcm_hw_params_set_format_mask(handle, hwparams, fmt_mask)) < 0)
        return SoundIoErrorOpeningDevice;

    if (!device->formats) {
        snd_pcm_hw_params_get_format_mask(hwparams, fmt_mask);
        device->formats = ALLOCATE(enum SoundIoFormat, 18);
        if (!device->formats)
            return SoundIoErrorNoMem;

        device->format_count = 0;
        test_fmt_mask(device, fmt_mask, SoundIoFormatS8);
        test_fmt_mask(device, fmt_mask, SoundIoFormatU8);
        test_fmt_mask(device, fmt_mask, SoundIoFormatS16LE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatS16BE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatU16LE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatU16BE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatS24LE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatS24BE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatU24LE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatU24BE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatS32LE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatS32BE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatU32LE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatU32BE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatFloat32LE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatFloat32BE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatFloat64LE);
        test_fmt_mask(device, fmt_mask, SoundIoFormatFloat64BE);
    }

    return 0;
}

static int probe_device(struct SoundIoDevice *device, snd_pcm_chmap_query_t **maps) {
    int err;
    snd_pcm_t *handle;

    snd_pcm_stream_t stream = aim_to_stream(device->aim);

    if ((err = snd_pcm_open(&handle, device->id, stream, 0)) < 0) {
        handle_channel_maps(device, maps);
        return SoundIoErrorOpeningDevice;
    }

    int channels_min, channels_max;
    if ((err = probe_open_device(device, handle, 0, &channels_min, &channels_max))) {
        handle_channel_maps(device, maps);
        snd_pcm_close(handle);
        return err;
    }

    if (!maps) {
        maps = snd_pcm_query_chmaps(handle);
        if (!maps) {
            // device gave us no channel maps. we're forced to conclude that
            // the min and max channel counts are correct.
            int layout_count = 0;
            for (int i = 0; i < soundio_channel_layout_builtin_count(); i += 1) {
                const struct SoundIoChannelLayout *layout = soundio_channel_layout_get_builtin(i);
                if (layout->channel_count >= channels_min && layout->channel_count <= channels_max) {
                    layout_count += 1;
                }
            }
            device->layout_count = layout_count;
            device->layouts = ALLOCATE(struct SoundIoChannelLayout, device->layout_count);
            if (!device->layouts) {
                snd_pcm_close(handle);
                return SoundIoErrorNoMem;
            }
            int layout_index = 0;
            for (int i = 0; i < soundio_channel_layout_builtin_count(); i += 1) {
                const struct SoundIoChannelLayout *layout = soundio_channel_layout_get_builtin(i);
                if (layout->channel_count >= channels_min && layout->channel_count <= channels_max) {
                    device->layouts[layout_index++] = *soundio_channel_layout_get_builtin(i);
                }
            }
        }
    }

    snd_pcm_chmap_t *chmap = snd_pcm_get_chmap(handle);
    if (chmap) {
        get_channel_layout(&device->current_layout, chmap);
        free(chmap);
    }
    if ((err = handle_channel_maps(device, maps))) {
        snd_pcm_close(handle);
        return err;
    }
    maps = NULL;

    if (!device->is_raw) {
        if (device->sample_rates[0].min == device->sample_rates[0].max)
            device->sample_rate_current = device->sample_rates[0].min;

        if (device->software_latency_min == device->software_latency_max)
            device->software_latency_current = device->software_latency_min;

        // now say that resampling is OK and see what the real min and max is.
        if ((err = probe_open_device(device, handle, 1, &channels_min, &channels_max)) < 0) {
            snd_pcm_close(handle);
            return SoundIoErrorOpeningDevice;
        }
    }

    snd_pcm_close(handle);
    return 0;
}

static inline bool str_has_prefix(const char *big_str, const char *prefix) {
    return strncmp(big_str, prefix, strlen(prefix)) == 0;
}

static int alsa_refresh_devices(struct SoundIoPrivate *si) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoAlsa *sia = &si->backend_data.alsa;

    int err;
    if ((err = snd_config_update_free_global()) < 0)
        return SoundIoErrorSystemResources;
    if ((err = snd_config_update()) < 0)
        return SoundIoErrorSystemResources;

    struct SoundIoDevicesInfo *devices_info = ALLOCATE(struct SoundIoDevicesInfo, 1);
    if (!devices_info)
        return SoundIoErrorNoMem;
    devices_info->default_output_index = -1;
    devices_info->default_input_index = -1;

    void **hints;
    if (snd_device_name_hint(-1, "pcm", &hints) < 0) {
        soundio_destroy_devices_info(devices_info);
        return SoundIoErrorNoMem;
    }

    int default_output_index = -1;
    int sysdefault_output_index = -1;
    int default_input_index = -1;
    int sysdefault_input_index = -1;

    for (void **hint_ptr = hints; *hint_ptr; hint_ptr += 1) {
        char *name = snd_device_name_get_hint(*hint_ptr, "NAME");
        // null - libsoundio has its own dummy backend. API clients should use
        // that instead of alsa null device.
        if (strcmp(name, "null") == 0 ||
            // all these surround devices are clutter
            str_has_prefix(name, "front:") ||
            str_has_prefix(name, "surround21:") ||
            str_has_prefix(name, "surround40:") ||
            str_has_prefix(name, "surround41:") ||
            str_has_prefix(name, "surround50:") ||
            str_has_prefix(name, "surround51:") ||
            str_has_prefix(name, "surround71:"))
        {
            free(name);
            continue;
        }

        // One or both of descr and descr1 can be NULL.
        char *descr = snd_device_name_get_hint(*hint_ptr, "DESC");
        char *descr1 = str_partition_on_char(descr, '\n');

        char *io = snd_device_name_get_hint(*hint_ptr, "IOID");
        bool is_playback;
        bool is_capture;

        // Workaround for Raspberry Pi driver bug, reporting itself as output
        // when really it is input.
        if (descr && strcmp(descr, "bcm2835 ALSA, bcm2835 ALSA") == 0 &&
            descr1 && strcmp(descr1, "Direct sample snooping device") == 0)
        {
            is_playback = false;
            is_capture = true;
        } else if (descr && strcmp(descr, "bcm2835 ALSA, bcm2835 IEC958/HDMI") == 0 &&
                   descr1 && strcmp(descr1, "Direct sample snooping device") == 0)
        {
            is_playback = false;
            is_capture = true;
        } else if (io) {
            if (strcmp(io, "Input") == 0) {
                is_playback = false;
                is_capture = true;
            } else {
                assert(strcmp(io, "Output") == 0);
                is_playback = true;
                is_capture = false;
            }
            free(io);
        } else {
            is_playback = true;
            is_capture = true;
        }

        for (int stream_type_i = 0; stream_type_i < ARRAY_LENGTH(stream_types); stream_type_i += 1) {
            snd_pcm_stream_t stream = stream_types[stream_type_i];
            if (stream == SND_PCM_STREAM_PLAYBACK && !is_playback) continue;
            if (stream == SND_PCM_STREAM_CAPTURE && !is_capture) continue;
            if (stream == SND_PCM_STREAM_CAPTURE && descr1 &&
                (strstr(descr1, "Output") || strstr(descr1, "output")))
            {
                continue;
            }


            struct SoundIoDevicePrivate *dev = ALLOCATE(struct SoundIoDevicePrivate, 1);
            if (!dev) {
                free(name);
                free(descr);
                soundio_destroy_devices_info(devices_info);
                snd_device_name_free_hint(hints);
                return SoundIoErrorNoMem;
            }
            struct SoundIoDevice *device = &dev->pub;
            device->ref_count = 1;
            device->soundio = soundio;
            device->is_raw = false;
            device->id = strdup(name);
            if (descr1) {
                device->name = soundio_alloc_sprintf(NULL, "%s: %s", descr, descr1);
            } else if (descr) {
                device->name = strdup(descr);
            } else {
                device->name = strdup(name);
            }

            if (!device->id || !device->name) {
                soundio_device_unref(device);
                free(name);
                free(descr);
                soundio_destroy_devices_info(devices_info);
                snd_device_name_free_hint(hints);
                return SoundIoErrorNoMem;
            }

            struct SoundIoListDevicePtr *device_list;
            bool is_default = str_has_prefix(name, "default:") || strcmp(name, "default") == 0;
            bool is_sysdefault = str_has_prefix(name, "sysdefault:") || strcmp(name, "sysdefault") == 0;

            if (stream == SND_PCM_STREAM_PLAYBACK) {
                device->aim = SoundIoDeviceAimOutput;
                device_list = &devices_info->output_devices;
                if (is_default)
                    default_output_index = device_list->length;
                if (is_sysdefault)
                    sysdefault_output_index = device_list->length;
                if (devices_info->default_output_index == -1)
                    devices_info->default_output_index = device_list->length;
            } else {
                assert(stream == SND_PCM_STREAM_CAPTURE);
                device->aim = SoundIoDeviceAimInput;
                device_list = &devices_info->input_devices;
                if (is_default)
                    default_input_index = device_list->length;
                if (is_sysdefault)
                    sysdefault_input_index = device_list->length;
                if (devices_info->default_input_index == -1)
                    devices_info->default_input_index = device_list->length;
            }

            device->probe_error = probe_device(device, NULL);

            if (SoundIoListDevicePtr_append(device_list, device)) {
                soundio_device_unref(device);
                free(name);
                free(descr);
                soundio_destroy_devices_info(devices_info);
                snd_device_name_free_hint(hints);
                return SoundIoErrorNoMem;
            }
        }

        free(name);
        free(descr);
    }

    if (default_input_index >= 0) {
        devices_info->default_input_index = default_input_index;
    } else if (sysdefault_input_index >= 0) {
        devices_info->default_input_index = sysdefault_input_index;
    }

    if (default_output_index >= 0) {
        devices_info->default_output_index = default_output_index;
    } else if (sysdefault_output_index >= 0) {
        devices_info->default_output_index = sysdefault_output_index;
    }

    snd_device_name_free_hint(hints);

    int card_index = -1;

    if (snd_card_next(&card_index) < 0)
        return SoundIoErrorSystemResources;

    snd_ctl_card_info_t *card_info;
    snd_ctl_card_info_alloca(&card_info);

    snd_pcm_info_t *pcm_info;
    snd_pcm_info_alloca(&pcm_info);

    while (card_index >= 0) {
        int err;
        snd_ctl_t *handle;
        char name[32];
        sprintf(name, "hw:%d", card_index);
        if ((err = snd_ctl_open(&handle, name, 0)) < 0) {
            if (err == -ENOENT) {
                break;
            } else {
                soundio_destroy_devices_info(devices_info);
                return SoundIoErrorOpeningDevice;
            }
        }

        if ((err = snd_ctl_card_info(handle, card_info)) < 0) {
            snd_ctl_close(handle);
            soundio_destroy_devices_info(devices_info);
            return SoundIoErrorSystemResources;
        }
        const char *card_name = snd_ctl_card_info_get_name(card_info);

        int device_index = -1;
        for (;;) {
            if (snd_ctl_pcm_next_device(handle, &device_index) < 0) {
                snd_ctl_close(handle);
                soundio_destroy_devices_info(devices_info);
                return SoundIoErrorSystemResources;
            }
            if (device_index < 0)
                break;

            snd_pcm_info_set_device(pcm_info, device_index);
            snd_pcm_info_set_subdevice(pcm_info, 0);

            for (int stream_type_i = 0; stream_type_i < ARRAY_LENGTH(stream_types); stream_type_i += 1) {
                snd_pcm_stream_t stream = stream_types[stream_type_i];
                snd_pcm_info_set_stream(pcm_info, stream);

                if ((err = snd_ctl_pcm_info(handle, pcm_info)) < 0) {
                    if (err == -ENOENT) {
                        continue;
                    } else {
                        snd_ctl_close(handle);
                        soundio_destroy_devices_info(devices_info);
                        return SoundIoErrorSystemResources;
                    }
                }

                const char *device_name = snd_pcm_info_get_name(pcm_info);

                struct SoundIoDevicePrivate *dev = ALLOCATE(struct SoundIoDevicePrivate, 1);
                if (!dev) {
                    snd_ctl_close(handle);
                    soundio_destroy_devices_info(devices_info);
                    return SoundIoErrorNoMem;
                }
                struct SoundIoDevice *device = &dev->pub;
                device->ref_count = 1;
                device->soundio = soundio;
                device->id = soundio_alloc_sprintf(NULL, "hw:%d,%d", card_index, device_index);
                device->name = soundio_alloc_sprintf(NULL, "%s %s", card_name, device_name);
                device->is_raw = true;

                if (!device->id || !device->name) {
                    soundio_device_unref(device);
                    snd_ctl_close(handle);
                    soundio_destroy_devices_info(devices_info);
                    return SoundIoErrorNoMem;
                }

                struct SoundIoListDevicePtr *device_list;
                if (stream == SND_PCM_STREAM_PLAYBACK) {
                    device->aim = SoundIoDeviceAimOutput;
                    device_list = &devices_info->output_devices;
                } else {
                    assert(stream == SND_PCM_STREAM_CAPTURE);
                    device->aim = SoundIoDeviceAimInput;
                    device_list = &devices_info->input_devices;
                }

                snd_pcm_chmap_query_t **maps = snd_pcm_query_chmaps_from_hw(card_index, device_index, -1, stream);
                device->probe_error = probe_device(device, maps);

                if (SoundIoListDevicePtr_append(device_list, device)) {
                    soundio_device_unref(device);
                    soundio_destroy_devices_info(devices_info);
                    return SoundIoErrorNoMem;
                }
            }
        }
        snd_ctl_close(handle);
        if (snd_card_next(&card_index) < 0) {
            soundio_destroy_devices_info(devices_info);
            return SoundIoErrorSystemResources;
        }
    }

    soundio_os_mutex_lock(sia->mutex);
    soundio_destroy_devices_info(sia->ready_devices_info);
    sia->ready_devices_info = devices_info;
    sia->have_devices_flag = true;
    soundio_os_cond_signal(sia->cond, sia->mutex);
    soundio->on_events_signal(soundio);
    soundio_os_mutex_unlock(sia->mutex);
    return 0;
}

static void shutdown_backend(struct SoundIoPrivate *si, int err) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoAlsa *sia = &si->backend_data.alsa;
    soundio_os_mutex_lock(sia->mutex);
    sia->shutdown_err = err;
    soundio_os_cond_signal(sia->cond, sia->mutex);
    soundio->on_events_signal(soundio);
    soundio_os_mutex_unlock(sia->mutex);
}

static bool copy_str(char *dest, const char *src, int buf_len) {
    for (;;) {
        buf_len -= 1;
        if (buf_len <= 0)
            return false;
        *dest = *src;
        dest += 1;
        src += 1;
        if (!*src)
            break;
    }
    *dest = '\0';
    return true;
}

static void device_thread_run(void *arg) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)arg;
    struct SoundIoAlsa *sia = &si->backend_data.alsa;

    // Some systems cannot read integer variables if they are not
    // properly aligned. On other systems, incorrect alignment may
    // decrease performance. Hence, the buffer used for reading from
    // the inotify file descriptor should have the same alignment as
    // struct inotify_event.
    char buf[4096] __attribute__ ((aligned(__alignof__(struct inotify_event))));
    const struct inotify_event *event;

    struct pollfd fds[2];
    fds[0].fd = sia->notify_fd;
    fds[0].events = POLLIN;

    fds[1].fd = sia->notify_pipe_fd[0];
    fds[1].events = POLLIN;

    int err;
    for (;;) {
        int poll_num = poll(fds, 2, -1);
        if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(sia->abort_flag))
            break;
        if (poll_num == -1) {
            if (errno == EINTR)
                continue;
            assert(errno != EFAULT);
            assert(errno != EINVAL);
            assert(errno == ENOMEM);
            // Kernel ran out of polling memory.
            shutdown_backend(si, SoundIoErrorSystemResources);
            return;
        }
        if (poll_num <= 0)
            continue;
        bool got_rescan_event = false;
        if (fds[0].revents & POLLIN) {
            for (;;) {
                ssize_t len = read(sia->notify_fd, buf, sizeof(buf));
                if (len == -1) {
                    assert(errno != EBADF);
                    assert(errno != EFAULT);
                    assert(errno != EINVAL);
                    assert(errno != EIO);
                    assert(errno != EISDIR);
                    if (errno == EBADF || errno == EFAULT || errno == EINVAL ||
                        errno == EIO || errno == EISDIR)
                    {
                        shutdown_backend(si, SoundIoErrorSystemResources);
                        return;
                    }
                }

                // catches EINTR and EAGAIN
                if (len <= 0)
                    break;

                // loop over all events in the buffer
                for (char *ptr = buf; ptr < buf + len; ptr += sizeof(struct inotify_event) + event->len) {
                    event = (const struct inotify_event *) ptr;

                    if (!((event->mask & IN_CLOSE_WRITE) || (event->mask & IN_DELETE) || (event->mask & IN_CREATE)))
                        continue;
                    if (event->mask & IN_ISDIR)
                        continue;
                    if (!event->len || event->len < 8)
                        continue;
                    if (strncmp(event->name, "controlC", 8) != 0) {
                        continue;
                    }
                    if (event->mask & IN_CREATE) {
                        if ((err = SoundIoListAlsaPendingFile_add_one(&sia->pending_files))) {
                            shutdown_backend(si, SoundIoErrorNoMem);
                            return;
                        }
                        struct SoundIoAlsaPendingFile *pending_file =
                            SoundIoListAlsaPendingFile_last_ptr(&sia->pending_files);
                        if (!copy_str(pending_file->name, event->name, SOUNDIO_MAX_ALSA_SND_FILE_LEN)) {
                            SoundIoListAlsaPendingFile_pop(&sia->pending_files);
                        }
                        continue;
                    }
                    if (sia->pending_files.length > 0) {
                        // At this point ignore IN_DELETE in favor of waiting until the files
                        // opened with IN_CREATE have their IN_CLOSE_WRITE event.
                        if (!(event->mask & IN_CLOSE_WRITE))
                            continue;
                        for (int i = 0; i < sia->pending_files.length; i += 1) {
                            struct SoundIoAlsaPendingFile *pending_file =
                                SoundIoListAlsaPendingFile_ptr_at(&sia->pending_files, i);
                            if (strcmp(pending_file->name, event->name) == 0) {
                                SoundIoListAlsaPendingFile_swap_remove(&sia->pending_files, i);
                                if (sia->pending_files.length == 0) {
                                    got_rescan_event = true;
                                }
                                break;
                            }
                        }
                    } else if (event->mask & IN_DELETE) {
                        // We are not waiting on created files to be closed, so when
                        // a delete happens we act on it.
                        got_rescan_event = true;
                    }
                }
            }
        }
        if (fds[1].revents & POLLIN) {
            got_rescan_event = true;
            for (;;) {
                ssize_t len = read(sia->notify_pipe_fd[0], buf, sizeof(buf));
                if (len == -1) {
                    assert(errno != EBADF);
                    assert(errno != EFAULT);
                    assert(errno != EINVAL);
                    assert(errno != EIO);
                    assert(errno != EISDIR);
                    if (errno == EBADF || errno == EFAULT || errno == EINVAL ||
                        errno == EIO || errno == EISDIR)
                    {
                        shutdown_backend(si, SoundIoErrorSystemResources);
                        return;
                    }
                }
                if (len <= 0)
                    break;
            }
        }
        if (got_rescan_event) {
            if ((err = alsa_refresh_devices(si))) {
                shutdown_backend(si, err);
                return;
            }
        }
    }
}

static void alsa_my_flush_events(struct SoundIoPrivate *si, bool wait) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoAlsa *sia = &si->backend_data.alsa;

    bool change = false;
    bool cb_shutdown = false;
    struct SoundIoDevicesInfo *old_devices_info = NULL;

    soundio_os_mutex_lock(sia->mutex);

    // block until have devices
    while (wait || (!sia->have_devices_flag && !sia->shutdown_err)) {
        soundio_os_cond_wait(sia->cond, sia->mutex);
        wait = false;
    }

    if (sia->shutdown_err && !sia->emitted_shutdown_cb) {
        sia->emitted_shutdown_cb = true;
        cb_shutdown = true;
    } else if (sia->ready_devices_info) {
        old_devices_info = si->safe_devices_info;
        si->safe_devices_info = sia->ready_devices_info;
        sia->ready_devices_info = NULL;
        change = true;
    }

    soundio_os_mutex_unlock(sia->mutex);

    if (cb_shutdown)
        soundio->on_backend_disconnect(soundio, sia->shutdown_err);
    else if (change)
        soundio->on_devices_change(soundio);

    soundio_destroy_devices_info(old_devices_info);
}

static void flush_events_alsa(struct SoundIoPrivate *si) {
    alsa_my_flush_events(si, false);
}

static void wait_events_alsa(struct SoundIoPrivate *si) {
    alsa_my_flush_events(si, false);
    alsa_my_flush_events(si, true);
}

static void wakeup_alsa(struct SoundIoPrivate *si) {
    struct SoundIoAlsa *sia = &si->backend_data.alsa;
    soundio_os_mutex_lock(sia->mutex);
    soundio_os_cond_signal(sia->cond, sia->mutex);
    soundio_os_mutex_unlock(sia->mutex);
}

static void force_device_scan_alsa(struct SoundIoPrivate *si) {
    struct SoundIoAlsa *sia = &si->backend_data.alsa;
    wakeup_device_poll(sia);
}

static void outstream_destroy_alsa(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamAlsa *osa = &os->backend_data.alsa;

    if (osa->thread) {
        SOUNDIO_ATOMIC_FLAG_CLEAR(osa->thread_exit_flag);
        wakeup_outstream_poll(osa);
        soundio_os_thread_destroy(osa->thread);
        osa->thread = NULL;
    }

    if (osa->handle) {
        snd_pcm_close(osa->handle);
        osa->handle = NULL;
    }

    free(osa->poll_fds);
    osa->poll_fds = NULL;

    free(osa->chmap);
    osa->chmap = NULL;

    free(osa->sample_buffer);
    osa->sample_buffer = NULL;
}

static int outstream_xrun_recovery(struct SoundIoOutStreamPrivate *os, int err) {
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoOutStreamAlsa *osa = &os->backend_data.alsa;
    if (err == -EPIPE) {
        err = snd_pcm_prepare(osa->handle);
        if (err >= 0)
            outstream->underflow_callback(outstream);
    } else if (err == -ESTRPIPE) {
        while ((err = snd_pcm_resume(osa->handle)) == -EAGAIN) {
            // wait until suspend flag is released
            poll(NULL, 0, 1);
        }
        if (err < 0)
            err = snd_pcm_prepare(osa->handle);
        if (err >= 0)
            outstream->underflow_callback(outstream);
    }
    return err;
}

static int instream_xrun_recovery(struct SoundIoInStreamPrivate *is, int err) {
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoInStreamAlsa *isa = &is->backend_data.alsa;
    if (err == -EPIPE) {
        err = snd_pcm_prepare(isa->handle);
        if (err >= 0)
            instream->overflow_callback(instream);
    } else if (err == -ESTRPIPE) {
        while ((err = snd_pcm_resume(isa->handle)) == -EAGAIN) {
            // wait until suspend flag is released
            poll(NULL, 0, 1);
        }
        if (err < 0)
            err = snd_pcm_prepare(isa->handle);
        if (err >= 0)
            instream->overflow_callback(instream);
    }
    return err;
}

static int outstream_wait_for_poll(struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamAlsa *osa = &os->backend_data.alsa;
    int err;
    unsigned short revents;
    for (;;) {
        if ((err = poll(osa->poll_fds, osa->poll_fd_count_with_extra, -1)) < 0) {
            return SoundIoErrorStreaming;
        }
        if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osa->thread_exit_flag))
            return SoundIoErrorInterrupted;
        if ((err = snd_pcm_poll_descriptors_revents(osa->handle,
                        osa->poll_fds, osa->poll_fd_count, &revents)) < 0)
        {
            return SoundIoErrorStreaming;
        }
        if (revents & (POLLERR|POLLNVAL|POLLHUP)) {
            return 0;
        }
        if (revents & POLLOUT)
            return 0;
    }
}

static int instream_wait_for_poll(struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamAlsa *isa = &is->backend_data.alsa;
    int err;
    unsigned short revents;
    for (;;) {
        if ((err = poll(isa->poll_fds, isa->poll_fd_count, -1)) < 0) {
            return err;
        }
        if ((err = snd_pcm_poll_descriptors_revents(isa->handle,
                        isa->poll_fds, isa->poll_fd_count, &revents)) < 0)
        {
            return err;
        }
        if (revents & (POLLERR|POLLNVAL|POLLHUP)) {
            return 0;
        }
        if (revents & POLLIN)
            return 0;
    }
}

static void outstream_thread_run(void *arg) {
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *) arg;
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoOutStreamAlsa *osa = &os->backend_data.alsa;

    int err;

    for (;;) {
        snd_pcm_state_t state = snd_pcm_state(osa->handle);
        switch (state) {
            case SND_PCM_STATE_SETUP:
            {
                if ((err = snd_pcm_prepare(osa->handle)) < 0) {
                    outstream->error_callback(outstream, SoundIoErrorStreaming);
                    return;
                }
                continue;
            }
            case SND_PCM_STATE_PREPARED:
            {
                snd_pcm_sframes_t avail = snd_pcm_avail(osa->handle);
                if (avail < 0) {
                    outstream->error_callback(outstream, SoundIoErrorStreaming);
                    return;
                }

                if ((snd_pcm_uframes_t)avail == osa->buffer_size_frames) {
                    outstream->write_callback(outstream, 0, avail);
                    if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osa->thread_exit_flag))
                        return;
                    continue;
                }

                if ((err = snd_pcm_start(osa->handle)) < 0) {
                    outstream->error_callback(outstream, SoundIoErrorStreaming);
                    return;
                }
                continue;
            }
            case SND_PCM_STATE_RUNNING:
            case SND_PCM_STATE_PAUSED:
            {
                if ((err = outstream_wait_for_poll(os))) {
                    if (err == SoundIoErrorInterrupted)
                        return;
                    outstream->error_callback(outstream, err);
                    return;
                }
                if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osa->thread_exit_flag))
                    return;
                if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osa->clear_buffer_flag)) {
                    if ((err = snd_pcm_drop(osa->handle)) < 0) {
                        outstream->error_callback(outstream, SoundIoErrorStreaming);
                        return;
                    }
                    if ((err = snd_pcm_reset(osa->handle)) < 0) {
                        if (err == -EBADFD) {
                            // If this happens the snd_pcm_drop will have done
                            // the function of the reset so it's ok that this
                            // did not work.
                        } else {
                            outstream->error_callback(outstream, SoundIoErrorStreaming);
                            return;
                        }
                    }
                    continue;
                }

                snd_pcm_sframes_t avail = snd_pcm_avail_update(osa->handle);
                if (avail < 0) {
                    if ((err = outstream_xrun_recovery(os, avail)) < 0) {
                        outstream->error_callback(outstream, SoundIoErrorStreaming);
                        return;
                    }
                    continue;
                }

                if (avail > 0)
                    outstream->write_callback(outstream, 0, avail);
                continue;
            }
            case SND_PCM_STATE_XRUN:
                if ((err = outstream_xrun_recovery(os, -EPIPE)) < 0) {
                    outstream->error_callback(outstream, SoundIoErrorStreaming);
                    return;
                }
                continue;
            case SND_PCM_STATE_SUSPENDED:
                if ((err = outstream_xrun_recovery(os, -ESTRPIPE)) < 0) {
                    outstream->error_callback(outstream, SoundIoErrorStreaming);
                    return;
                }
                continue;
            case SND_PCM_STATE_OPEN:
            case SND_PCM_STATE_DRAINING:
            case SND_PCM_STATE_DISCONNECTED:
                outstream->error_callback(outstream, SoundIoErrorStreaming);
                return;
        }
    }
}

static void instream_thread_run(void *arg) {
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *) arg;
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoInStreamAlsa *isa = &is->backend_data.alsa;

    int err;

    for (;;) {
        snd_pcm_state_t state = snd_pcm_state(isa->handle);
        switch (state) {
            case SND_PCM_STATE_SETUP:
                if ((err = snd_pcm_prepare(isa->handle)) < 0) {
                    instream->error_callback(instream, SoundIoErrorStreaming);
                    return;
                }
                continue;
            case SND_PCM_STATE_PREPARED:
                if ((err = snd_pcm_start(isa->handle)) < 0) {
                    instream->error_callback(instream, SoundIoErrorStreaming);
                    return;
                }
                continue;
            case SND_PCM_STATE_RUNNING:
            case SND_PCM_STATE_PAUSED:
            {
                if ((err = instream_wait_for_poll(is)) < 0) {
                    if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(isa->thread_exit_flag))
                        return;
                    instream->error_callback(instream, SoundIoErrorStreaming);
                    return;
                }
                if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(isa->thread_exit_flag))
                    return;

                snd_pcm_sframes_t avail = snd_pcm_avail_update(isa->handle);

                if (avail < 0) {
                    if ((err = instream_xrun_recovery(is, avail)) < 0) {
                        instream->error_callback(instream, SoundIoErrorStreaming);
                        return;
                    }
                    continue;
                }

                if (avail > 0)
                    instream->read_callback(instream, 0, avail);
                continue;
            }
            case SND_PCM_STATE_XRUN:
                if ((err = instream_xrun_recovery(is, -EPIPE)) < 0) {
                    instream->error_callback(instream, SoundIoErrorStreaming);
                    return;
                }
                continue;
            case SND_PCM_STATE_SUSPENDED:
                if ((err = instream_xrun_recovery(is, -ESTRPIPE)) < 0) {
                    instream->error_callback(instream, SoundIoErrorStreaming);
                    return;
                }
                continue;
            case SND_PCM_STATE_OPEN:
            case SND_PCM_STATE_DRAINING:
            case SND_PCM_STATE_DISCONNECTED:
                instream->error_callback(instream, SoundIoErrorStreaming);
                return;
        }
    }
}

static int outstream_open_alsa(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamAlsa *osa = &os->backend_data.alsa;
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoDevice *device = outstream->device;

    SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osa->clear_buffer_flag);

    if (outstream->software_latency == 0.0)
        outstream->software_latency = 1.0;
    outstream->software_latency = soundio_double_clamp(device->software_latency_min, outstream->software_latency, device->software_latency_max);

    int ch_count = outstream->layout.channel_count;

    osa->chmap_size = sizeof(int) + sizeof(int) * ch_count;
    osa->chmap = (snd_pcm_chmap_t *)ALLOCATE(char, osa->chmap_size);
    if (!osa->chmap) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorNoMem;
    }

    int err;

    snd_pcm_hw_params_t *hwparams;
    snd_pcm_hw_params_alloca(&hwparams);

    snd_pcm_stream_t stream = aim_to_stream(outstream->device->aim);

    if ((err = snd_pcm_open(&osa->handle, outstream->device->id, stream, 0)) < 0) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorOpeningDevice;
    }

    if ((err = snd_pcm_hw_params_any(osa->handle, hwparams)) < 0) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorOpeningDevice;
    }

    int want_resample = !outstream->device->is_raw;
    if ((err = snd_pcm_hw_params_set_rate_resample(osa->handle, hwparams, want_resample)) < 0) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorOpeningDevice;
    }

    if ((err = set_access(osa->handle, hwparams, &osa->access))) {
        outstream_destroy_alsa(si, os);
        return err;
    }

    if ((err = snd_pcm_hw_params_set_channels(osa->handle, hwparams, ch_count)) < 0) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorOpeningDevice;
    }

    if ((err = snd_pcm_hw_params_set_rate(osa->handle, hwparams, outstream->sample_rate, 0)) < 0) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorOpeningDevice;
    }

    snd_pcm_format_t format = to_alsa_fmt(outstream->format);
    int phys_bits_per_sample = snd_pcm_format_physical_width(format);
    if (phys_bits_per_sample % 8 != 0) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorIncompatibleDevice;
    }
    int phys_bytes_per_sample = phys_bits_per_sample / 8;
    if ((err = snd_pcm_hw_params_set_format(osa->handle, hwparams, format)) < 0) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorOpeningDevice;
    }

    osa->buffer_size_frames = outstream->software_latency * outstream->sample_rate;
    if ((err = snd_pcm_hw_params_set_buffer_size_near(osa->handle, hwparams, &osa->buffer_size_frames)) < 0) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorOpeningDevice;
    }
    outstream->software_latency = ((double)osa->buffer_size_frames) / (double)outstream->sample_rate;

    // write the hardware parameters to device
    if ((err = snd_pcm_hw_params(osa->handle, hwparams)) < 0) {
        outstream_destroy_alsa(si, os);
        return (err == -EINVAL) ? SoundIoErrorIncompatibleDevice : SoundIoErrorOpeningDevice;
    }

    if ((snd_pcm_hw_params_get_period_size(hwparams, &osa->period_size, NULL)) < 0) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorOpeningDevice;
    }


    // set channel map
    osa->chmap->channels = ch_count;
    for (int i = 0; i < ch_count; i += 1) {
        osa->chmap->pos[i] = to_alsa_chmap_pos(outstream->layout.channels[i]);
    }
    if ((err = snd_pcm_set_chmap(osa->handle, osa->chmap)) < 0)
        outstream->layout_error = SoundIoErrorIncompatibleDevice;

    // get current swparams
    snd_pcm_sw_params_t *swparams;
    snd_pcm_sw_params_alloca(&swparams);

    if ((err = snd_pcm_sw_params_current(osa->handle, swparams)) < 0) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorOpeningDevice;
    }

    if ((err = snd_pcm_sw_params_set_start_threshold(osa->handle, swparams, 0)) < 0) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorOpeningDevice;
    }

    if ((err = snd_pcm_sw_params_set_avail_min(osa->handle, swparams, osa->period_size)) < 0) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorOpeningDevice;
    }

    // write the software parameters to device
    if ((err = snd_pcm_sw_params(osa->handle, swparams)) < 0) {
        outstream_destroy_alsa(si, os);
        return (err == -EINVAL) ? SoundIoErrorIncompatibleDevice : SoundIoErrorOpeningDevice;
    }

    if (osa->access == SND_PCM_ACCESS_RW_INTERLEAVED || osa->access == SND_PCM_ACCESS_RW_NONINTERLEAVED) {
        osa->sample_buffer_size = ch_count * osa->period_size * phys_bytes_per_sample;
        osa->sample_buffer = ALLOCATE_NONZERO(char, osa->sample_buffer_size);
        if (!osa->sample_buffer) {
            outstream_destroy_alsa(si, os);
            return SoundIoErrorNoMem;
        }
    }

    osa->poll_fd_count = snd_pcm_poll_descriptors_count(osa->handle);
    if (osa->poll_fd_count <= 0) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorOpeningDevice;
    }

    osa->poll_fd_count_with_extra = osa->poll_fd_count + 1;
    osa->poll_fds = ALLOCATE(struct pollfd, osa->poll_fd_count_with_extra);
    if (!osa->poll_fds) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorNoMem;
    }

    if ((err = snd_pcm_poll_descriptors(osa->handle, osa->poll_fds, osa->poll_fd_count)) < 0) {
        outstream_destroy_alsa(si, os);
        return SoundIoErrorOpeningDevice;
    }

    struct pollfd *extra_fd = &osa->poll_fds[osa->poll_fd_count];
    if (pipe2(osa->poll_exit_pipe_fd, O_NONBLOCK)) {
        assert(errno != EFAULT);
        assert(errno != EINVAL);
        assert(errno == EMFILE || errno == ENFILE);
        outstream_destroy_alsa(si, os);
        return SoundIoErrorSystemResources;
    }
    extra_fd->fd = osa->poll_exit_pipe_fd[0];
    extra_fd->events = POLLIN;

    return 0;
}

static int outstream_start_alsa(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamAlsa *osa = &os->backend_data.alsa;
    struct SoundIo *soundio = &si->pub;

    assert(!osa->thread);

    int err;
    SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osa->thread_exit_flag);
    if ((err = soundio_os_thread_create(outstream_thread_run, os, soundio->emit_rtprio_warning, &osa->thread)))
        return err;

    return 0;
}

static int outstream_begin_write_alsa(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os,
        struct SoundIoChannelArea **out_areas, int *frame_count)
{
    *out_areas = NULL;
    struct SoundIoOutStreamAlsa *osa = &os->backend_data.alsa;
    struct SoundIoOutStream *outstream = &os->pub;

    if (osa->access == SND_PCM_ACCESS_RW_INTERLEAVED) {
        for (int ch = 0; ch < outstream->layout.channel_count; ch += 1) {
            osa->areas[ch].ptr = osa->sample_buffer + ch * outstream->bytes_per_sample;
            osa->areas[ch].step = outstream->bytes_per_frame;
        }

        osa->write_frame_count = soundio_int_min(*frame_count, osa->period_size);
        *frame_count = osa->write_frame_count;
    } else if (osa->access == SND_PCM_ACCESS_RW_NONINTERLEAVED) {
        for (int ch = 0; ch < outstream->layout.channel_count; ch += 1) {
            osa->areas[ch].ptr = osa->sample_buffer + ch * outstream->bytes_per_sample * osa->period_size;
            osa->areas[ch].step = outstream->bytes_per_sample;
        }

        osa->write_frame_count = soundio_int_min(*frame_count, osa->period_size);
        *frame_count = osa->write_frame_count;
    } else {
        const snd_pcm_channel_area_t *areas;
        snd_pcm_uframes_t frames = *frame_count;
        int err;

        if ((err = snd_pcm_mmap_begin(osa->handle, &areas, &osa->offset, &frames)) < 0) {
            if (err == -EPIPE || err == -ESTRPIPE)
                return SoundIoErrorUnderflow;
            else
                return SoundIoErrorStreaming;
        }

        for (int ch = 0; ch < outstream->layout.channel_count; ch += 1) {
            if ((areas[ch].first % 8 != 0) || (areas[ch].step % 8 != 0))
                return SoundIoErrorIncompatibleDevice;
            osa->areas[ch].step = areas[ch].step / 8;
            osa->areas[ch].ptr = ((char *)areas[ch].addr) + (areas[ch].first / 8) +
                (osa->areas[ch].step * osa->offset);
        }

        osa->write_frame_count = frames;
        *frame_count = osa->write_frame_count;
    }

    *out_areas = osa->areas;
    return 0;
}

static int outstream_end_write_alsa(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamAlsa *osa = &os->backend_data.alsa;
    struct SoundIoOutStream *outstream = &os->pub;

    snd_pcm_sframes_t commitres;
    if (osa->access == SND_PCM_ACCESS_RW_INTERLEAVED) {
        commitres = snd_pcm_writei(osa->handle, osa->sample_buffer, osa->write_frame_count);
    } else if (osa->access == SND_PCM_ACCESS_RW_NONINTERLEAVED) {
        char *ptrs[SOUNDIO_MAX_CHANNELS];
        for (int ch = 0; ch < outstream->layout.channel_count; ch += 1) {
            ptrs[ch] = osa->sample_buffer + ch * outstream->bytes_per_sample * osa->period_size;
        }
        commitres = snd_pcm_writen(osa->handle, (void**)ptrs, osa->write_frame_count);
    } else {
        commitres = snd_pcm_mmap_commit(osa->handle, osa->offset, osa->write_frame_count);
    }

    if (commitres < 0 || commitres != osa->write_frame_count) {
        int err = (commitres >= 0) ? -EPIPE : commitres;
        if (err == -EPIPE || err == -ESTRPIPE)
            return SoundIoErrorUnderflow;
        else
            return SoundIoErrorStreaming;
    }
    return 0;
}

static int outstream_clear_buffer_alsa(struct SoundIoPrivate *si,
        struct SoundIoOutStreamPrivate *os)
{
    struct SoundIoOutStreamAlsa *osa = &os->backend_data.alsa;
    SOUNDIO_ATOMIC_FLAG_CLEAR(osa->clear_buffer_flag);
    return 0;
}

static int outstream_pause_alsa(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os, bool pause) {
    if (!si)
        return SoundIoErrorInvalid;

    struct SoundIoOutStreamAlsa *osa = &os->backend_data.alsa;

    if (!osa->handle)
        return SoundIoErrorInvalid;

    if (osa->is_paused == pause)
        return 0;

    int err;
    if ((err = snd_pcm_pause(osa->handle, pause)) < 0) {
        return SoundIoErrorIncompatibleDevice;
    }

    osa->is_paused = pause;
    return 0;
}

static int outstream_get_latency_alsa(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os,
        double *out_latency)
{
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoOutStreamAlsa *osa = &os->backend_data.alsa;
    int err;

    snd_pcm_sframes_t delay;
    if ((err = snd_pcm_delay(osa->handle, &delay)) < 0) {
        return SoundIoErrorStreaming;
    }

    *out_latency = delay / (double)outstream->sample_rate;
    return 0;
}

static void instream_destroy_alsa(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamAlsa *isa = &is->backend_data.alsa;

    if (isa->thread) {
        SOUNDIO_ATOMIC_FLAG_CLEAR(isa->thread_exit_flag);
        soundio_os_thread_destroy(isa->thread);
        isa->thread = NULL;
    }

    if (isa->handle) {
        snd_pcm_close(isa->handle);
        isa->handle = NULL;
    }

    free(isa->poll_fds);
    isa->poll_fds = NULL;

    free(isa->chmap);
    isa->chmap = NULL;

    free(isa->sample_buffer);
    isa->sample_buffer = NULL;
}

static int instream_open_alsa(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamAlsa *isa = &is->backend_data.alsa;
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoDevice *device = instream->device;

    if (instream->software_latency == 0.0)
        instream->software_latency = 1.0;
    instream->software_latency = soundio_double_clamp(device->software_latency_min, instream->software_latency, device->software_latency_max);

    int ch_count = instream->layout.channel_count;

    isa->chmap_size = sizeof(int) + sizeof(int) * ch_count;
    isa->chmap = (snd_pcm_chmap_t *)ALLOCATE(char, isa->chmap_size);
    if (!isa->chmap) {
        instream_destroy_alsa(si, is);
        return SoundIoErrorNoMem;
    }

    int err;

    snd_pcm_hw_params_t *hwparams;
    snd_pcm_hw_params_alloca(&hwparams);

    snd_pcm_stream_t stream = aim_to_stream(instream->device->aim);

    if ((err = snd_pcm_open(&isa->handle, instream->device->id, stream, 0)) < 0) {
        instream_destroy_alsa(si, is);
        return SoundIoErrorOpeningDevice;
    }

    if ((err = snd_pcm_hw_params_any(isa->handle, hwparams)) < 0) {
        instream_destroy_alsa(si, is);
        return SoundIoErrorOpeningDevice;
    }

    int want_resample = !instream->device->is_raw;
    if ((err = snd_pcm_hw_params_set_rate_resample(isa->handle, hwparams, want_resample)) < 0) {
        instream_destroy_alsa(si, is);
        return SoundIoErrorOpeningDevice;
    }

    if ((err = set_access(isa->handle, hwparams, &isa->access))) {
        instream_destroy_alsa(si, is);
        return err;
    }

    if ((err = snd_pcm_hw_params_set_channels(isa->handle, hwparams, ch_count)) < 0) {
        instream_destroy_alsa(si, is);
        return SoundIoErrorOpeningDevice;
    }

    if ((err = snd_pcm_hw_params_set_rate(isa->handle, hwparams, instream->sample_rate, 0)) < 0) {
        instream_destroy_alsa(si, is);
        return SoundIoErrorOpeningDevice;
    }

    snd_pcm_format_t format = to_alsa_fmt(instream->format);
    int phys_bits_per_sample = snd_pcm_format_physical_width(format);
    if (phys_bits_per_sample % 8 != 0) {
        instream_destroy_alsa(si, is);
        return SoundIoErrorIncompatibleDevice;
    }
    int phys_bytes_per_sample = phys_bits_per_sample / 8;
    if ((err = snd_pcm_hw_params_set_format(isa->handle, hwparams, format)) < 0) {
        instream_destroy_alsa(si, is);
        return SoundIoErrorOpeningDevice;
    }

    snd_pcm_uframes_t period_frames = ceil_dbl_to_uframes(0.5 * instream->software_latency * (double)instream->sample_rate);
    if ((err = snd_pcm_hw_params_set_period_size_near(isa->handle, hwparams, &period_frames, NULL)) < 0) {
        instream_destroy_alsa(si, is);
        return SoundIoErrorOpeningDevice;
    }
    instream->software_latency = ((double)period_frames) / (double)instream->sample_rate;
    isa->period_size = period_frames;


    snd_pcm_uframes_t buffer_size_frames;
    if ((err = snd_pcm_hw_params_set_buffer_size_last(isa->handle, hwparams, &buffer_size_frames)) < 0) {
        instream_destroy_alsa(si, is);
        return SoundIoErrorOpeningDevice;
    }

    // write the hardware parameters to device
    if ((err = snd_pcm_hw_params(isa->handle, hwparams)) < 0) {
        instream_destroy_alsa(si, is);
        return (err == -EINVAL) ? SoundIoErrorIncompatibleDevice : SoundIoErrorOpeningDevice;
    }

    // set channel map
    isa->chmap->channels = ch_count;
    for (int i = 0; i < ch_count; i += 1) {
        isa->chmap->pos[i] = to_alsa_chmap_pos(instream->layout.channels[i]);
    }
    if ((err = snd_pcm_set_chmap(isa->handle, isa->chmap)) < 0)
        instream->layout_error = SoundIoErrorIncompatibleDevice;

    // get current swparams
    snd_pcm_sw_params_t *swparams;
    snd_pcm_sw_params_alloca(&swparams);

    if ((err = snd_pcm_sw_params_current(isa->handle, swparams)) < 0) {
        instream_destroy_alsa(si, is);
        return SoundIoErrorOpeningDevice;
    }

    // write the software parameters to device
    if ((err = snd_pcm_sw_params(isa->handle, swparams)) < 0) {
        instream_destroy_alsa(si, is);
        return (err == -EINVAL) ? SoundIoErrorIncompatibleDevice : SoundIoErrorOpeningDevice;
    }

    if (isa->access == SND_PCM_ACCESS_RW_INTERLEAVED || isa->access == SND_PCM_ACCESS_RW_NONINTERLEAVED) {
        isa->sample_buffer_size = ch_count * isa->period_size * phys_bytes_per_sample;
        isa->sample_buffer = ALLOCATE_NONZERO(char, isa->sample_buffer_size);
        if (!isa->sample_buffer) {
            instream_destroy_alsa(si, is);
            return SoundIoErrorNoMem;
        }
    }

    isa->poll_fd_count = snd_pcm_poll_descriptors_count(isa->handle);
    if (isa->poll_fd_count <= 0) {
        instream_destroy_alsa(si, is);
        return SoundIoErrorOpeningDevice;
    }

    isa->poll_fds = ALLOCATE(struct pollfd, isa->poll_fd_count);
    if (!isa->poll_fds) {
        instream_destroy_alsa(si, is);
        return SoundIoErrorNoMem;
    }

    if ((err = snd_pcm_poll_descriptors(isa->handle, isa->poll_fds, isa->poll_fd_count)) < 0) {
        instream_destroy_alsa(si, is);
        return SoundIoErrorOpeningDevice;
    }

    return 0;
}

static int instream_start_alsa(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamAlsa *isa = &is->backend_data.alsa;
    struct SoundIo *soundio = &si->pub;

    assert(!isa->thread);

    SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(isa->thread_exit_flag);
    int err;
    if ((err = soundio_os_thread_create(instream_thread_run, is, soundio->emit_rtprio_warning, &isa->thread))) {
        instream_destroy_alsa(si, is);
        return err;
    }

    return 0;
}

static int instream_begin_read_alsa(struct SoundIoPrivate *si,
        struct SoundIoInStreamPrivate *is, struct SoundIoChannelArea **out_areas, int *frame_count)
{
    *out_areas = NULL;
    struct SoundIoInStreamAlsa *isa = &is->backend_data.alsa;
    struct SoundIoInStream *instream = &is->pub;

    if (isa->access == SND_PCM_ACCESS_RW_INTERLEAVED) {
        for (int ch = 0; ch < instream->layout.channel_count; ch += 1) {
            isa->areas[ch].ptr = isa->sample_buffer + ch * instream->bytes_per_sample;
            isa->areas[ch].step = instream->bytes_per_frame;
        }

        isa->read_frame_count = soundio_int_min(*frame_count, isa->period_size);
        *frame_count = isa->read_frame_count;

        snd_pcm_sframes_t commitres = snd_pcm_readi(isa->handle, isa->sample_buffer, isa->read_frame_count);
        if (commitres < 0 || commitres != isa->read_frame_count) {
            int err = (commitres >= 0) ? -EPIPE : commitres;
            if ((err = instream_xrun_recovery(is, err)) < 0)
                return SoundIoErrorStreaming;
        }
    } else if (isa->access == SND_PCM_ACCESS_RW_NONINTERLEAVED) {
        char *ptrs[SOUNDIO_MAX_CHANNELS];
        for (int ch = 0; ch < instream->layout.channel_count; ch += 1) {
            isa->areas[ch].ptr = isa->sample_buffer + ch * instream->bytes_per_sample * isa->period_size;
            isa->areas[ch].step = instream->bytes_per_sample;
            ptrs[ch] = isa->areas[ch].ptr;
        }

        isa->read_frame_count = soundio_int_min(*frame_count, isa->period_size);
        *frame_count = isa->read_frame_count;

        snd_pcm_sframes_t commitres = snd_pcm_readn(isa->handle, (void**)ptrs, isa->read_frame_count);
        if (commitres < 0 || commitres != isa->read_frame_count) {
            int err = (commitres >= 0) ? -EPIPE : commitres;
            if ((err = instream_xrun_recovery(is, err)) < 0)
                return SoundIoErrorStreaming;
        }
    } else {
        const snd_pcm_channel_area_t *areas;
        snd_pcm_uframes_t frames = *frame_count;
        int err;

        if ((err = snd_pcm_mmap_begin(isa->handle, &areas, &isa->offset, &frames)) < 0) {
            if ((err = instream_xrun_recovery(is, err)) < 0)
                return SoundIoErrorStreaming;
        }

        for (int ch = 0; ch < instream->layout.channel_count; ch += 1) {
            if ((areas[ch].first % 8 != 0) || (areas[ch].step % 8 != 0))
                return SoundIoErrorIncompatibleDevice;
            isa->areas[ch].step = areas[ch].step / 8;
            isa->areas[ch].ptr = ((char *)areas[ch].addr) + (areas[ch].first / 8) +
                (isa->areas[ch].step * isa->offset);
        }

        isa->read_frame_count = frames;
        *frame_count = isa->read_frame_count;
    }

    *out_areas = isa->areas;
    return 0;
}

static int instream_end_read_alsa(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamAlsa *isa = &is->backend_data.alsa;

    if (isa->access == SND_PCM_ACCESS_RW_INTERLEAVED) {
        // nothing to do
    } else if (isa->access == SND_PCM_ACCESS_RW_NONINTERLEAVED) {
        // nothing to do
    } else {
        snd_pcm_sframes_t commitres = snd_pcm_mmap_commit(isa->handle, isa->offset, isa->read_frame_count);
        if (commitres < 0 || commitres != isa->read_frame_count) {
            int err = (commitres >= 0) ? -EPIPE : commitres;
            if ((err = instream_xrun_recovery(is, err)) < 0)
                return SoundIoErrorStreaming;
        }
    }

    return 0;
}

static int instream_pause_alsa(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is, bool pause) {
    struct SoundIoInStreamAlsa *isa = &is->backend_data.alsa;

    if (isa->is_paused == pause)
        return 0;

    int err;
    if ((err = snd_pcm_pause(isa->handle, pause)) < 0)
        return SoundIoErrorIncompatibleDevice;

    isa->is_paused = pause;
    return 0;
}

static int instream_get_latency_alsa(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is,
        double *out_latency)
{
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoInStreamAlsa *isa = &is->backend_data.alsa;
    int err;

    snd_pcm_sframes_t delay;
    if ((err = snd_pcm_delay(isa->handle, &delay)) < 0) {
        return SoundIoErrorStreaming;
    }

    *out_latency = delay / (double)instream->sample_rate;
    return 0;
}

int soundio_alsa_init(struct SoundIoPrivate *si) {
    struct SoundIoAlsa *sia = &si->backend_data.alsa;
    int err;

    sia->notify_fd = -1;
    sia->notify_wd = -1;
    SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(sia->abort_flag);

    sia->mutex = soundio_os_mutex_create();
    if (!sia->mutex) {
        destroy_alsa(si);
        return SoundIoErrorNoMem;
    }

    sia->cond = soundio_os_cond_create();
    if (!sia->cond) {
        destroy_alsa(si);
        return SoundIoErrorNoMem;
    }


    // set up inotify to watch /dev/snd for devices added or removed
    sia->notify_fd = inotify_init1(IN_NONBLOCK);
    if (sia->notify_fd == -1) {
        err = errno;
        assert(err != EINVAL);
        destroy_alsa(si);
        if (err == EMFILE || err == ENFILE) {
            return SoundIoErrorSystemResources;
        } else {
            assert(err == ENOMEM);
            return SoundIoErrorNoMem;
        }
    }

    sia->notify_wd = inotify_add_watch(sia->notify_fd, "/dev/snd", IN_CREATE | IN_CLOSE_WRITE | IN_DELETE);
    if (sia->notify_wd == -1) {
        err = errno;
        assert(err != EACCES);
        assert(err != EBADF);
        assert(err != EFAULT);
        assert(err != EINVAL);
        assert(err != ENAMETOOLONG);
        destroy_alsa(si);
        if (err == ENOSPC) {
            return SoundIoErrorSystemResources;
        } else if (err == ENOMEM) {
            return SoundIoErrorNoMem;
        } else {
            // Kernel must not have ALSA support.
            return SoundIoErrorInitAudioBackend;
        }
    }

    if (pipe2(sia->notify_pipe_fd, O_NONBLOCK)) {
        assert(errno != EFAULT);
        assert(errno != EINVAL);
        assert(errno == EMFILE || errno == ENFILE);
        return SoundIoErrorSystemResources;
    }

    wakeup_device_poll(sia);

    if ((err = soundio_os_thread_create(device_thread_run, si, NULL, &sia->thread))) {
        destroy_alsa(si);
        return err;
    }

    si->destroy = destroy_alsa;
    si->flush_events = flush_events_alsa;
    si->wait_events = wait_events_alsa;
    si->wakeup = wakeup_alsa;
    si->force_device_scan = force_device_scan_alsa;

    si->outstream_open = outstream_open_alsa;
    si->outstream_destroy = outstream_destroy_alsa;
    si->outstream_start = outstream_start_alsa;
    si->outstream_begin_write = outstream_begin_write_alsa;
    si->outstream_end_write = outstream_end_write_alsa;
    si->outstream_clear_buffer = outstream_clear_buffer_alsa;
    si->outstream_pause = outstream_pause_alsa;
    si->outstream_get_latency = outstream_get_latency_alsa;

    si->instream_open = instream_open_alsa;
    si->instream_destroy = instream_destroy_alsa;
    si->instream_start = instream_start_alsa;
    si->instream_begin_read = instream_begin_read_alsa;
    si->instream_end_read = instream_end_read_alsa;
    si->instream_pause = instream_pause_alsa;
    si->instream_get_latency = instream_get_latency_alsa;

    return 0;
}
#endif
#if SOUNDIO_HAVE_PULSEAUDIO
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

// (amalg) #include "pulseaudio.h"
// (amalg) #include "soundio_private.h"

#include <string.h>
#include <stdio.h>


static void subscribe_callback(pa_context *context,
        pa_subscription_event_type_t event_bits, uint32_t index, void *userdata)
{
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)userdata;
    struct SoundIo *soundio = &si->pub;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    sipa->device_scan_queued = true;
    pa_threaded_mainloop_signal(sipa->main_loop, 0);
    soundio->on_events_signal(soundio);
}

static int subscribe_to_events(struct SoundIoPrivate *si) {
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    pa_subscription_mask_t events = (pa_subscription_mask_t)(
            PA_SUBSCRIPTION_MASK_SINK|PA_SUBSCRIPTION_MASK_SOURCE|PA_SUBSCRIPTION_MASK_SERVER);
    pa_operation *subscribe_op = pa_context_subscribe(sipa->pulse_context, events, NULL, si);
    if (!subscribe_op)
        return SoundIoErrorNoMem;
    pa_operation_unref(subscribe_op);
    return 0;
}

static void context_state_callback(pa_context *context, void *userdata) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)userdata;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    struct SoundIo *soundio = &si->pub;

    switch (pa_context_get_state(context)) {
    case PA_CONTEXT_UNCONNECTED: // The context hasn't been connected yet.
        return;
    case PA_CONTEXT_CONNECTING: // A connection is being established.
        return;
    case PA_CONTEXT_AUTHORIZING: // The client is authorizing itself to the daemon.
        return;
    case PA_CONTEXT_SETTING_NAME: // The client is passing its application name to the daemon.
        return;
    case PA_CONTEXT_READY: // The connection is established, the context is ready to execute operations.
        sipa->ready_flag = true;
        pa_threaded_mainloop_signal(sipa->main_loop, 0);
        return;
    case PA_CONTEXT_TERMINATED: // The connection was terminated cleanly.
        pa_threaded_mainloop_signal(sipa->main_loop, 0);
        return;
    case PA_CONTEXT_FAILED: // The connection failed or was disconnected.
        if (sipa->ready_flag) {
            sipa->connection_err = SoundIoErrorBackendDisconnected;
        } else {
            sipa->connection_err = SoundIoErrorInitAudioBackend;
            sipa->ready_flag = true;
        }
        pa_threaded_mainloop_signal(sipa->main_loop, 0);
        soundio->on_events_signal(soundio);
        return;
    }
}

static void destroy_pa(struct SoundIoPrivate *si) {
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;

    if (sipa->main_loop)
        pa_threaded_mainloop_stop(sipa->main_loop);

    pa_context_disconnect(sipa->pulse_context);
    pa_context_unref(sipa->pulse_context);

    soundio_destroy_devices_info(sipa->current_devices_info);
    soundio_destroy_devices_info(sipa->ready_devices_info);

    if (sipa->main_loop)
        pa_threaded_mainloop_free(sipa->main_loop);

    if (sipa->props)
        pa_proplist_free(sipa->props);

    free(sipa->default_sink_name);
    free(sipa->default_source_name);
}

static enum SoundIoFormat from_pulseaudio_format(pa_sample_spec sample_spec) {
    switch (sample_spec.format) {
    case PA_SAMPLE_U8:          return SoundIoFormatU8;
    case PA_SAMPLE_S16LE:       return SoundIoFormatS16LE;
    case PA_SAMPLE_S16BE:       return SoundIoFormatS16BE;
    case PA_SAMPLE_FLOAT32LE:   return SoundIoFormatFloat32LE;
    case PA_SAMPLE_FLOAT32BE:   return SoundIoFormatFloat32BE;
    case PA_SAMPLE_S32LE:       return SoundIoFormatS32LE;
    case PA_SAMPLE_S32BE:       return SoundIoFormatS32BE;
    case PA_SAMPLE_S24_32LE:    return SoundIoFormatS24LE;
    case PA_SAMPLE_S24_32BE:    return SoundIoFormatS24BE;

    case PA_SAMPLE_MAX:
    case PA_SAMPLE_INVALID:
    case PA_SAMPLE_ALAW:
    case PA_SAMPLE_ULAW:
    case PA_SAMPLE_S24LE:
    case PA_SAMPLE_S24BE:
        return SoundIoFormatInvalid;
    }
    return SoundIoFormatInvalid;
}

static enum SoundIoChannelId from_pulseaudio_channel_pos(pa_channel_position_t pos) {
    switch (pos) {
    case PA_CHANNEL_POSITION_MONO: return SoundIoChannelIdFrontCenter;
    case PA_CHANNEL_POSITION_FRONT_LEFT: return SoundIoChannelIdFrontLeft;
    case PA_CHANNEL_POSITION_FRONT_RIGHT: return SoundIoChannelIdFrontRight;
    case PA_CHANNEL_POSITION_FRONT_CENTER: return SoundIoChannelIdFrontCenter;
    case PA_CHANNEL_POSITION_REAR_CENTER: return SoundIoChannelIdBackCenter;
    case PA_CHANNEL_POSITION_REAR_LEFT: return SoundIoChannelIdBackLeft;
    case PA_CHANNEL_POSITION_REAR_RIGHT: return SoundIoChannelIdBackRight;
    case PA_CHANNEL_POSITION_LFE: return SoundIoChannelIdLfe;
    case PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER: return SoundIoChannelIdFrontLeftCenter;
    case PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER: return SoundIoChannelIdFrontRightCenter;
    case PA_CHANNEL_POSITION_SIDE_LEFT: return SoundIoChannelIdSideLeft;
    case PA_CHANNEL_POSITION_SIDE_RIGHT: return SoundIoChannelIdSideRight;
    case PA_CHANNEL_POSITION_TOP_CENTER: return SoundIoChannelIdTopCenter;
    case PA_CHANNEL_POSITION_TOP_FRONT_LEFT: return SoundIoChannelIdTopFrontLeft;
    case PA_CHANNEL_POSITION_TOP_FRONT_RIGHT: return SoundIoChannelIdTopFrontRight;
    case PA_CHANNEL_POSITION_TOP_FRONT_CENTER: return SoundIoChannelIdTopFrontCenter;
    case PA_CHANNEL_POSITION_TOP_REAR_LEFT: return SoundIoChannelIdTopBackLeft;
    case PA_CHANNEL_POSITION_TOP_REAR_RIGHT: return SoundIoChannelIdTopBackRight;
    case PA_CHANNEL_POSITION_TOP_REAR_CENTER: return SoundIoChannelIdTopBackCenter;

    case PA_CHANNEL_POSITION_AUX0: return SoundIoChannelIdAux0;
    case PA_CHANNEL_POSITION_AUX1: return SoundIoChannelIdAux1;
    case PA_CHANNEL_POSITION_AUX2: return SoundIoChannelIdAux2;
    case PA_CHANNEL_POSITION_AUX3: return SoundIoChannelIdAux3;
    case PA_CHANNEL_POSITION_AUX4: return SoundIoChannelIdAux4;
    case PA_CHANNEL_POSITION_AUX5: return SoundIoChannelIdAux5;
    case PA_CHANNEL_POSITION_AUX6: return SoundIoChannelIdAux6;
    case PA_CHANNEL_POSITION_AUX7: return SoundIoChannelIdAux7;
    case PA_CHANNEL_POSITION_AUX8: return SoundIoChannelIdAux8;
    case PA_CHANNEL_POSITION_AUX9: return SoundIoChannelIdAux9;
    case PA_CHANNEL_POSITION_AUX10: return SoundIoChannelIdAux10;
    case PA_CHANNEL_POSITION_AUX11: return SoundIoChannelIdAux11;
    case PA_CHANNEL_POSITION_AUX12: return SoundIoChannelIdAux12;
    case PA_CHANNEL_POSITION_AUX13: return SoundIoChannelIdAux13;
    case PA_CHANNEL_POSITION_AUX14: return SoundIoChannelIdAux14;
    case PA_CHANNEL_POSITION_AUX15: return SoundIoChannelIdAux15;

    default: return SoundIoChannelIdInvalid;
    }
}

static void set_from_pulseaudio_channel_map(pa_channel_map channel_map, struct SoundIoChannelLayout *channel_layout) {
    channel_layout->channel_count = channel_map.channels;
    for (int i = 0; i < channel_map.channels; i += 1) {
        channel_layout->channels[i] = from_pulseaudio_channel_pos(channel_map.map[i]);
    }
    channel_layout->name = NULL;
    int builtin_layout_count = soundio_channel_layout_builtin_count();
    for (int i = 0; i < builtin_layout_count; i += 1) {
        const struct SoundIoChannelLayout *builtin_layout = soundio_channel_layout_get_builtin(i);
        if (soundio_channel_layout_equal(builtin_layout, channel_layout)) {
            channel_layout->name = builtin_layout->name;
            break;
        }
    }
}

static int pa_set_all_device_channel_layouts(struct SoundIoDevice *device) {
    device->layout_count = soundio_channel_layout_builtin_count();
    device->layouts = ALLOCATE(struct SoundIoChannelLayout, device->layout_count);
    if (!device->layouts)
        return SoundIoErrorNoMem;
    for (int i = 0; i < device->layout_count; i += 1)
        device->layouts[i] = *soundio_channel_layout_get_builtin(i);
    return 0;
}

static int pa_set_all_device_formats(struct SoundIoDevice *device) {
    device->format_count = 9;
    device->formats = ALLOCATE(enum SoundIoFormat, device->format_count);
    if (!device->formats)
        return SoundIoErrorNoMem;
    device->formats[0] = SoundIoFormatU8;
    device->formats[1] = SoundIoFormatS16LE;
    device->formats[2] = SoundIoFormatS16BE;
    device->formats[3] = SoundIoFormatFloat32LE;
    device->formats[4] = SoundIoFormatFloat32BE;
    device->formats[5] = SoundIoFormatS32LE;
    device->formats[6] = SoundIoFormatS32BE;
    device->formats[7] = SoundIoFormatS24LE;
    device->formats[8] = SoundIoFormatS24BE;
    return 0;
}

static int perform_operation(struct SoundIoPrivate *si, pa_operation *op) {
    if (!op)
        return SoundIoErrorNoMem;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    for (;;) {
        switch (pa_operation_get_state(op)) {
        case PA_OPERATION_RUNNING:
            pa_threaded_mainloop_wait(sipa->main_loop);
            continue;
        case PA_OPERATION_DONE:
            pa_operation_unref(op);
            return 0;
        case PA_OPERATION_CANCELLED:
            pa_operation_unref(op);
            return SoundIoErrorInterrupted;
        }
    }
}

static void sink_info_callback(pa_context *pulse_context, const pa_sink_info *info, int eol, void *userdata) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)userdata;
    struct SoundIo *soundio = &si->pub;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    int err;
    if (eol) {
        pa_threaded_mainloop_signal(sipa->main_loop, 0);
        return;
    }
    if (sipa->device_query_err)
        return;

    struct SoundIoDevicePrivate *dev = ALLOCATE(struct SoundIoDevicePrivate, 1);
    if (!dev) {
        sipa->device_query_err = SoundIoErrorNoMem;
        return;
    }
    struct SoundIoDevice *device = &dev->pub;

    device->ref_count = 1;
    device->soundio = soundio;
    device->id = strdup(info->name);
    device->name = strdup(info->description);
    if (!device->id || !device->name) {
        soundio_device_unref(device);
        sipa->device_query_err = SoundIoErrorNoMem;
        return;
    }

    device->sample_rate_current = info->sample_spec.rate;
    // PulseAudio performs resampling, so any value is valid. Let's pick
    // some reasonable min and max values.
    device->sample_rate_count = 1;
    device->sample_rates = &dev->prealloc_sample_rate_range;
    device->sample_rates[0].min = soundio_int_min(SOUNDIO_MIN_SAMPLE_RATE, device->sample_rate_current);
    device->sample_rates[0].max = soundio_int_max(SOUNDIO_MAX_SAMPLE_RATE, device->sample_rate_current);

    device->current_format = from_pulseaudio_format(info->sample_spec);
    // PulseAudio performs sample format conversion, so any PulseAudio
    // value is valid.
    if ((err = pa_set_all_device_formats(device))) {
        soundio_device_unref(device);
        sipa->device_query_err = SoundIoErrorNoMem;
        return;
    }

    set_from_pulseaudio_channel_map(info->channel_map, &device->current_layout);
    // PulseAudio does channel layout remapping, so any channel layout is valid.
    if ((err = pa_set_all_device_channel_layouts(device))) {
        soundio_device_unref(device);
        sipa->device_query_err = SoundIoErrorNoMem;
        return;
    }

    device->aim = SoundIoDeviceAimOutput;

    if (SoundIoListDevicePtr_append(&sipa->current_devices_info->output_devices, device)) {
        soundio_device_unref(device);
        sipa->device_query_err = SoundIoErrorNoMem;
        return;
    }
}

static void source_info_callback(pa_context *pulse_context, const pa_source_info *info, int eol, void *userdata) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)userdata;
    struct SoundIo *soundio = &si->pub;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    int err;

    if (eol) {
        pa_threaded_mainloop_signal(sipa->main_loop, 0);
        return;
    }
    if (sipa->device_query_err)
        return;

    struct SoundIoDevicePrivate *dev = ALLOCATE(struct SoundIoDevicePrivate, 1);
    if (!dev) {
        sipa->device_query_err = SoundIoErrorNoMem;
        return;
    }
    struct SoundIoDevice *device = &dev->pub;

    device->ref_count = 1;
    device->soundio = soundio;
    device->id = strdup(info->name);
    device->name = strdup(info->description);
    if (!device->id || !device->name) {
        soundio_device_unref(device);
        sipa->device_query_err = SoundIoErrorNoMem;
        return;
    }

    device->sample_rate_current = info->sample_spec.rate;
    // PulseAudio performs resampling, so any value is valid. Let's pick
    // some reasonable min and max values.
    device->sample_rate_count = 1;
    device->sample_rates = &dev->prealloc_sample_rate_range;
    device->sample_rates[0].min = soundio_int_min(SOUNDIO_MIN_SAMPLE_RATE, device->sample_rate_current);
    device->sample_rates[0].max = soundio_int_max(SOUNDIO_MAX_SAMPLE_RATE, device->sample_rate_current);

    device->current_format = from_pulseaudio_format(info->sample_spec);
    // PulseAudio performs sample format conversion, so any PulseAudio
    // value is valid.
    if ((err = pa_set_all_device_formats(device))) {
        soundio_device_unref(device);
        sipa->device_query_err = SoundIoErrorNoMem;
        return;
    }

    set_from_pulseaudio_channel_map(info->channel_map, &device->current_layout);
    // PulseAudio does channel layout remapping, so any channel layout is valid.
    if ((err = pa_set_all_device_channel_layouts(device))) {
        soundio_device_unref(device);
        sipa->device_query_err = SoundIoErrorNoMem;
        return;
    }

    device->aim = SoundIoDeviceAimInput;

    if (SoundIoListDevicePtr_append(&sipa->current_devices_info->input_devices, device)) {
        soundio_device_unref(device);
        sipa->device_query_err = SoundIoErrorNoMem;
        return;
    }
}

static void server_info_callback(pa_context *pulse_context, const pa_server_info *info, void *userdata) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)userdata;
    assert(si);
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;

    assert(!sipa->default_sink_name);
    assert(!sipa->default_source_name);

    sipa->default_sink_name = strdup(info->default_sink_name);
    sipa->default_source_name = strdup(info->default_source_name);

    if (!sipa->default_sink_name || !sipa->default_source_name)
        sipa->device_query_err = SoundIoErrorNoMem;

    pa_threaded_mainloop_signal(sipa->main_loop, 0);
}

// always called even when refresh_devices succeeds
static void cleanup_pa_refresh_devices(struct SoundIoPrivate *si) {
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;

    soundio_destroy_devices_info(sipa->current_devices_info);
    sipa->current_devices_info = NULL;

    free(sipa->default_sink_name);
    sipa->default_sink_name = NULL;

    free(sipa->default_source_name);
    sipa->default_source_name = NULL;
}

// call this while holding the main loop lock
static int pa_refresh_devices(struct SoundIoPrivate *si) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;

    assert(!sipa->current_devices_info);
    sipa->current_devices_info = ALLOCATE(struct SoundIoDevicesInfo, 1);
    if (!sipa->current_devices_info)
        return SoundIoErrorNoMem;

    pa_operation *list_sink_op = pa_context_get_sink_info_list(sipa->pulse_context, sink_info_callback, si);
    pa_operation *list_source_op = pa_context_get_source_info_list(sipa->pulse_context, source_info_callback, si);
    pa_operation *server_info_op = pa_context_get_server_info(sipa->pulse_context, server_info_callback, si);

    int err;
    if ((err = perform_operation(si, list_sink_op))) {
        return err;
    }
    if ((err = perform_operation(si, list_source_op))) {
        return err;
    }
    if ((err = perform_operation(si, server_info_op))) {
        return err;
    }

    if (sipa->device_query_err) {
        return sipa->device_query_err;
    }

    // based on the default sink name, figure out the default output index
    // if the name doesn't match just pick the first one. if there are no
    // devices then we need to set it to -1.
    sipa->current_devices_info->default_output_index = -1;
    sipa->current_devices_info->default_input_index = -1;

    if (sipa->current_devices_info->input_devices.length > 0) {
        sipa->current_devices_info->default_input_index = 0;
        for (int i = 0; i < sipa->current_devices_info->input_devices.length; i += 1) {
            struct SoundIoDevice *device = SoundIoListDevicePtr_val_at(
                    &sipa->current_devices_info->input_devices, i);

            assert(device->aim == SoundIoDeviceAimInput);
            if (strcmp(device->id, sipa->default_source_name) == 0) {
                sipa->current_devices_info->default_input_index = i;
            }
        }
    }

    if (sipa->current_devices_info->output_devices.length > 0) {
        sipa->current_devices_info->default_output_index = 0;
        for (int i = 0; i < sipa->current_devices_info->output_devices.length; i += 1) {
            struct SoundIoDevice *device = SoundIoListDevicePtr_val_at(
                    &sipa->current_devices_info->output_devices, i);

            assert(device->aim == SoundIoDeviceAimOutput);
            if (strcmp(device->id, sipa->default_sink_name) == 0) {
                sipa->current_devices_info->default_output_index = i;
            }
        }
    }

    soundio_destroy_devices_info(sipa->ready_devices_info);
    sipa->ready_devices_info = sipa->current_devices_info;
    sipa->current_devices_info = NULL;
    pa_threaded_mainloop_signal(sipa->main_loop, 0);
    soundio->on_events_signal(soundio);

    return 0;
}

static void pa_my_flush_events(struct SoundIoPrivate *si, bool wait) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;

    bool change = false;
    bool cb_shutdown = false;
    struct SoundIoDevicesInfo *old_devices_info = NULL;

    pa_threaded_mainloop_lock(sipa->main_loop);

    if (wait)
        pa_threaded_mainloop_wait(sipa->main_loop);

    if (sipa->device_scan_queued && !sipa->connection_err) {
        sipa->device_scan_queued = false;
        sipa->connection_err = pa_refresh_devices(si);
        cleanup_pa_refresh_devices(si);
    }

    if (sipa->connection_err && !sipa->emitted_shutdown_cb) {
        sipa->emitted_shutdown_cb = true;
        cb_shutdown = true;
    } else if (sipa->ready_devices_info) {
        old_devices_info = si->safe_devices_info;
        si->safe_devices_info = sipa->ready_devices_info;
        sipa->ready_devices_info = NULL;
        change = true;
    }

    pa_threaded_mainloop_unlock(sipa->main_loop);

    if (cb_shutdown)
        soundio->on_backend_disconnect(soundio, sipa->connection_err);
    else if (change)
        soundio->on_devices_change(soundio);

    soundio_destroy_devices_info(old_devices_info);
}

static void flush_events_pa(struct SoundIoPrivate *si) {
    pa_my_flush_events(si, false);
}

static void wait_events_pa(struct SoundIoPrivate *si) {
    pa_my_flush_events(si, false);
    pa_my_flush_events(si, true);
}

static void wakeup_pa(struct SoundIoPrivate *si) {
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    pa_threaded_mainloop_lock(sipa->main_loop);
    pa_threaded_mainloop_signal(sipa->main_loop, 0);
    pa_threaded_mainloop_unlock(sipa->main_loop);
}

static void force_device_scan_pa(struct SoundIoPrivate *si) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    pa_threaded_mainloop_lock(sipa->main_loop);
    sipa->device_scan_queued = true;
    pa_threaded_mainloop_signal(sipa->main_loop, 0);
    soundio->on_events_signal(soundio);
    pa_threaded_mainloop_unlock(sipa->main_loop);
}

static pa_sample_format_t to_pulseaudio_format(enum SoundIoFormat format) {
    switch (format) {
    case SoundIoFormatU8:         return PA_SAMPLE_U8;
    case SoundIoFormatS16LE:      return PA_SAMPLE_S16LE;
    case SoundIoFormatS16BE:      return PA_SAMPLE_S16BE;
    case SoundIoFormatS24LE:      return PA_SAMPLE_S24_32LE;
    case SoundIoFormatS24BE:      return PA_SAMPLE_S24_32BE;
    case SoundIoFormatS32LE:      return PA_SAMPLE_S32LE;
    case SoundIoFormatS32BE:      return PA_SAMPLE_S32BE;
    case SoundIoFormatFloat32LE:  return PA_SAMPLE_FLOAT32LE;
    case SoundIoFormatFloat32BE:  return PA_SAMPLE_FLOAT32BE;

    case SoundIoFormatInvalid:
    case SoundIoFormatS8:
    case SoundIoFormatU16LE:
    case SoundIoFormatU16BE:
    case SoundIoFormatU24LE:
    case SoundIoFormatU24BE:
    case SoundIoFormatU32LE:
    case SoundIoFormatU32BE:
    case SoundIoFormatFloat64LE:
    case SoundIoFormatFloat64BE:
        return PA_SAMPLE_INVALID;
    }
    return PA_SAMPLE_INVALID;
}

static pa_channel_position_t to_pulseaudio_channel_pos(enum SoundIoChannelId channel_id) {
    switch (channel_id) {
    case SoundIoChannelIdFrontLeft: return PA_CHANNEL_POSITION_FRONT_LEFT;
    case SoundIoChannelIdFrontRight: return PA_CHANNEL_POSITION_FRONT_RIGHT;
    case SoundIoChannelIdFrontCenter: return PA_CHANNEL_POSITION_FRONT_CENTER;
    case SoundIoChannelIdLfe: return PA_CHANNEL_POSITION_LFE;
    case SoundIoChannelIdBackLeft: return PA_CHANNEL_POSITION_REAR_LEFT;
    case SoundIoChannelIdBackRight: return PA_CHANNEL_POSITION_REAR_RIGHT;
    case SoundIoChannelIdFrontLeftCenter: return PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER;
    case SoundIoChannelIdFrontRightCenter: return PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER;
    case SoundIoChannelIdBackCenter: return PA_CHANNEL_POSITION_REAR_CENTER;
    case SoundIoChannelIdSideLeft: return PA_CHANNEL_POSITION_SIDE_LEFT;
    case SoundIoChannelIdSideRight: return PA_CHANNEL_POSITION_SIDE_RIGHT;
    case SoundIoChannelIdTopCenter: return PA_CHANNEL_POSITION_TOP_CENTER;
    case SoundIoChannelIdTopFrontLeft: return PA_CHANNEL_POSITION_TOP_FRONT_LEFT;
    case SoundIoChannelIdTopFrontCenter: return PA_CHANNEL_POSITION_TOP_FRONT_CENTER;
    case SoundIoChannelIdTopFrontRight: return PA_CHANNEL_POSITION_TOP_FRONT_RIGHT;
    case SoundIoChannelIdTopBackLeft: return PA_CHANNEL_POSITION_TOP_REAR_LEFT;
    case SoundIoChannelIdTopBackCenter: return PA_CHANNEL_POSITION_TOP_REAR_CENTER;
    case SoundIoChannelIdTopBackRight: return PA_CHANNEL_POSITION_TOP_REAR_RIGHT;

    case SoundIoChannelIdAux0: return PA_CHANNEL_POSITION_AUX0;
    case SoundIoChannelIdAux1: return PA_CHANNEL_POSITION_AUX1;
    case SoundIoChannelIdAux2: return PA_CHANNEL_POSITION_AUX2;
    case SoundIoChannelIdAux3: return PA_CHANNEL_POSITION_AUX3;
    case SoundIoChannelIdAux4: return PA_CHANNEL_POSITION_AUX4;
    case SoundIoChannelIdAux5: return PA_CHANNEL_POSITION_AUX5;
    case SoundIoChannelIdAux6: return PA_CHANNEL_POSITION_AUX6;
    case SoundIoChannelIdAux7: return PA_CHANNEL_POSITION_AUX7;
    case SoundIoChannelIdAux8: return PA_CHANNEL_POSITION_AUX8;
    case SoundIoChannelIdAux9: return PA_CHANNEL_POSITION_AUX9;
    case SoundIoChannelIdAux10: return PA_CHANNEL_POSITION_AUX10;
    case SoundIoChannelIdAux11: return PA_CHANNEL_POSITION_AUX11;
    case SoundIoChannelIdAux12: return PA_CHANNEL_POSITION_AUX12;
    case SoundIoChannelIdAux13: return PA_CHANNEL_POSITION_AUX13;
    case SoundIoChannelIdAux14: return PA_CHANNEL_POSITION_AUX14;
    case SoundIoChannelIdAux15: return PA_CHANNEL_POSITION_AUX15;

    default:
        return PA_CHANNEL_POSITION_INVALID;
    }
}

static pa_channel_map to_pulseaudio_channel_map(const struct SoundIoChannelLayout *channel_layout) {
    pa_channel_map channel_map;
    channel_map.channels = channel_layout->channel_count;

    assert((unsigned)channel_layout->channel_count <= PA_CHANNELS_MAX);

    for (int i = 0; i < channel_layout->channel_count; i += 1)
        channel_map.map[i] = to_pulseaudio_channel_pos(channel_layout->channels[i]);

    return channel_map;
}

static void playback_stream_state_callback(pa_stream *stream, void *userdata) {
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate*) userdata;
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIo *soundio = outstream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    struct SoundIoOutStreamPulseAudio *ospa = &os->backend_data.pulseaudio;
    switch (pa_stream_get_state(stream)) {
        case PA_STREAM_UNCONNECTED:
        case PA_STREAM_CREATING:
        case PA_STREAM_TERMINATED:
            break;
        case PA_STREAM_READY:
            SOUNDIO_ATOMIC_STORE(ospa->stream_ready, true);
            pa_threaded_mainloop_signal(sipa->main_loop, 0);
            break;
        case PA_STREAM_FAILED:
            outstream->error_callback(outstream, SoundIoErrorStreaming);
            break;
    }
}

static void playback_stream_underflow_callback(pa_stream *stream, void *userdata) {
    struct SoundIoOutStream *outstream = (struct SoundIoOutStream*)userdata;
    outstream->underflow_callback(outstream);
}

static void playback_stream_write_callback(pa_stream *stream, size_t nbytes, void *userdata) {
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate*)(userdata);
    struct SoundIoOutStream *outstream = &os->pub;
    int frame_count = nbytes / outstream->bytes_per_frame;
    outstream->write_callback(outstream, 0, frame_count);
}

static void outstream_destroy_pa(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamPulseAudio *ospa = &os->backend_data.pulseaudio;

    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    pa_stream *stream = ospa->stream;
    if (stream) {
        pa_threaded_mainloop_lock(sipa->main_loop);

        pa_stream_set_write_callback(stream, NULL, NULL);
        pa_stream_set_state_callback(stream, NULL, NULL);
        pa_stream_set_underflow_callback(stream, NULL, NULL);
        pa_stream_set_overflow_callback(stream, NULL, NULL);
        pa_stream_disconnect(stream);

        pa_stream_unref(stream);

        pa_threaded_mainloop_unlock(sipa->main_loop);

        ospa->stream = NULL;
    }
}

static void timing_update_callback(pa_stream *stream, int success, void *userdata) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)userdata;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    pa_threaded_mainloop_signal(sipa->main_loop, 0);
}

static int outstream_open_pa(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamPulseAudio *ospa = &os->backend_data.pulseaudio;
    struct SoundIoOutStream *outstream = &os->pub;

    if ((unsigned)outstream->layout.channel_count > PA_CHANNELS_MAX)
        return SoundIoErrorIncompatibleBackend;

    if (!outstream->name)
        outstream->name = "SoundIoOutStream";

    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    SOUNDIO_ATOMIC_STORE(ospa->stream_ready, false);
    SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(ospa->clear_buffer_flag);

    assert(sipa->pulse_context);

    pa_threaded_mainloop_lock(sipa->main_loop);

    pa_sample_spec sample_spec;
    sample_spec.format = to_pulseaudio_format(outstream->format);
    sample_spec.rate = outstream->sample_rate;

    sample_spec.channels = outstream->layout.channel_count;
    pa_channel_map channel_map = to_pulseaudio_channel_map(&outstream->layout);

    ospa->stream = pa_stream_new(sipa->pulse_context, outstream->name, &sample_spec, &channel_map);
    if (!ospa->stream) {
        pa_threaded_mainloop_unlock(sipa->main_loop);
        outstream_destroy_pa(si, os);
        return SoundIoErrorNoMem;
    }
    pa_stream_set_state_callback(ospa->stream, playback_stream_state_callback, os);

    ospa->buffer_attr.maxlength = UINT32_MAX;
    ospa->buffer_attr.tlength = UINT32_MAX;
    ospa->buffer_attr.prebuf = 0;
    ospa->buffer_attr.minreq = UINT32_MAX;
    ospa->buffer_attr.fragsize = UINT32_MAX;

    int bytes_per_second = outstream->bytes_per_frame * outstream->sample_rate;
    if (outstream->software_latency > 0.0) {
        int buffer_length = outstream->bytes_per_frame *
            ceil_dbl_to_int(outstream->software_latency * bytes_per_second / (double)outstream->bytes_per_frame);

        ospa->buffer_attr.maxlength = buffer_length;
        ospa->buffer_attr.tlength = buffer_length;
    }

    pa_stream_flags_t flags = (pa_stream_flags_t)(PA_STREAM_START_CORKED | PA_STREAM_AUTO_TIMING_UPDATE |
            PA_STREAM_INTERPOLATE_TIMING);

    int err = pa_stream_connect_playback(ospa->stream,
            outstream->device->id, &ospa->buffer_attr,
            flags, NULL, NULL);
    if (err) {
        pa_threaded_mainloop_unlock(sipa->main_loop);
        return SoundIoErrorOpeningDevice;
    }

    while (!SOUNDIO_ATOMIC_LOAD(ospa->stream_ready))
        pa_threaded_mainloop_wait(sipa->main_loop);

    pa_operation *update_timing_info_op = pa_stream_update_timing_info(ospa->stream, timing_update_callback, si);
    if ((err = perform_operation(si, update_timing_info_op))) {
        pa_threaded_mainloop_unlock(sipa->main_loop);
        return err;
    }

    size_t writable_size = pa_stream_writable_size(ospa->stream);
    outstream->software_latency = ((double)writable_size) / (double)bytes_per_second;

    pa_threaded_mainloop_unlock(sipa->main_loop);

    return 0;
}

static int outstream_start_pa(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    struct SoundIoOutStreamPulseAudio *ospa = &os->backend_data.pulseaudio;

    pa_threaded_mainloop_lock(sipa->main_loop);

    ospa->write_byte_count = pa_stream_writable_size(ospa->stream);
    int frame_count = ospa->write_byte_count / outstream->bytes_per_frame;
    outstream->write_callback(outstream, 0, frame_count);

    pa_operation *op = pa_stream_cork(ospa->stream, false, NULL, NULL);
    if (!op) {
        pa_threaded_mainloop_unlock(sipa->main_loop);
        return SoundIoErrorStreaming;
    }
    pa_operation_unref(op);
    pa_stream_set_write_callback(ospa->stream, playback_stream_write_callback, os);
    pa_stream_set_underflow_callback(ospa->stream, playback_stream_underflow_callback, outstream);
    pa_stream_set_overflow_callback(ospa->stream, playback_stream_underflow_callback, outstream);

    pa_threaded_mainloop_unlock(sipa->main_loop);

    return 0;
}

static int outstream_begin_write_pa(struct SoundIoPrivate *si,
        struct SoundIoOutStreamPrivate *os, struct SoundIoChannelArea **out_areas, int *frame_count)
{
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoOutStreamPulseAudio *ospa = &os->backend_data.pulseaudio;
    pa_stream *stream = ospa->stream;

    ospa->write_byte_count = *frame_count * outstream->bytes_per_frame;
    if (pa_stream_begin_write(stream, (void**)&ospa->write_ptr, &ospa->write_byte_count))
        return SoundIoErrorStreaming;

    for (int ch = 0; ch < outstream->layout.channel_count; ch += 1) {
        ospa->areas[ch].ptr = ospa->write_ptr + outstream->bytes_per_sample * ch;
        ospa->areas[ch].step = outstream->bytes_per_frame;
    }

    *frame_count = ospa->write_byte_count / outstream->bytes_per_frame;
    *out_areas = ospa->areas;

    return 0;
}

static int outstream_end_write_pa(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamPulseAudio *ospa = &os->backend_data.pulseaudio;
    pa_stream *stream = ospa->stream;

    pa_seek_mode_t seek_mode = SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(ospa->clear_buffer_flag) ? PA_SEEK_RELATIVE : PA_SEEK_RELATIVE_ON_READ;
    if (pa_stream_write(stream, ospa->write_ptr, ospa->write_byte_count, NULL, 0, seek_mode))
        return SoundIoErrorStreaming;

    return 0;
}

static int outstream_clear_buffer_pa(struct SoundIoPrivate *si,
        struct SoundIoOutStreamPrivate *os)
{
    struct SoundIoOutStreamPulseAudio *ospa = &os->backend_data.pulseaudio;
    SOUNDIO_ATOMIC_FLAG_CLEAR(ospa->clear_buffer_flag);
    return 0;
}

static int outstream_pause_pa(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os, bool pause) {
    struct SoundIoOutStreamPulseAudio *ospa = &os->backend_data.pulseaudio;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;

    if (!pa_threaded_mainloop_in_thread(sipa->main_loop)) {
        pa_threaded_mainloop_lock(sipa->main_loop);
    }

    if (pause != pa_stream_is_corked(ospa->stream)) {
        pa_operation *op = pa_stream_cork(ospa->stream, pause, NULL, NULL);
        if (!op) {
            pa_threaded_mainloop_unlock(sipa->main_loop);
            return SoundIoErrorStreaming;
        }
        pa_operation_unref(op);
    }

    if (!pa_threaded_mainloop_in_thread(sipa->main_loop)) {
        pa_threaded_mainloop_unlock(sipa->main_loop);
    }

    return 0;
}

static int outstream_get_latency_pa(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os, double *out_latency) {
    struct SoundIoOutStreamPulseAudio *ospa = &os->backend_data.pulseaudio;

    int err;
    pa_usec_t r_usec;
    int negative;
    if ((err = pa_stream_get_latency(ospa->stream, &r_usec, &negative))) {
        return SoundIoErrorStreaming;
    }
    *out_latency = r_usec / 1000000.0;
    return 0;
}

static void recording_stream_state_callback(pa_stream *stream, void *userdata) {
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate*)userdata;
    struct SoundIoInStreamPulseAudio *ispa = &is->backend_data.pulseaudio;
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIo *soundio = instream->device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    switch (pa_stream_get_state(stream)) {
        case PA_STREAM_UNCONNECTED:
        case PA_STREAM_CREATING:
        case PA_STREAM_TERMINATED:
            break;
        case PA_STREAM_READY:
            SOUNDIO_ATOMIC_STORE(ispa->stream_ready, true);
            pa_threaded_mainloop_signal(sipa->main_loop, 0);
            break;
        case PA_STREAM_FAILED:
            instream->error_callback(instream, SoundIoErrorStreaming);
            break;
    }
}

static void recording_stream_read_callback(pa_stream *stream, size_t nbytes, void *userdata) {
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate*)userdata;
    struct SoundIoInStream *instream = &is->pub;
    assert(nbytes % instream->bytes_per_frame == 0);
    assert(nbytes > 0);
    int available_frame_count = nbytes / instream->bytes_per_frame;
    instream->read_callback(instream, 0, available_frame_count);
}

static void instream_destroy_pa(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamPulseAudio *ispa = &is->backend_data.pulseaudio;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    pa_stream *stream = ispa->stream;
    if (stream) {
        pa_threaded_mainloop_lock(sipa->main_loop);

        pa_stream_set_state_callback(stream, NULL, NULL);
        pa_stream_set_read_callback(stream, NULL, NULL);
        pa_stream_disconnect(stream);
        pa_stream_unref(stream);

        pa_threaded_mainloop_unlock(sipa->main_loop);

        ispa->stream = NULL;
    }
}

static int instream_open_pa(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamPulseAudio *ispa = &is->backend_data.pulseaudio;
    struct SoundIoInStream *instream = &is->pub;

    if ((unsigned)instream->layout.channel_count > PA_CHANNELS_MAX)
        return SoundIoErrorIncompatibleBackend;
    if (!instream->name)
        instream->name = "SoundIoInStream";

    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    SOUNDIO_ATOMIC_STORE(ispa->stream_ready, false);

    pa_threaded_mainloop_lock(sipa->main_loop);

    pa_sample_spec sample_spec;
    sample_spec.format = to_pulseaudio_format(instream->format);
    sample_spec.rate = instream->sample_rate;
    sample_spec.channels = instream->layout.channel_count;

    pa_channel_map channel_map = to_pulseaudio_channel_map(&instream->layout);

    ispa->stream = pa_stream_new(sipa->pulse_context, instream->name, &sample_spec, &channel_map);
    if (!ispa->stream) {
        pa_threaded_mainloop_unlock(sipa->main_loop);
        instream_destroy_pa(si, is);
        return SoundIoErrorNoMem;
    }

    pa_stream *stream = ispa->stream;

    pa_stream_set_state_callback(stream, recording_stream_state_callback, is);
    pa_stream_set_read_callback(stream, recording_stream_read_callback, is);

    ispa->buffer_attr.maxlength = UINT32_MAX;
    ispa->buffer_attr.tlength = UINT32_MAX;
    ispa->buffer_attr.prebuf = 0;
    ispa->buffer_attr.minreq = UINT32_MAX;
    ispa->buffer_attr.fragsize = UINT32_MAX;

    if (instream->software_latency > 0.0) {
        int bytes_per_second = instream->bytes_per_frame * instream->sample_rate;
        int buffer_length = instream->bytes_per_frame *
            ceil_dbl_to_int(instream->software_latency * bytes_per_second / (double)instream->bytes_per_frame);
        ispa->buffer_attr.fragsize = buffer_length;
    }

    pa_threaded_mainloop_unlock(sipa->main_loop);

    return 0;
}

static int instream_start_pa(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoInStreamPulseAudio *ispa = &is->backend_data.pulseaudio;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;
    pa_threaded_mainloop_lock(sipa->main_loop);

    pa_stream_flags_t flags = (pa_stream_flags_t)(PA_STREAM_AUTO_TIMING_UPDATE | PA_STREAM_INTERPOLATE_TIMING);

    int err = pa_stream_connect_record(ispa->stream,
            instream->device->id,
            &ispa->buffer_attr, flags);
    if (err) {
        pa_threaded_mainloop_unlock(sipa->main_loop);
        return SoundIoErrorOpeningDevice;
    }

    while (!SOUNDIO_ATOMIC_LOAD(ispa->stream_ready))
        pa_threaded_mainloop_wait(sipa->main_loop);

    pa_operation *update_timing_info_op = pa_stream_update_timing_info(ispa->stream, timing_update_callback, si);
    if ((err = perform_operation(si, update_timing_info_op))) {
        pa_threaded_mainloop_unlock(sipa->main_loop);
        return err;
    }


    pa_threaded_mainloop_unlock(sipa->main_loop);
    return 0;
}

static int instream_begin_read_pa(struct SoundIoPrivate *si,
        struct SoundIoInStreamPrivate *is, struct SoundIoChannelArea **out_areas, int *frame_count)
{
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoInStreamPulseAudio *ispa = &is->backend_data.pulseaudio;
    pa_stream *stream = ispa->stream;

    assert(SOUNDIO_ATOMIC_LOAD(ispa->stream_ready));

    if (!ispa->peek_buf) {
        if (pa_stream_peek(stream, (const void **)&ispa->peek_buf, &ispa->peek_buf_size))
            return SoundIoErrorStreaming;

        ispa->peek_buf_frames_left = ispa->peek_buf_size / instream->bytes_per_frame;
        ispa->peek_buf_index = 0;

        // hole
        if (!ispa->peek_buf) {
            *frame_count = ispa->peek_buf_frames_left;
            *out_areas = NULL;
            return 0;
        }
    }

    ispa->read_frame_count = soundio_int_min(*frame_count, ispa->peek_buf_frames_left);
    *frame_count = ispa->read_frame_count;
    for (int ch = 0; ch < instream->layout.channel_count; ch += 1) {
        ispa->areas[ch].ptr = ispa->peek_buf + ispa->peek_buf_index + instream->bytes_per_sample * ch;
        ispa->areas[ch].step = instream->bytes_per_frame;
    }

    *out_areas = ispa->areas;

    return 0;
}

static int instream_end_read_pa(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoInStreamPulseAudio *ispa = &is->backend_data.pulseaudio;
    pa_stream *stream = ispa->stream;

    // hole
    if (!ispa->peek_buf) {
        if (pa_stream_drop(stream))
            return SoundIoErrorStreaming;
        return 0;
    }

    size_t advance_bytes = ispa->read_frame_count * instream->bytes_per_frame;
    ispa->peek_buf_index += advance_bytes;
    ispa->peek_buf_frames_left -= ispa->read_frame_count;

    if (ispa->peek_buf_index >= ispa->peek_buf_size) {
        if (pa_stream_drop(stream))
            return SoundIoErrorStreaming;
        ispa->peek_buf = NULL;
    }

    return 0;
}

static int instream_pause_pa(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is, bool pause) {
    struct SoundIoInStreamPulseAudio *ispa = &is->backend_data.pulseaudio;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;

    if (!pa_threaded_mainloop_in_thread(sipa->main_loop)) {
        pa_threaded_mainloop_lock(sipa->main_loop);
    }

    if (pause != pa_stream_is_corked(ispa->stream)) {
        pa_operation *op = pa_stream_cork(ispa->stream, pause, NULL, NULL);
        if (!op)
            return SoundIoErrorStreaming;
        pa_operation_unref(op);
    }

    if (!pa_threaded_mainloop_in_thread(sipa->main_loop)) {
        pa_threaded_mainloop_unlock(sipa->main_loop);
    }

    return 0;
}

static int instream_get_latency_pa(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is, double *out_latency) {
    struct SoundIoInStreamPulseAudio *ispa = &is->backend_data.pulseaudio;

    int err;
    pa_usec_t r_usec;
    int negative;
    if ((err = pa_stream_get_latency(ispa->stream, &r_usec, &negative))) {
        return SoundIoErrorStreaming;
    }
    *out_latency = r_usec / 1000000.0;
    return 0;
}

int soundio_pulseaudio_init(struct SoundIoPrivate *si) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoPulseAudio *sipa = &si->backend_data.pulseaudio;

    sipa->device_scan_queued = true;

    sipa->main_loop = pa_threaded_mainloop_new();
    if (!sipa->main_loop) {
        destroy_pa(si);
        return SoundIoErrorNoMem;
    }

    pa_mainloop_api *main_loop_api = pa_threaded_mainloop_get_api(sipa->main_loop);

    sipa->props = pa_proplist_new();
    if (!sipa->props) {
        destroy_pa(si);
        return SoundIoErrorNoMem;
    }

    sipa->pulse_context = pa_context_new_with_proplist(main_loop_api, soundio->app_name, sipa->props);
    if (!sipa->pulse_context) {
        destroy_pa(si);
        return SoundIoErrorNoMem;
    }

    pa_context_set_subscribe_callback(sipa->pulse_context, subscribe_callback, si);
    pa_context_set_state_callback(sipa->pulse_context, context_state_callback, si);

    int err = pa_context_connect(sipa->pulse_context, NULL, (pa_context_flags_t)0, NULL);
    if (err) {
        destroy_pa(si);
        return SoundIoErrorInitAudioBackend;
    }

    if (pa_threaded_mainloop_start(sipa->main_loop)) {
        destroy_pa(si);
        return SoundIoErrorNoMem;
    }

    pa_threaded_mainloop_lock(sipa->main_loop);

    // block until ready
    while (!sipa->ready_flag)
        pa_threaded_mainloop_wait(sipa->main_loop);

    if (sipa->connection_err) {
        pa_threaded_mainloop_unlock(sipa->main_loop);
        destroy_pa(si);
        return sipa->connection_err;
    }

    if ((err = subscribe_to_events(si))) {
        pa_threaded_mainloop_unlock(sipa->main_loop);
        destroy_pa(si);
        return err;
    }

    pa_threaded_mainloop_unlock(sipa->main_loop);

    si->destroy = destroy_pa;
    si->flush_events = flush_events_pa;
    si->wait_events = wait_events_pa;
    si->wakeup = wakeup_pa;
    si->force_device_scan = force_device_scan_pa;

    si->outstream_open = outstream_open_pa;
    si->outstream_destroy = outstream_destroy_pa;
    si->outstream_start = outstream_start_pa;
    si->outstream_begin_write = outstream_begin_write_pa;
    si->outstream_end_write = outstream_end_write_pa;
    si->outstream_clear_buffer = outstream_clear_buffer_pa;
    si->outstream_pause = outstream_pause_pa;
    si->outstream_get_latency = outstream_get_latency_pa;

    si->instream_open = instream_open_pa;
    si->instream_destroy = instream_destroy_pa;
    si->instream_start = instream_start_pa;
    si->instream_begin_read = instream_begin_read_pa;
    si->instream_end_read = instream_end_read_pa;
    si->instream_pause = instream_pause_pa;
    si->instream_get_latency = instream_get_latency_pa;

    return 0;
}
#endif
#if SOUNDIO_HAVE_COREAUDIO
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

// (amalg) #include "coreaudio.h"
// (amalg) #include "soundio_private.h"

#include <assert.h>

static const int OUTPUT_ELEMENT = 0;
static const int INPUT_ELEMENT = 1;

static AudioObjectPropertyAddress device_listen_props[] = {
    {
        kAudioDevicePropertyDeviceHasChanged,
        kAudioObjectPropertyScopeGlobal,
        kAudioObjectPropertyElementMaster
    },
    {
        kAudioObjectPropertyName,
        kAudioObjectPropertyScopeGlobal,
        kAudioObjectPropertyElementMaster
    },
    {
        kAudioDevicePropertyDeviceUID,
        kAudioObjectPropertyScopeGlobal,
        kAudioObjectPropertyElementMaster
    },
    {
        kAudioDevicePropertyStreamConfiguration,
        kAudioObjectPropertyScopeInput,
        kAudioObjectPropertyElementMaster
    },
    {
        kAudioDevicePropertyStreamConfiguration,
        kAudioObjectPropertyScopeOutput,
        kAudioObjectPropertyElementMaster
    },
    {
        kAudioDevicePropertyPreferredChannelLayout,
        kAudioObjectPropertyScopeInput,
        kAudioObjectPropertyElementMaster
    },
    {
        kAudioDevicePropertyPreferredChannelLayout,
        kAudioObjectPropertyScopeOutput,
        kAudioObjectPropertyElementMaster
    },
    {
        kAudioDevicePropertyNominalSampleRate,
        kAudioObjectPropertyScopeInput,
        kAudioObjectPropertyElementMaster
    },
    {
        kAudioDevicePropertyNominalSampleRate,
        kAudioObjectPropertyScopeOutput,
        kAudioObjectPropertyElementMaster
    },
    {
        kAudioDevicePropertyAvailableNominalSampleRates,
        kAudioObjectPropertyScopeInput,
        kAudioObjectPropertyElementMaster
    },
    {
        kAudioDevicePropertyAvailableNominalSampleRates,
        kAudioObjectPropertyScopeOutput,
        kAudioObjectPropertyElementMaster
    },
    {
        kAudioDevicePropertyBufferFrameSize,
        kAudioObjectPropertyScopeInput,
        kAudioObjectPropertyElementMaster
    },
    {
        kAudioDevicePropertyBufferFrameSize,
        kAudioObjectPropertyScopeOutput,
        kAudioObjectPropertyElementMaster
    },
    {
        kAudioDevicePropertyBufferFrameSizeRange,
        kAudioObjectPropertyScopeInput,
        kAudioObjectPropertyElementMaster
    },
    {
        kAudioDevicePropertyBufferFrameSizeRange,
        kAudioObjectPropertyScopeOutput,
        kAudioObjectPropertyElementMaster
    },
};

static enum SoundIoDeviceAim aims[] = {
    SoundIoDeviceAimInput,
    SoundIoDeviceAimOutput,
};

SOUNDIO_MAKE_LIST_DEF(AudioDeviceID, SoundIoListAudioDeviceID, SOUNDIO_LIST_STATIC)

static OSStatus on_devices_changed(AudioObjectID in_object_id, UInt32 in_number_addresses,
    const AudioObjectPropertyAddress in_addresses[], void *in_client_data)
{
    struct SoundIoPrivate *si = (struct SoundIoPrivate*)in_client_data;
    struct SoundIoCoreAudio *sica = &si->backend_data.coreaudio;

    SOUNDIO_ATOMIC_STORE(sica->device_scan_queued, true);
    soundio_os_cond_signal(sica->scan_devices_cond, NULL);

    return noErr;
}

static OSStatus on_service_restarted(AudioObjectID in_object_id, UInt32 in_number_addresses,
    const AudioObjectPropertyAddress in_addresses[], void *in_client_data)
{
    struct SoundIoPrivate *si = (struct SoundIoPrivate*)in_client_data;
    struct SoundIoCoreAudio *sica = &si->backend_data.coreaudio;

    SOUNDIO_ATOMIC_STORE(sica->service_restarted, true);
    soundio_os_cond_signal(sica->scan_devices_cond, NULL);

    return noErr;
}

static void unsubscribe_device_listeners(struct SoundIoPrivate *si) {
    struct SoundIoCoreAudio *sica = &si->backend_data.coreaudio;

    for (int device_index = 0; device_index < sica->registered_listeners.length; device_index += 1) {
        AudioDeviceID device_id = SoundIoListAudioDeviceID_val_at(&sica->registered_listeners, device_index);
        for (int i = 0; i < ARRAY_LENGTH(device_listen_props); i += 1) {
            AudioObjectRemovePropertyListener(device_id, &device_listen_props[i],
                on_devices_changed, si);
        }
    }
    SoundIoListAudioDeviceID_clear(&sica->registered_listeners);
}

static void destroy_ca(struct SoundIoPrivate *si) {
    struct SoundIoCoreAudio *sica = &si->backend_data.coreaudio;

    AudioObjectPropertyAddress prop_address = {
        kAudioHardwarePropertyDevices,
        kAudioObjectPropertyScopeGlobal,
        kAudioObjectPropertyElementMaster
    };
    AudioObjectRemovePropertyListener(kAudioObjectSystemObject, &prop_address, on_devices_changed, si);

    prop_address.mSelector = kAudioHardwarePropertyServiceRestarted;
    AudioObjectRemovePropertyListener(kAudioObjectSystemObject, &prop_address, on_service_restarted, si);

    unsubscribe_device_listeners(si);
    SoundIoListAudioDeviceID_deinit(&sica->registered_listeners);

    if (sica->thread) {
        SOUNDIO_ATOMIC_FLAG_CLEAR(sica->abort_flag);
        soundio_os_cond_signal(sica->scan_devices_cond, NULL);
        soundio_os_thread_destroy(sica->thread);
    }

    if (sica->cond)
        soundio_os_cond_destroy(sica->cond);

    if (sica->have_devices_cond)
        soundio_os_cond_destroy(sica->have_devices_cond);

    if (sica->scan_devices_cond)
        soundio_os_cond_destroy(sica->scan_devices_cond);

    if (sica->mutex)
        soundio_os_mutex_destroy(sica->mutex);

    soundio_destroy_devices_info(sica->ready_devices_info);
}

// Possible errors:
//  * SoundIoErrorNoMem
//  * SoundIoErrorEncodingString
static int from_cf_string(CFStringRef string_ref, char **out_str, int *out_str_len) {
    assert(string_ref);

    CFIndex length = CFStringGetLength(string_ref);
    CFIndex max_size = CFStringGetMaximumSizeForEncoding(length, kCFStringEncodingUTF8);
    char *buf = ALLOCATE_NONZERO(char, max_size);
    if (!buf)
        return SoundIoErrorNoMem;

    if (!CFStringGetCString(string_ref, buf, max_size, kCFStringEncodingUTF8)) {
        free(buf);
        return SoundIoErrorEncodingString;
    }

    *out_str = buf;
    *out_str_len = strlen(buf);
    return 0;
}

static int aim_to_scope(enum SoundIoDeviceAim aim) {
    return (aim == SoundIoDeviceAimInput) ?
        kAudioObjectPropertyScopeInput : kAudioObjectPropertyScopeOutput;
}

static enum SoundIoChannelId from_channel_descr(const AudioChannelDescription *descr) {
    switch (descr->mChannelLabel) {
        default:                                        return SoundIoChannelIdInvalid;
        case kAudioChannelLabel_Left:                   return SoundIoChannelIdFrontLeft;
        case kAudioChannelLabel_Right:                  return SoundIoChannelIdFrontRight;
        case kAudioChannelLabel_Center:                 return SoundIoChannelIdFrontCenter;
        case kAudioChannelLabel_LFEScreen:              return SoundIoChannelIdLfe;
        case kAudioChannelLabel_LeftSurround:           return SoundIoChannelIdBackLeft;
        case kAudioChannelLabel_RightSurround:          return SoundIoChannelIdBackRight;
        case kAudioChannelLabel_LeftCenter:             return SoundIoChannelIdFrontLeftCenter;
        case kAudioChannelLabel_RightCenter:            return SoundIoChannelIdFrontRightCenter;
        case kAudioChannelLabel_CenterSurround:         return SoundIoChannelIdBackCenter;
        case kAudioChannelLabel_LeftSurroundDirect:     return SoundIoChannelIdSideLeft;
        case kAudioChannelLabel_RightSurroundDirect:    return SoundIoChannelIdSideRight;
        case kAudioChannelLabel_TopCenterSurround:      return SoundIoChannelIdTopCenter;
        case kAudioChannelLabel_VerticalHeightLeft:     return SoundIoChannelIdTopFrontLeft;
        case kAudioChannelLabel_VerticalHeightCenter:   return SoundIoChannelIdTopFrontCenter;
        case kAudioChannelLabel_VerticalHeightRight:    return SoundIoChannelIdTopFrontRight;
        case kAudioChannelLabel_TopBackLeft:            return SoundIoChannelIdTopBackLeft;
        case kAudioChannelLabel_TopBackCenter:          return SoundIoChannelIdTopBackCenter;
        case kAudioChannelLabel_TopBackRight:           return SoundIoChannelIdTopBackRight;
        case kAudioChannelLabel_RearSurroundLeft:       return SoundIoChannelIdBackLeft;
        case kAudioChannelLabel_RearSurroundRight:      return SoundIoChannelIdBackRight;
        case kAudioChannelLabel_LeftWide:               return SoundIoChannelIdFrontLeftWide;
        case kAudioChannelLabel_RightWide:              return SoundIoChannelIdFrontRightWide;
        case kAudioChannelLabel_LFE2:                   return SoundIoChannelIdLfe2;
        case kAudioChannelLabel_LeftTotal:              return SoundIoChannelIdFrontLeft;
        case kAudioChannelLabel_RightTotal:             return SoundIoChannelIdFrontRight;
        case kAudioChannelLabel_HearingImpaired:        return SoundIoChannelIdHearingImpaired;
        case kAudioChannelLabel_Narration:              return SoundIoChannelIdNarration;
        case kAudioChannelLabel_Mono:                   return SoundIoChannelIdFrontCenter;
        case kAudioChannelLabel_DialogCentricMix:       return SoundIoChannelIdDialogCentricMix;
        case kAudioChannelLabel_CenterSurroundDirect:   return SoundIoChannelIdBackCenter;
        case kAudioChannelLabel_Haptic:                 return SoundIoChannelIdHaptic;

        case kAudioChannelLabel_Ambisonic_W:            return SoundIoChannelIdAmbisonicW;
        case kAudioChannelLabel_Ambisonic_X:            return SoundIoChannelIdAmbisonicX;
        case kAudioChannelLabel_Ambisonic_Y:            return SoundIoChannelIdAmbisonicY;
        case kAudioChannelLabel_Ambisonic_Z:            return SoundIoChannelIdAmbisonicZ;

        case kAudioChannelLabel_MS_Mid:                 return SoundIoChannelIdMsMid;
        case kAudioChannelLabel_MS_Side:                return SoundIoChannelIdMsSide;

        case kAudioChannelLabel_XY_X:                   return SoundIoChannelIdXyX;
        case kAudioChannelLabel_XY_Y:                   return SoundIoChannelIdXyY;

        case kAudioChannelLabel_HeadphonesLeft:         return SoundIoChannelIdHeadphonesLeft;
        case kAudioChannelLabel_HeadphonesRight:        return SoundIoChannelIdHeadphonesRight;
        case kAudioChannelLabel_ClickTrack:             return SoundIoChannelIdClickTrack;
        case kAudioChannelLabel_ForeignLanguage:        return SoundIoChannelIdForeignLanguage;

        case kAudioChannelLabel_Discrete:               return SoundIoChannelIdAux;

        case kAudioChannelLabel_Discrete_0:             return SoundIoChannelIdAux0;
        case kAudioChannelLabel_Discrete_1:             return SoundIoChannelIdAux1;
        case kAudioChannelLabel_Discrete_2:             return SoundIoChannelIdAux2;
        case kAudioChannelLabel_Discrete_3:             return SoundIoChannelIdAux3;
        case kAudioChannelLabel_Discrete_4:             return SoundIoChannelIdAux4;
        case kAudioChannelLabel_Discrete_5:             return SoundIoChannelIdAux5;
        case kAudioChannelLabel_Discrete_6:             return SoundIoChannelIdAux6;
        case kAudioChannelLabel_Discrete_7:             return SoundIoChannelIdAux7;
        case kAudioChannelLabel_Discrete_8:             return SoundIoChannelIdAux8;
        case kAudioChannelLabel_Discrete_9:             return SoundIoChannelIdAux9;
        case kAudioChannelLabel_Discrete_10:            return SoundIoChannelIdAux10;
        case kAudioChannelLabel_Discrete_11:            return SoundIoChannelIdAux11;
        case kAudioChannelLabel_Discrete_12:            return SoundIoChannelIdAux12;
        case kAudioChannelLabel_Discrete_13:            return SoundIoChannelIdAux13;
        case kAudioChannelLabel_Discrete_14:            return SoundIoChannelIdAux14;
        case kAudioChannelLabel_Discrete_15:            return SoundIoChannelIdAux15;
    }
}

// See https://developer.apple.com/library/mac/documentation/MusicAudio/Reference/CoreAudioDataTypesRef/#//apple_ref/doc/constant_group/Audio_Channel_Layout_Tags
// Possible Errors:
// * SoundIoErrorIncompatibleDevice
// This does not handle all the possible layout enum values and it does not
// handle channel bitmaps.
static int from_coreaudio_layout(const AudioChannelLayout *ca_layout, struct SoundIoChannelLayout *layout) {
    switch (ca_layout->mChannelLayoutTag) {
    case kAudioChannelLayoutTag_UseChannelDescriptions:
    {
        layout->channel_count = soundio_int_min(
            SOUNDIO_MAX_CHANNELS,
            ca_layout->mNumberChannelDescriptions
        );
        for (int i = 0; i < layout->channel_count; i += 1) {
            layout->channels[i] = from_channel_descr(&ca_layout->mChannelDescriptions[i]);
        }
        break;
    }
    case kAudioChannelLayoutTag_UseChannelBitmap:
        return SoundIoErrorIncompatibleDevice;
    case kAudioChannelLayoutTag_Mono:
        layout->channel_count = 1;
        layout->channels[0] = SoundIoChannelIdFrontCenter;
        break;
    case kAudioChannelLayoutTag_Stereo:
    case kAudioChannelLayoutTag_StereoHeadphones:
    case kAudioChannelLayoutTag_MatrixStereo:
    case kAudioChannelLayoutTag_Binaural:
        layout->channel_count = 2;
        layout->channels[0] = SoundIoChannelIdFrontLeft;
        layout->channels[1] = SoundIoChannelIdFrontRight;
        break;
    case kAudioChannelLayoutTag_XY:
        layout->channel_count = 2;
        layout->channels[0] = SoundIoChannelIdXyX;
        layout->channels[1] = SoundIoChannelIdXyY;
        break;
    case kAudioChannelLayoutTag_MidSide:
        layout->channel_count = 2;
        layout->channels[0] = SoundIoChannelIdMsMid;
        layout->channels[1] = SoundIoChannelIdMsSide;
        break;
    case kAudioChannelLayoutTag_Ambisonic_B_Format:
        layout->channel_count = 4;
        layout->channels[0] = SoundIoChannelIdAmbisonicW;
        layout->channels[1] = SoundIoChannelIdAmbisonicX;
        layout->channels[2] = SoundIoChannelIdAmbisonicY;
        layout->channels[3] = SoundIoChannelIdAmbisonicZ;
        break;
    case kAudioChannelLayoutTag_Quadraphonic:
        layout->channel_count = 4;
        layout->channels[0] = SoundIoChannelIdFrontLeft;
        layout->channels[1] = SoundIoChannelIdFrontRight;
        layout->channels[2] = SoundIoChannelIdBackLeft;
        layout->channels[3] = SoundIoChannelIdBackRight;
        break;
    case kAudioChannelLayoutTag_Pentagonal:
        layout->channel_count = 5;
        layout->channels[0] = SoundIoChannelIdSideLeft;
        layout->channels[1] = SoundIoChannelIdSideRight;
        layout->channels[2] = SoundIoChannelIdBackLeft;
        layout->channels[3] = SoundIoChannelIdBackRight;
        layout->channels[4] = SoundIoChannelIdFrontCenter;
        break;
    case kAudioChannelLayoutTag_Hexagonal:
        layout->channel_count = 6;
        layout->channels[0] = SoundIoChannelIdFrontLeft;
        layout->channels[1] = SoundIoChannelIdFrontRight;
        layout->channels[2] = SoundIoChannelIdBackLeft;
        layout->channels[3] = SoundIoChannelIdBackRight;
        layout->channels[4] = SoundIoChannelIdFrontCenter;
        layout->channels[5] = SoundIoChannelIdBackCenter;
        break;
    case kAudioChannelLayoutTag_Octagonal:
        layout->channel_count = 8;
        layout->channels[0] = SoundIoChannelIdFrontLeft;
        layout->channels[1] = SoundIoChannelIdFrontRight;
        layout->channels[2] = SoundIoChannelIdBackLeft;
        layout->channels[3] = SoundIoChannelIdBackRight;
        layout->channels[4] = SoundIoChannelIdFrontCenter;
        layout->channels[5] = SoundIoChannelIdBackCenter;
        layout->channels[6] = SoundIoChannelIdSideLeft;
        layout->channels[7] = SoundIoChannelIdSideRight;
        break;
    case kAudioChannelLayoutTag_Cube:
        layout->channel_count = 8;
        layout->channels[0] = SoundIoChannelIdFrontLeft;
        layout->channels[1] = SoundIoChannelIdFrontRight;
        layout->channels[2] = SoundIoChannelIdBackLeft;
        layout->channels[3] = SoundIoChannelIdBackRight;
        layout->channels[4] = SoundIoChannelIdTopFrontLeft;
        layout->channels[5] = SoundIoChannelIdTopFrontRight;
        layout->channels[6] = SoundIoChannelIdTopBackLeft;
        layout->channels[7] = SoundIoChannelIdTopBackRight;
        break;
    default:
        return SoundIoErrorIncompatibleDevice;
    }
    soundio_channel_layout_detect_builtin(layout);
    return 0;
}

static bool all_channels_invalid(const struct SoundIoChannelLayout *layout) {
    for (int i = 0; i < layout->channel_count; i += 1) {
        if (layout->channels[i] != SoundIoChannelIdInvalid)
            return false;
    }
    return true;
}

struct RefreshDevices {
    struct SoundIoPrivate *si;
    struct SoundIoDevicesInfo *devices_info;
    int devices_size;
    AudioObjectID *devices;
    CFStringRef string_ref;
    char *device_name;
    int device_name_len;
    AudioBufferList *buffer_list;
    struct SoundIoDevice *device;
    AudioChannelLayout *audio_channel_layout;
    char *device_uid;
    int device_uid_len;
    AudioValueRange *avr_array;
    bool ok;
};

static void deinit_refresh_devices(struct RefreshDevices *rd) {
    if (!rd->ok)
        unsubscribe_device_listeners(rd->si);
    soundio_destroy_devices_info(rd->devices_info);
    free(rd->devices);
    if (rd->string_ref)
        CFRelease(rd->string_ref);
    free(rd->device_name);
    free(rd->buffer_list);
    soundio_device_unref(rd->device);
    free(rd->audio_channel_layout);
    free(rd->device_uid);
    free(rd->avr_array);
}

static int refresh_devices(struct SoundIoPrivate *si) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoCoreAudio *sica = &si->backend_data.coreaudio;

    UInt32 io_size;
    OSStatus os_err;
    int err;

    unsubscribe_device_listeners(si);

    struct RefreshDevices rd = {0};
    rd.si = si;

    if (!(rd.devices_info = ALLOCATE(struct SoundIoDevicesInfo, 1))) {
        deinit_refresh_devices(&rd);
        return SoundIoErrorNoMem;
    }

    AudioObjectPropertyAddress prop_address = {
        kAudioHardwarePropertyDevices,
        kAudioObjectPropertyScopeGlobal,
        kAudioObjectPropertyElementMaster
    };

    if ((os_err = AudioObjectGetPropertyDataSize(kAudioObjectSystemObject,
        &prop_address, 0, NULL, &io_size)))
    {
        deinit_refresh_devices(&rd);
        return SoundIoErrorOpeningDevice;
    }

    AudioObjectID default_input_id;
    AudioObjectID default_output_id;

    int device_count = io_size / (UInt32)sizeof(AudioObjectID);
    if (device_count >= 1) {
        rd.devices_size = io_size;
        rd.devices = (AudioObjectID *)ALLOCATE(char, rd.devices_size);
        if (!rd.devices) {
            deinit_refresh_devices(&rd);
            return SoundIoErrorNoMem;
        }

        if ((os_err = AudioObjectGetPropertyData(kAudioObjectSystemObject, &prop_address, 0, NULL,
            &io_size, rd.devices)))
        {
            deinit_refresh_devices(&rd);
            return SoundIoErrorOpeningDevice;
        }


        io_size = sizeof(AudioObjectID);
        prop_address.mSelector = kAudioHardwarePropertyDefaultInputDevice;
        if ((os_err = AudioObjectGetPropertyData(kAudioObjectSystemObject, &prop_address,
            0, NULL, &io_size, &default_input_id)))
        {
            deinit_refresh_devices(&rd);
            return SoundIoErrorOpeningDevice;
        }

        io_size = sizeof(AudioObjectID);
        prop_address.mSelector = kAudioHardwarePropertyDefaultOutputDevice;
        if ((os_err = AudioObjectGetPropertyData(kAudioObjectSystemObject, &prop_address,
            0, NULL, &io_size, &default_output_id)))
        {
            deinit_refresh_devices(&rd);
            return SoundIoErrorOpeningDevice;
        }
    }

    for (int device_i = 0; device_i < device_count; device_i += 1) {
        AudioObjectID device_id = rd.devices[device_i];

        for (int i = 0; i < ARRAY_LENGTH(device_listen_props); i += 1) {
            if ((err = SoundIoListAudioDeviceID_add_one(&sica->registered_listeners))) {
                deinit_refresh_devices(&rd);
                return SoundIoErrorOpeningDevice;
            }
            if ((os_err = AudioObjectAddPropertyListener(device_id, &device_listen_props[i],
                on_devices_changed, si)))
            {
                deinit_refresh_devices(&rd);
                return SoundIoErrorOpeningDevice;
            }
            AudioDeviceID *last_device_id = SoundIoListAudioDeviceID_last_ptr(&sica->registered_listeners);
            *last_device_id = device_id;
        }

        prop_address.mSelector = kAudioObjectPropertyName;
        prop_address.mScope = kAudioObjectPropertyScopeGlobal;
        prop_address.mElement = kAudioObjectPropertyElementMaster;
        io_size = sizeof(CFStringRef);
        if (rd.string_ref) {
            CFRelease(rd.string_ref);
            rd.string_ref = NULL;
        }
        if ((os_err = AudioObjectGetPropertyData(device_id, &prop_address,
            0, NULL, &io_size, &rd.string_ref)))
        {
            deinit_refresh_devices(&rd);
            return SoundIoErrorOpeningDevice;
        }

        free(rd.device_name);
        rd.device_name = NULL;
        if ((err = from_cf_string(rd.string_ref, &rd.device_name, &rd.device_name_len))) {
            deinit_refresh_devices(&rd);
            return err;
        }

        prop_address.mSelector = kAudioDevicePropertyDeviceUID;
        prop_address.mScope = kAudioObjectPropertyScopeGlobal;
        prop_address.mElement = kAudioObjectPropertyElementMaster;
        io_size = sizeof(CFStringRef);
        if (rd.string_ref) {
            CFRelease(rd.string_ref);
            rd.string_ref = NULL;
        }
        if ((os_err = AudioObjectGetPropertyData(device_id, &prop_address,
            0, NULL, &io_size, &rd.string_ref)))
        {
            deinit_refresh_devices(&rd);
            return SoundIoErrorOpeningDevice;
        }

        free(rd.device_uid);
        rd.device_uid = NULL;
        if ((err = from_cf_string(rd.string_ref, &rd.device_uid, &rd.device_uid_len))) {
            deinit_refresh_devices(&rd);
            return err;
        }


        for (int aim_i = 0; aim_i < ARRAY_LENGTH(aims); aim_i += 1) {
            enum SoundIoDeviceAim aim = aims[aim_i];

            io_size = 0;
            prop_address.mSelector = kAudioDevicePropertyStreamConfiguration;
            prop_address.mScope = aim_to_scope(aim);
            prop_address.mElement = kAudioObjectPropertyElementMaster;
            if ((os_err = AudioObjectGetPropertyDataSize(device_id, &prop_address, 0, NULL, &io_size))) {
                deinit_refresh_devices(&rd);
                return SoundIoErrorOpeningDevice;
            }

            free(rd.buffer_list);
            rd.buffer_list = (AudioBufferList*)ALLOCATE_NONZERO(char, io_size);
            if (!rd.buffer_list) {
                deinit_refresh_devices(&rd);
                return SoundIoErrorNoMem;
            }

            if ((os_err = AudioObjectGetPropertyData(device_id, &prop_address, 0, NULL,
                &io_size, rd.buffer_list)))
            {
                deinit_refresh_devices(&rd);
                return SoundIoErrorOpeningDevice;
            }

            int channel_count = 0;
            for (int i = 0; i < rd.buffer_list->mNumberBuffers; i += 1) {
                channel_count += rd.buffer_list->mBuffers[i].mNumberChannels;
            }

            if (channel_count <= 0)
                continue;

            struct SoundIoDevicePrivate *dev = ALLOCATE(struct SoundIoDevicePrivate, 1);
            if (!dev) {
                deinit_refresh_devices(&rd);
                return SoundIoErrorNoMem;
            }
            struct SoundIoDeviceCoreAudio *dca = &dev->backend_data.coreaudio;
            dca->device_id = device_id;
            assert(!rd.device);
            rd.device = &dev->pub;
            rd.device->ref_count = 1;
            rd.device->soundio = soundio;
            rd.device->is_raw = false;
            rd.device->aim = aim;
            rd.device->id = soundio_str_dupe(rd.device_uid, rd.device_uid_len);
            rd.device->name = soundio_str_dupe(rd.device_name, rd.device_name_len);

            if (!rd.device->id || !rd.device->name) {
                deinit_refresh_devices(&rd);
                return SoundIoErrorNoMem;
            }

            prop_address.mSelector = kAudioDevicePropertyPreferredChannelLayout;
            prop_address.mScope = aim_to_scope(aim);
            prop_address.mElement = kAudioObjectPropertyElementMaster;
            if (!(os_err = AudioObjectGetPropertyDataSize(device_id, &prop_address,
                0, NULL, &io_size)))
            {
                rd.audio_channel_layout = (AudioChannelLayout *)ALLOCATE(char, io_size);
                if (!rd.audio_channel_layout) {
                    deinit_refresh_devices(&rd);
                    return SoundIoErrorNoMem;
                }
                if ((os_err = AudioObjectGetPropertyData(device_id, &prop_address, 0, NULL,
                    &io_size, rd.audio_channel_layout)))
                {
                    deinit_refresh_devices(&rd);
                    return SoundIoErrorOpeningDevice;
                }
                if ((err = from_coreaudio_layout(rd.audio_channel_layout, &rd.device->current_layout))) {
                    rd.device->current_layout.channel_count = channel_count;
                }
            }
            if (all_channels_invalid(&rd.device->current_layout)) {
                const struct SoundIoChannelLayout *guessed_layout =
                    soundio_channel_layout_get_default(channel_count);
                if (guessed_layout)
                    rd.device->current_layout = *guessed_layout;
            }

            rd.device->layout_count = 1;
            rd.device->layouts = &rd.device->current_layout;

            rd.device->format_count = 4;
            rd.device->formats = ALLOCATE(enum SoundIoFormat, rd.device->format_count);
            if (!rd.device->formats)
                return SoundIoErrorNoMem;
            rd.device->formats[0] = SoundIoFormatS16LE;
            rd.device->formats[1] = SoundIoFormatS32LE;
            rd.device->formats[2] = SoundIoFormatFloat32LE;
            rd.device->formats[3] = SoundIoFormatFloat64LE;

            prop_address.mSelector = kAudioDevicePropertyNominalSampleRate;
            prop_address.mScope = aim_to_scope(aim);
            prop_address.mElement = kAudioObjectPropertyElementMaster;
            io_size = sizeof(double);
            double value;
            if ((os_err = AudioObjectGetPropertyData(device_id, &prop_address, 0, NULL,
                &io_size, &value)))
            {
                deinit_refresh_devices(&rd);
                return SoundIoErrorOpeningDevice;
            }
            double floored_value = floor(value);
            if (value != floored_value) {
                deinit_refresh_devices(&rd);
                return SoundIoErrorIncompatibleDevice;
            }
            rd.device->sample_rate_current = (int)floored_value;

            // If you try to open an input stream with anything but the current
            // nominal sample rate, AudioUnitRender returns an error.
            if (aim == SoundIoDeviceAimInput) {
                rd.device->sample_rate_count = 1;
                rd.device->sample_rates = &dev->prealloc_sample_rate_range;
                rd.device->sample_rates[0].min = rd.device->sample_rate_current;
                rd.device->sample_rates[0].max = rd.device->sample_rate_current;
            } else {
                prop_address.mSelector = kAudioDevicePropertyAvailableNominalSampleRates;
                prop_address.mScope = aim_to_scope(aim);
                prop_address.mElement = kAudioObjectPropertyElementMaster;
                if ((os_err = AudioObjectGetPropertyDataSize(device_id, &prop_address, 0, NULL,
                    &io_size)))
                {
                    deinit_refresh_devices(&rd);
                    return SoundIoErrorOpeningDevice;
                }
                int avr_array_len = io_size / sizeof(AudioValueRange);
                rd.avr_array = (AudioValueRange*)ALLOCATE(char, io_size);

                if (!rd.avr_array) {
                    deinit_refresh_devices(&rd);
                    return SoundIoErrorNoMem;
                }

                if ((os_err = AudioObjectGetPropertyData(device_id, &prop_address, 0, NULL,
                    &io_size, rd.avr_array)))
                {
                    deinit_refresh_devices(&rd);
                    return SoundIoErrorOpeningDevice;
                }

                if (avr_array_len == 1) {
                    rd.device->sample_rate_count = 1;
                    rd.device->sample_rates = &dev->prealloc_sample_rate_range;
                    rd.device->sample_rates[0].min = ceil_dbl_to_int(rd.avr_array[0].mMinimum);
                    rd.device->sample_rates[0].max = (int)(rd.avr_array[0].mMaximum);
                } else {
                    rd.device->sample_rate_count = avr_array_len;
                    rd.device->sample_rates = ALLOCATE(struct SoundIoSampleRateRange, avr_array_len);
                    if (!rd.device->sample_rates) {
                        deinit_refresh_devices(&rd);
                        return SoundIoErrorNoMem;
                    }
                    for (int i = 0; i < avr_array_len; i += 1) {
                        AudioValueRange *avr = &rd.avr_array[i];
                        int min_val = ceil_dbl_to_int(avr->mMinimum);
                        int max_val = (int)(avr->mMaximum);
                        rd.device->sample_rates[i].min = min_val;
                        rd.device->sample_rates[i].max = max_val;
                    }
                }
            }

            prop_address.mSelector = kAudioDevicePropertyBufferFrameSize;
            prop_address.mScope = aim_to_scope(aim);
            prop_address.mElement = kAudioObjectPropertyElementMaster;
            io_size = sizeof(UInt32);
            UInt32 buffer_frame_size;
            if ((os_err = AudioObjectGetPropertyData(device_id, &prop_address, 0, NULL,
                &io_size, &buffer_frame_size)))
            {
                deinit_refresh_devices(&rd);
                return SoundIoErrorOpeningDevice;
            }
            double use_sample_rate = rd.device->sample_rate_current;
            rd.device->software_latency_current = buffer_frame_size / use_sample_rate;

            prop_address.mSelector = kAudioDevicePropertyBufferFrameSizeRange;
            prop_address.mScope = aim_to_scope(aim);
            prop_address.mElement = kAudioObjectPropertyElementMaster;
            io_size = sizeof(AudioValueRange);
            AudioValueRange avr;
            if ((os_err = AudioObjectGetPropertyData(device_id, &prop_address, 0, NULL,
                &io_size, &avr)))
            {
                deinit_refresh_devices(&rd);
                return SoundIoErrorOpeningDevice;
            }
            rd.device->software_latency_min = avr.mMinimum / use_sample_rate;
            rd.device->software_latency_max = avr.mMaximum / use_sample_rate;

            prop_address.mSelector = kAudioDevicePropertyLatency;
            prop_address.mScope = aim_to_scope(aim);
            prop_address.mElement = kAudioObjectPropertyElementMaster;
            io_size = sizeof(UInt32);
            if ((os_err = AudioObjectGetPropertyData(device_id, &prop_address, 0, NULL,
                &io_size, &dca->latency_frames)))
            {
                deinit_refresh_devices(&rd);
                return SoundIoErrorOpeningDevice;
            }

            struct SoundIoListDevicePtr *device_list;
            if (rd.device->aim == SoundIoDeviceAimOutput) {
                device_list = &rd.devices_info->output_devices;
                if (device_id == default_output_id)
                    rd.devices_info->default_output_index = device_list->length;
            } else {
                assert(rd.device->aim == SoundIoDeviceAimInput);
                device_list = &rd.devices_info->input_devices;
                if (device_id == default_input_id)
                    rd.devices_info->default_input_index = device_list->length;
            }

            if ((err = SoundIoListDevicePtr_append(device_list, rd.device))) {
                deinit_refresh_devices(&rd);
                return err;
            }
            rd.device = NULL;
        }
    }

    soundio_os_mutex_lock(sica->mutex);
    soundio_destroy_devices_info(sica->ready_devices_info);
    sica->ready_devices_info = rd.devices_info;
    soundio_os_mutex_unlock(sica->mutex);

    rd.devices_info = NULL;
    rd.ok = true;
    deinit_refresh_devices(&rd);

    return 0;
}

static void shutdown_backend(struct SoundIoPrivate *si, int err) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoCoreAudio *sica = &si->backend_data.coreaudio;
    soundio_os_mutex_lock(sica->mutex);
    sica->shutdown_err = err;
    SOUNDIO_ATOMIC_STORE(sica->have_devices_flag, true);
    soundio_os_mutex_unlock(sica->mutex);
    soundio_os_cond_signal(sica->cond, NULL);
    soundio_os_cond_signal(sica->have_devices_cond, NULL);
    soundio->on_events_signal(soundio);
}

static void flush_events_ca(struct SoundIoPrivate *si) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoCoreAudio *sica = &si->backend_data.coreaudio;

    // block until have devices
    while (!SOUNDIO_ATOMIC_LOAD(sica->have_devices_flag))
        soundio_os_cond_wait(sica->have_devices_cond, NULL);

    bool change = false;
    bool cb_shutdown = false;
    struct SoundIoDevicesInfo *old_devices_info = NULL;

    soundio_os_mutex_lock(sica->mutex);

    if (sica->shutdown_err && !sica->emitted_shutdown_cb) {
        sica->emitted_shutdown_cb = true;
        cb_shutdown = true;
    } else if (sica->ready_devices_info) {
        old_devices_info = si->safe_devices_info;
        si->safe_devices_info = sica->ready_devices_info;
        sica->ready_devices_info = NULL;
        change = true;
    }

    soundio_os_mutex_unlock(sica->mutex);

    if (cb_shutdown)
        soundio->on_backend_disconnect(soundio, sica->shutdown_err);
    else if (change)
        soundio->on_devices_change(soundio);

    soundio_destroy_devices_info(old_devices_info);
}

static void wait_events_ca(struct SoundIoPrivate *si) {
    struct SoundIoCoreAudio *sica = &si->backend_data.coreaudio;
    flush_events_ca(si);
    soundio_os_cond_wait(sica->cond, NULL);
}

static void wakeup_ca(struct SoundIoPrivate *si) {
    struct SoundIoCoreAudio *sica = &si->backend_data.coreaudio;
    soundio_os_cond_signal(sica->cond, NULL);
}

static void force_device_scan_ca(struct SoundIoPrivate *si) {
    struct SoundIoCoreAudio *sica = &si->backend_data.coreaudio;
    SOUNDIO_ATOMIC_STORE(sica->device_scan_queued, true);
    soundio_os_cond_signal(sica->scan_devices_cond, NULL);
}

static void device_thread_run(void *arg) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)arg;
    struct SoundIo *soundio = &si->pub;
    struct SoundIoCoreAudio *sica = &si->backend_data.coreaudio;
    int err;

    for (;;) {
        if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(sica->abort_flag))
            break;
        if (SOUNDIO_ATOMIC_LOAD(sica->service_restarted)) {
            shutdown_backend(si, SoundIoErrorBackendDisconnected);
            return;
        }
        if (SOUNDIO_ATOMIC_EXCHANGE(sica->device_scan_queued, false)) {
            err = refresh_devices(si);
            if (err) {
                shutdown_backend(si, err);
                return;
            }
            if (!SOUNDIO_ATOMIC_EXCHANGE(sica->have_devices_flag, true))
                soundio_os_cond_signal(sica->have_devices_cond, NULL);
            soundio_os_cond_signal(sica->cond, NULL);
            soundio->on_events_signal(soundio);
        }
        soundio_os_cond_wait(sica->scan_devices_cond, NULL);
    }
}

static OSStatus on_outstream_device_overload(AudioObjectID in_object_id, UInt32 in_number_addresses,
    const AudioObjectPropertyAddress in_addresses[], void *in_client_data)
{
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)in_client_data;
    struct SoundIoOutStream *outstream = &os->pub;
    outstream->underflow_callback(outstream);
    return noErr;
}

static void outstream_destroy_ca(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamCoreAudio *osca = &os->backend_data.coreaudio;
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoDevice *device = outstream->device;
    struct SoundIoDevicePrivate *dev = (struct SoundIoDevicePrivate *)device;
    struct SoundIoDeviceCoreAudio *dca = &dev->backend_data.coreaudio;

    AudioObjectPropertyAddress prop_address = {
        kAudioDeviceProcessorOverload,
        kAudioObjectPropertyScopeGlobal,
        kAudioObjectPropertyElementMaster
    };
    AudioObjectRemovePropertyListener(dca->device_id, &prop_address, on_outstream_device_overload, os);

    if (osca->instance) {
        AudioOutputUnitStop(osca->instance);
        AudioComponentInstanceDispose(osca->instance);
        osca->instance = NULL;
    }
}

static OSStatus write_callback_ca(void *userdata, AudioUnitRenderActionFlags *io_action_flags,
    const AudioTimeStamp *in_time_stamp, UInt32 in_bus_number, UInt32 in_number_frames,
    AudioBufferList *io_data)
{
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *) userdata;
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoOutStreamCoreAudio *osca = &os->backend_data.coreaudio;

    osca->io_data = io_data;
    osca->buffer_index = 0;
    osca->frames_left = in_number_frames;
    outstream->write_callback(outstream, osca->frames_left, osca->frames_left);
    osca->io_data = NULL;

    return noErr;
}

static int set_ca_desc(enum SoundIoFormat fmt, AudioStreamBasicDescription *desc) {
    switch (fmt) {
    case SoundIoFormatFloat32LE:
        desc->mFormatFlags = kAudioFormatFlagIsFloat;
        desc->mBitsPerChannel = 32;
        break;
    case SoundIoFormatFloat64LE:
        desc->mFormatFlags = kAudioFormatFlagIsFloat;
        desc->mBitsPerChannel = 64;
        break;
    case SoundIoFormatS32LE:
        desc->mFormatFlags = kAudioFormatFlagIsSignedInteger;
        desc->mBitsPerChannel = 32;
        break;
    case SoundIoFormatS16LE:
        desc->mFormatFlags = kAudioFormatFlagIsSignedInteger;
        desc->mBitsPerChannel = 16;
        break;
    default:
        return SoundIoErrorIncompatibleDevice;
    }
    return 0;
}

static int outstream_open_ca(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamCoreAudio *osca = &os->backend_data.coreaudio;
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoDevice *device = outstream->device;
    struct SoundIoDevicePrivate *dev = (struct SoundIoDevicePrivate *)device;
    struct SoundIoDeviceCoreAudio *dca = &dev->backend_data.coreaudio;

    if (outstream->software_latency == 0.0)
        outstream->software_latency = device->software_latency_current;

    outstream->software_latency = soundio_double_clamp(
            device->software_latency_min,
            outstream->software_latency,
            device->software_latency_max);

    AudioComponentDescription desc = {0};
    desc.componentType = kAudioUnitType_Output;
    desc.componentSubType = kAudioUnitSubType_HALOutput;
    desc.componentManufacturer = kAudioUnitManufacturer_Apple;

    AudioComponent component = AudioComponentFindNext(NULL, &desc);
    if (!component) {
        outstream_destroy_ca(si, os);
        return SoundIoErrorOpeningDevice;
    }

    OSStatus os_err;
    if ((os_err = AudioComponentInstanceNew(component, &osca->instance))) {
        outstream_destroy_ca(si, os);
        return SoundIoErrorOpeningDevice;
    }

    if ((os_err = AudioUnitInitialize(osca->instance))) {
        outstream_destroy_ca(si, os);
        return SoundIoErrorOpeningDevice;
    }

    AudioStreamBasicDescription format = {0};
    format.mSampleRate = outstream->sample_rate;
    format.mFormatID = kAudioFormatLinearPCM;
    int err;
    if ((err = set_ca_desc(outstream->format, &format))) {
        outstream_destroy_ca(si, os);
        return err;
    }
    format.mBytesPerPacket = outstream->bytes_per_frame;
    format.mFramesPerPacket = 1;
    format.mBytesPerFrame = outstream->bytes_per_frame;
    format.mChannelsPerFrame = outstream->layout.channel_count;

    if ((os_err = AudioUnitSetProperty(osca->instance, kAudioOutputUnitProperty_CurrentDevice,
        kAudioUnitScope_Input, OUTPUT_ELEMENT, &dca->device_id, sizeof(AudioDeviceID))))
    {
        outstream_destroy_ca(si, os);
        return SoundIoErrorOpeningDevice;
    }

    if ((os_err = AudioUnitSetProperty(osca->instance, kAudioUnitProperty_StreamFormat,
        kAudioUnitScope_Input, OUTPUT_ELEMENT, &format, sizeof(AudioStreamBasicDescription))))
    {
        outstream_destroy_ca(si, os);
        return SoundIoErrorIncompatibleDevice;
    }

    AURenderCallbackStruct render_callback = {write_callback_ca, os};
    if ((os_err = AudioUnitSetProperty(osca->instance, kAudioUnitProperty_SetRenderCallback,
        kAudioUnitScope_Input, OUTPUT_ELEMENT, &render_callback, sizeof(AURenderCallbackStruct))))
    {
        outstream_destroy_ca(si, os);
        return SoundIoErrorOpeningDevice;
    }

    AudioObjectPropertyAddress prop_address = {
        kAudioDevicePropertyBufferFrameSize,
        kAudioObjectPropertyScopeInput,
        OUTPUT_ELEMENT
    };
    UInt32 buffer_frame_size = outstream->software_latency * outstream->sample_rate;
    if ((os_err = AudioObjectSetPropertyData(dca->device_id, &prop_address,
        0, NULL, sizeof(UInt32), &buffer_frame_size)))
    {
        outstream_destroy_ca(si, os);
        return SoundIoErrorOpeningDevice;
    }

    prop_address.mSelector = kAudioDeviceProcessorOverload;
    prop_address.mScope = kAudioObjectPropertyScopeGlobal;
    prop_address.mElement = OUTPUT_ELEMENT;
    if ((os_err = AudioObjectAddPropertyListener(dca->device_id, &prop_address,
        on_outstream_device_overload, os)))
    {
        outstream_destroy_ca(si, os);
        return SoundIoErrorOpeningDevice;
    }

    osca->hardware_latency = dca->latency_frames / (double)outstream->sample_rate;

    return 0;
}

static int outstream_pause_ca(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os, bool pause) {
    struct SoundIoOutStreamCoreAudio *osca = &os->backend_data.coreaudio;
    OSStatus os_err;
    if (pause) {
        if ((os_err = AudioOutputUnitStop(osca->instance))) {
            return SoundIoErrorStreaming;
        }
    } else {
        if ((os_err = AudioOutputUnitStart(osca->instance))) {
            return SoundIoErrorStreaming;
        }
    }

    return 0;
}

static int outstream_start_ca(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    return outstream_pause_ca(si, os, false);
}

static int outstream_begin_write_ca(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os,
        struct SoundIoChannelArea **out_areas, int *frame_count)
{
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoOutStreamCoreAudio *osca = &os->backend_data.coreaudio;

    if (osca->buffer_index >= osca->io_data->mNumberBuffers)
        return SoundIoErrorInvalid;

    if (*frame_count != osca->frames_left)
        return SoundIoErrorInvalid;

    AudioBuffer *audio_buffer = &osca->io_data->mBuffers[osca->buffer_index];
    assert(audio_buffer->mNumberChannels == outstream->layout.channel_count);
    osca->write_frame_count = audio_buffer->mDataByteSize / outstream->bytes_per_frame;
    *frame_count = osca->write_frame_count;
    assert((audio_buffer->mDataByteSize % outstream->bytes_per_frame) == 0);
    for (int ch = 0; ch < outstream->layout.channel_count; ch += 1) {
        osca->areas[ch].ptr = ((char*)audio_buffer->mData) + outstream->bytes_per_sample * ch;
        osca->areas[ch].step = outstream->bytes_per_frame;
    }
    *out_areas = osca->areas;
    return 0;
}

static int outstream_end_write_ca(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamCoreAudio *osca = &os->backend_data.coreaudio;
    osca->buffer_index += 1;
    osca->frames_left -= osca->write_frame_count;
    assert(osca->frames_left >= 0);
    return 0;
}

static int outstream_clear_buffer_ca(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    return SoundIoErrorIncompatibleBackend;
}

static int outstream_get_latency_ca(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os,
        double *out_latency)
{
    struct SoundIoOutStreamCoreAudio *osca = &os->backend_data.coreaudio;
    *out_latency = osca->hardware_latency;
    return 0;
}

static OSStatus on_instream_device_overload(AudioObjectID in_object_id, UInt32 in_number_addresses,
    const AudioObjectPropertyAddress in_addresses[], void *in_client_data)
{
    struct SoundIoInStreamPrivate *os = (struct SoundIoInStreamPrivate *)in_client_data;
    struct SoundIoInStream *instream = &os->pub;
    instream->overflow_callback(instream);
    return noErr;
}

static void instream_destroy_ca(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamCoreAudio *isca = &is->backend_data.coreaudio;
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoDevice *device = instream->device;
    struct SoundIoDevicePrivate *dev = (struct SoundIoDevicePrivate *)device;
    struct SoundIoDeviceCoreAudio *dca = &dev->backend_data.coreaudio;

    AudioObjectPropertyAddress prop_address = {
        kAudioDeviceProcessorOverload,
        kAudioObjectPropertyScopeGlobal,
        kAudioObjectPropertyElementMaster
    };
    AudioObjectRemovePropertyListener(dca->device_id, &prop_address, on_instream_device_overload, is);

    if (isca->instance) {
        AudioOutputUnitStop(isca->instance);
        AudioComponentInstanceDispose(isca->instance);
        isca->instance = NULL;
    }

    free(isca->buffer_list);
    isca->buffer_list = NULL;
}

static OSStatus read_callback_ca(void *userdata, AudioUnitRenderActionFlags *io_action_flags,
    const AudioTimeStamp *in_time_stamp, UInt32 in_bus_number, UInt32 in_number_frames,
    AudioBufferList *io_data)
{
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *) userdata;
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoInStreamCoreAudio *isca = &is->backend_data.coreaudio;

    for (int i = 0; i < isca->buffer_list->mNumberBuffers; i += 1) {
        isca->buffer_list->mBuffers[i].mData = NULL;
    }

    OSStatus os_err;
    if ((os_err = AudioUnitRender(isca->instance, io_action_flags, in_time_stamp,
        in_bus_number, in_number_frames, isca->buffer_list)))
    {
        instream->error_callback(instream, SoundIoErrorStreaming);
        return noErr;
    }

    if (isca->buffer_list->mNumberBuffers == 1) {
        AudioBuffer *audio_buffer = &isca->buffer_list->mBuffers[0];
        assert(audio_buffer->mNumberChannels == instream->layout.channel_count);
        assert(audio_buffer->mDataByteSize == in_number_frames * instream->bytes_per_frame);
        for (int ch = 0; ch < instream->layout.channel_count; ch += 1) {
            isca->areas[ch].ptr = ((char*)audio_buffer->mData) + (instream->bytes_per_sample * ch);
            isca->areas[ch].step = instream->bytes_per_frame;
        }
    } else {
        assert(isca->buffer_list->mNumberBuffers == instream->layout.channel_count);
        for (int ch = 0; ch < instream->layout.channel_count; ch += 1) {
            AudioBuffer *audio_buffer = &isca->buffer_list->mBuffers[ch];
            assert(audio_buffer->mDataByteSize == in_number_frames * instream->bytes_per_sample);
            isca->areas[ch].ptr = (char*)audio_buffer->mData;
            isca->areas[ch].step = instream->bytes_per_sample;
        }
    }

    isca->frames_left = in_number_frames;
    instream->read_callback(instream, isca->frames_left, isca->frames_left);

    return noErr;
}

static int instream_open_ca(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamCoreAudio *isca = &is->backend_data.coreaudio;
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoDevice *device = instream->device;
    struct SoundIoDevicePrivate *dev = (struct SoundIoDevicePrivate *)device;
    struct SoundIoDeviceCoreAudio *dca = &dev->backend_data.coreaudio;
    UInt32 io_size;
    OSStatus os_err;

    if (instream->software_latency == 0.0)
        instream->software_latency = device->software_latency_current;

    instream->software_latency = soundio_double_clamp(
            device->software_latency_min,
            instream->software_latency,
            device->software_latency_max);


    AudioObjectPropertyAddress prop_address;
    prop_address.mSelector = kAudioDevicePropertyStreamConfiguration;
    prop_address.mScope = kAudioObjectPropertyScopeInput;
    prop_address.mElement = kAudioObjectPropertyElementMaster;
    io_size = 0;
    if ((os_err = AudioObjectGetPropertyDataSize(dca->device_id, &prop_address,
        0, NULL, &io_size)))
    {
        instream_destroy_ca(si, is);
        return SoundIoErrorOpeningDevice;
    }

    isca->buffer_list = (AudioBufferList*)ALLOCATE_NONZERO(char, io_size);
    if (!isca->buffer_list) {
        instream_destroy_ca(si, is);
        return SoundIoErrorNoMem;
    }

    if ((os_err = AudioObjectGetPropertyData(dca->device_id, &prop_address,
        0, NULL, &io_size, isca->buffer_list)))
    {
        instream_destroy_ca(si, is);
        return SoundIoErrorOpeningDevice;
    }


    AudioComponentDescription desc = {0};
    desc.componentType = kAudioUnitType_Output;
    desc.componentSubType = kAudioUnitSubType_HALOutput;
    desc.componentManufacturer = kAudioUnitManufacturer_Apple;

    AudioComponent component = AudioComponentFindNext(NULL, &desc);
    if (!component) {
        instream_destroy_ca(si, is);
        return SoundIoErrorOpeningDevice;
    }

    if ((os_err = AudioComponentInstanceNew(component, &isca->instance))) {
        instream_destroy_ca(si, is);
        return SoundIoErrorOpeningDevice;
    }

    if ((os_err = AudioUnitInitialize(isca->instance))) {
        instream_destroy_ca(si, is);
        return SoundIoErrorOpeningDevice;
    }

    UInt32 enable_io = 1;
    if ((os_err = AudioUnitSetProperty(isca->instance, kAudioOutputUnitProperty_EnableIO,
        kAudioUnitScope_Input, INPUT_ELEMENT, &enable_io, sizeof(UInt32))))
    {
        instream_destroy_ca(si, is);
        return SoundIoErrorOpeningDevice;
    }

    enable_io = 0;
    if ((os_err = AudioUnitSetProperty(isca->instance, kAudioOutputUnitProperty_EnableIO,
        kAudioUnitScope_Output, OUTPUT_ELEMENT, &enable_io, sizeof(UInt32))))
    {
        instream_destroy_ca(si, is);
        return SoundIoErrorOpeningDevice;
    }

    if ((os_err = AudioUnitSetProperty(isca->instance, kAudioOutputUnitProperty_CurrentDevice,
        kAudioUnitScope_Output, INPUT_ELEMENT, &dca->device_id, sizeof(AudioDeviceID))))
    {
        instream_destroy_ca(si, is);
        return SoundIoErrorOpeningDevice;
    }


    AudioStreamBasicDescription format = {0};
    format.mSampleRate = instream->sample_rate;
    format.mFormatID = kAudioFormatLinearPCM;
    format.mBytesPerPacket = instream->bytes_per_frame;
    format.mFramesPerPacket = 1;
    format.mBytesPerFrame = instream->bytes_per_frame;
    format.mChannelsPerFrame = instream->layout.channel_count;

    int err;
    if ((err = set_ca_desc(instream->format, &format))) {
        instream_destroy_ca(si, is);
        return err;
    }

    if ((os_err = AudioUnitSetProperty(isca->instance, kAudioUnitProperty_StreamFormat,
        kAudioUnitScope_Output, INPUT_ELEMENT, &format, sizeof(AudioStreamBasicDescription))))
    {
        instream_destroy_ca(si, is);
        return SoundIoErrorOpeningDevice;
    }

    AURenderCallbackStruct input_callback = {read_callback_ca, is};
    if ((os_err = AudioUnitSetProperty(isca->instance, kAudioOutputUnitProperty_SetInputCallback,
        kAudioUnitScope_Output, INPUT_ELEMENT, &input_callback, sizeof(AURenderCallbackStruct))))
    {
        instream_destroy_ca(si, is);
        return SoundIoErrorOpeningDevice;
    }


    prop_address.mSelector = kAudioDevicePropertyBufferFrameSize;
    prop_address.mScope = kAudioObjectPropertyScopeOutput;
    prop_address.mElement = INPUT_ELEMENT;
    UInt32 buffer_frame_size = instream->software_latency * instream->sample_rate;
    if ((os_err = AudioObjectSetPropertyData(dca->device_id, &prop_address,
        0, NULL, sizeof(UInt32), &buffer_frame_size)))
    {
        instream_destroy_ca(si, is);
        return SoundIoErrorOpeningDevice;
    }

    prop_address.mSelector = kAudioDeviceProcessorOverload;
    prop_address.mScope = kAudioObjectPropertyScopeGlobal;
    if ((os_err = AudioObjectAddPropertyListener(dca->device_id, &prop_address,
        on_instream_device_overload, is)))
    {
        instream_destroy_ca(si, is);
        return SoundIoErrorOpeningDevice;
    }

    isca->hardware_latency = dca->latency_frames / (double)instream->sample_rate;

    return 0;
}

static int instream_pause_ca(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is, bool pause) {
    struct SoundIoInStreamCoreAudio *isca = &is->backend_data.coreaudio;
    OSStatus os_err;
    if (pause) {
        if ((os_err = AudioOutputUnitStop(isca->instance))) {
            return SoundIoErrorStreaming;
        }
    } else {
        if ((os_err = AudioOutputUnitStart(isca->instance))) {
            return SoundIoErrorStreaming;
        }
    }

    return 0;
}

static int instream_start_ca(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    return instream_pause_ca(si, is, false);
}

static int instream_begin_read_ca(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is,
        struct SoundIoChannelArea **out_areas, int *frame_count)
{
    struct SoundIoInStreamCoreAudio *isca = &is->backend_data.coreaudio;

    if (*frame_count != isca->frames_left)
        return SoundIoErrorInvalid;

    *out_areas = isca->areas;

    return 0;
}

static int instream_end_read_ca(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamCoreAudio *isca = &is->backend_data.coreaudio;
    isca->frames_left = 0;
    return 0;
}

static int instream_get_latency_ca(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is,
        double *out_latency)
{
    struct SoundIoInStreamCoreAudio *isca = &is->backend_data.coreaudio;
    *out_latency = isca->hardware_latency;
    return 0;
}


int soundio_coreaudio_init(struct SoundIoPrivate *si) {
    struct SoundIoCoreAudio *sica = &si->backend_data.coreaudio;
    int err;

    SOUNDIO_ATOMIC_STORE(sica->have_devices_flag, false);
    SOUNDIO_ATOMIC_STORE(sica->device_scan_queued, true);
    SOUNDIO_ATOMIC_STORE(sica->service_restarted, false);
    SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(sica->abort_flag);

    sica->mutex = soundio_os_mutex_create();
    if (!sica->mutex) {
        destroy_ca(si);
        return SoundIoErrorNoMem;
    }

    sica->cond = soundio_os_cond_create();
    if (!sica->cond) {
        destroy_ca(si);
        return SoundIoErrorNoMem;
    }

    sica->have_devices_cond = soundio_os_cond_create();
    if (!sica->have_devices_cond) {
        destroy_ca(si);
        return SoundIoErrorNoMem;
    }

    sica->scan_devices_cond = soundio_os_cond_create();
    if (!sica->scan_devices_cond) {
        destroy_ca(si);
        return SoundIoErrorNoMem;
    }

    AudioObjectPropertyAddress prop_address = {
        kAudioHardwarePropertyDevices,
        kAudioObjectPropertyScopeGlobal,
        kAudioObjectPropertyElementMaster
    };
    if ((err = AudioObjectAddPropertyListener(kAudioObjectSystemObject, &prop_address,
        on_devices_changed, si)))
    {
        destroy_ca(si);
        return SoundIoErrorSystemResources;
    }

    prop_address.mSelector = kAudioHardwarePropertyServiceRestarted;
    if ((err = AudioObjectAddPropertyListener(kAudioObjectSystemObject, &prop_address,
        on_service_restarted, si)))
    {
        destroy_ca(si);
        return SoundIoErrorSystemResources;
    }

    if ((err = soundio_os_thread_create(device_thread_run, si, NULL, &sica->thread))) {
        destroy_ca(si);
        return err;
    }

    si->destroy = destroy_ca;
    si->flush_events = flush_events_ca;
    si->wait_events = wait_events_ca;
    si->wakeup = wakeup_ca;
    si->force_device_scan = force_device_scan_ca;

    si->outstream_open = outstream_open_ca;
    si->outstream_destroy = outstream_destroy_ca;
    si->outstream_start = outstream_start_ca;
    si->outstream_begin_write = outstream_begin_write_ca;
    si->outstream_end_write = outstream_end_write_ca;
    si->outstream_clear_buffer = outstream_clear_buffer_ca;
    si->outstream_pause = outstream_pause_ca;
    si->outstream_get_latency = outstream_get_latency_ca;

    si->instream_open = instream_open_ca;
    si->instream_destroy = instream_destroy_ca;
    si->instream_start = instream_start_ca;
    si->instream_begin_read = instream_begin_read_ca;
    si->instream_end_read = instream_end_read_ca;
    si->instream_pause = instream_pause_ca;
    si->instream_get_latency = instream_get_latency_ca;

    return 0;
}
#endif
#if SOUNDIO_HAVE_WASAPI
/*
 * Copyright (c) 2015 Andrew Kelley
 *
 * This file is part of libsoundio, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif

#define INITGUID
#define CINTERFACE
#define COBJMACROS
#define CONST_VTABLE
#include <initguid.h>
#include <audioclient.h>
#include <endpointvolume.h>
#include <mmdeviceapi.h>
#include <mmreg.h>
#include <functiondiscoverykeys_devpkey.h>

// (amalg) #include "wasapi.h"
// (amalg) #include "soundio_private.h"

#include <stdio.h>

// Some HRESULT values are not defined by the windows headers
#ifndef E_NOTFOUND
#define E_NOTFOUND 0x80070490
#endif //E_NOTFOUND

#ifdef __cplusplus
// In C++ mode, IsEqualGUID() takes its arguments by reference
#define IS_EQUAL_GUID(a, b) IsEqualGUID(*(a), *(b))
#define IS_EQUAL_IID(a, b) IsEqualIID((a), *(b))

// And some constants are passed by reference
#define IID_IAUDIOCLIENT                      (IID_IAudioClient)
#define IID_IMMENDPOINT                       (IID_IMMEndpoint)
#define IID_IAUDIOCLOCKADJUSTMENT             (IID_IAudioClockAdjustment)
#define IID_IAUDIOSESSIONCONTROL              (IID_IAudioSessionControl)
#define IID_IAUDIORENDERCLIENT                (IID_IAudioRenderClient)
#define IID_IMMDEVICEENUMERATOR               (IID_IMMDeviceEnumerator)
#define IID_IAUDIOCAPTURECLIENT               (IID_IAudioCaptureClient)
#define CLSID_MMDEVICEENUMERATOR              (CLSID_MMDeviceEnumerator)
#define PKEY_DEVICE_FRIENDLYNAME              (PKEY_Device_FriendlyName)
#define PKEY_AUDIOENGINE_DEVICEFORMAT         (PKEY_AudioEngine_DeviceFormat)

// And some GUID are never implemented (Ignoring the INITGUID define)
static const CLSID CLSID_MMDeviceEnumerator  = __uuidof(MMDeviceEnumerator);
static const IID   IID_IMMDeviceEnumerator   = {
    //MIDL_INTERFACE("A95664D2-9614-4F35-A746-DE8DB63617E6")
    0xa95664d2, 0x9614, 0x4f35, {0xa7, 0x46, 0xde, 0x8d, 0xb6, 0x36, 0x17, 0xe6}
};
static const IID   IID_IMMNotificationClient = {
    //MIDL_INTERFACE("7991EEC9-7E89-4D85-8390-6C703CEC60C0")
    0x7991eec9, 0x7e89, 0x4d85, {0x83, 0x90, 0x6c, 0x70, 0x3c, 0xec, 0x60, 0xc0}
};
static const IID   IID_IAudioClient = {
    //MIDL_INTERFACE("1CB9AD4C-DBFA-4c32-B178-C2F568A703B2")
    0x1cb9ad4c, 0xdbfa, 0x4c32, {0xb1, 0x78, 0xc2, 0xf5, 0x68, 0xa7, 0x03, 0xb2}
};
static const IID   IID_IAudioRenderClient    = {
    //MIDL_INTERFACE("F294ACFC-3146-4483-A7BF-ADDCA7C260E2")
    0xf294acfc, 0x3146, 0x4483, {0xa7, 0xbf, 0xad, 0xdc, 0xa7, 0xc2, 0x60, 0xe2}
};
static const IID   IID_IAudioSessionControl  = {
    //MIDL_INTERFACE("F4B1A599-7266-4319-A8CA-E70ACB11E8CD")
    0xf4b1a599, 0x7266, 0x4319, {0xa8, 0xca, 0xe7, 0x0a, 0xcb, 0x11, 0xe8, 0xcd}
};
static const IID   IID_IAudioSessionEvents   = {
    //MIDL_INTERFACE("24918ACC-64B3-37C1-8CA9-74A66E9957A8")
    0x24918acc, 0x64b3, 0x37c1, {0x8c, 0xa9, 0x74, 0xa6, 0x6e, 0x99, 0x57, 0xa8}
};
static const IID IID_IMMEndpoint = {
    //MIDL_INTERFACE("1BE09788-6894-4089-8586-9A2A6C265AC5")
    0x1be09788, 0x6894, 0x4089, {0x85, 0x86, 0x9a, 0x2a, 0x6c, 0x26, 0x5a, 0xc5}
};
static const IID IID_IAudioClockAdjustment = {
    //MIDL_INTERFACE("f6e4c0a0-46d9-4fb8-be21-57a3ef2b626c")
    0xf6e4c0a0, 0x46d9, 0x4fb8, {0xbe, 0x21, 0x57, 0xa3, 0xef, 0x2b, 0x62, 0x6c}
};
static const IID IID_IAudioCaptureClient = {
    //MIDL_INTERFACE("C8ADBD64-E71E-48a0-A4DE-185C395CD317")
    0xc8adbd64, 0xe71e, 0x48a0, {0xa4, 0xde, 0x18, 0x5c, 0x39, 0x5c, 0xd3, 0x17}
};

#else
#define IS_EQUAL_GUID(a, b) IsEqualGUID((a), (b))
#define IS_EQUAL_IID(a, b) IsEqualIID((a), (b))

#define IID_IAUDIOCLIENT (&IID_IAudioClient)
#define IID_IMMENDPOINT (&IID_IMMEndpoint)
#define PKEY_DEVICE_FRIENDLYNAME (&PKEY_Device_FriendlyName)
#define PKEY_AUDIOENGINE_DEVICEFORMAT (&PKEY_AudioEngine_DeviceFormat)
#define CLSID_MMDEVICEENUMERATOR (&CLSID_MMDeviceEnumerator)
#define IID_IAUDIOCLOCKADJUSTMENT (&IID_IAudioClockAdjustment)
#define IID_IAUDIOSESSIONCONTROL (&IID_IAudioSessionControl)
#define IID_IAUDIORENDERCLIENT (&IID_IAudioRenderClient)
#define IID_IMMDEVICEENUMERATOR (&IID_IMMDeviceEnumerator)
#define IID_IAUDIOCAPTURECLIENT (&IID_IAudioCaptureClient)
#endif

// Attempting to use the Windows-supplied versions of these constants resulted
// in `undefined reference` linker errors.
const static GUID SOUNDIO_KSDATAFORMAT_SUBTYPE_IEEE_FLOAT = {
    0x00000003,0x0000,0x0010, {0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71}};

const static GUID SOUNDIO_KSDATAFORMAT_SUBTYPE_PCM = {
    0x00000001,0x0000,0x0010, {0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71}};

// Adding more common sample rates helps the heuristics; feel free to do that.
static int test_sample_rates[] = {
    8000,
    11025,
    16000,
    22050,
    32000,
    37800,
    44056,
    44100,
    47250,
    48000,
    50000,
    50400,
    88200,
    96000,
    176400,
    192000,
    352800,
    2822400,
    5644800,
};

// If you modify this list, also modify `to_wave_format_format` appropriately.
static enum SoundIoFormat test_formats[] = {
    SoundIoFormatU8,
    SoundIoFormatS16LE,
    SoundIoFormatS24LE,
    SoundIoFormatS32LE,
    SoundIoFormatFloat32LE,
    SoundIoFormatFloat64LE,
};

// If you modify this list, also modify `to_wave_format_layout` appropriately.
static enum SoundIoChannelLayoutId test_layouts[] = {
    SoundIoChannelLayoutIdMono,
    SoundIoChannelLayoutIdStereo,
    SoundIoChannelLayoutIdQuad,
    SoundIoChannelLayoutId4Point0,
    SoundIoChannelLayoutId5Point1,
    SoundIoChannelLayoutId7Point1,
    SoundIoChannelLayoutId5Point1Back,
};

/*
// useful for debugging but no point in compiling into binary
static const char *hresult_to_str(HRESULT hr) {
    switch (hr) {
        default: return "(unknown)";
        case AUDCLNT_E_NOT_INITIALIZED: return "AUDCLNT_E_NOT_INITIALIZED";
        case AUDCLNT_E_ALREADY_INITIALIZED: return "AUDCLNT_E_ALREADY_INITIALIZED";
        case AUDCLNT_E_WRONG_ENDPOINT_TYPE: return "AUDCLNT_E_WRONG_ENDPOINT_TYPE";
        case AUDCLNT_E_DEVICE_INVALIDATED: return "AUDCLNT_E_DEVICE_INVALIDATED";
        case AUDCLNT_E_NOT_STOPPED: return "AUDCLNT_E_NOT_STOPPED";
        case AUDCLNT_E_BUFFER_TOO_LARGE: return "AUDCLNT_E_BUFFER_TOO_LARGE";
        case AUDCLNT_E_OUT_OF_ORDER: return "AUDCLNT_E_OUT_OF_ORDER";
        case AUDCLNT_E_UNSUPPORTED_FORMAT: return "AUDCLNT_E_UNSUPPORTED_FORMAT";
        case AUDCLNT_E_INVALID_SIZE: return "AUDCLNT_E_INVALID_SIZE";
        case AUDCLNT_E_DEVICE_IN_USE: return "AUDCLNT_E_DEVICE_IN_USE";
        case AUDCLNT_E_BUFFER_OPERATION_PENDING: return "AUDCLNT_E_BUFFER_OPERATION_PENDING";
        case AUDCLNT_E_THREAD_NOT_REGISTERED: return "AUDCLNT_E_THREAD_NOT_REGISTERED";
        case AUDCLNT_E_EXCLUSIVE_MODE_NOT_ALLOWED: return "AUDCLNT_E_EXCLUSIVE_MODE_NOT_ALLOWED";
        case AUDCLNT_E_ENDPOINT_CREATE_FAILED: return "AUDCLNT_E_ENDPOINT_CREATE_FAILED";
        case AUDCLNT_E_SERVICE_NOT_RUNNING: return "AUDCLNT_E_SERVICE_NOT_RUNNING";
        case AUDCLNT_E_EVENTHANDLE_NOT_EXPECTED: return "AUDCLNT_E_EVENTHANDLE_NOT_EXPECTED";
        case AUDCLNT_E_EXCLUSIVE_MODE_ONLY: return "AUDCLNT_E_EXCLUSIVE_MODE_ONLY";
        case AUDCLNT_E_BUFDURATION_PERIOD_NOT_EQUAL: return "AUDCLNT_E_BUFDURATION_PERIOD_NOT_EQUAL";
        case AUDCLNT_E_EVENTHANDLE_NOT_SET: return "AUDCLNT_E_EVENTHANDLE_NOT_SET";
        case AUDCLNT_E_INCORRECT_BUFFER_SIZE: return "AUDCLNT_E_INCORRECT_BUFFER_SIZE";
        case AUDCLNT_E_BUFFER_SIZE_ERROR: return "AUDCLNT_E_BUFFER_SIZE_ERROR";
        case AUDCLNT_E_CPUUSAGE_EXCEEDED: return "AUDCLNT_E_CPUUSAGE_EXCEEDED";
        case AUDCLNT_E_BUFFER_ERROR: return "AUDCLNT_E_BUFFER_ERROR";
        case AUDCLNT_E_BUFFER_SIZE_NOT_ALIGNED: return "AUDCLNT_E_BUFFER_SIZE_NOT_ALIGNED";
        case AUDCLNT_E_INVALID_DEVICE_PERIOD: return "AUDCLNT_E_INVALID_DEVICE_PERIOD";
        case AUDCLNT_E_INVALID_STREAM_FLAG: return "AUDCLNT_E_INVALID_STREAM_FLAG";
        case AUDCLNT_E_ENDPOINT_OFFLOAD_NOT_CAPABLE: return "AUDCLNT_E_ENDPOINT_OFFLOAD_NOT_CAPABLE";
        case AUDCLNT_E_OUT_OF_OFFLOAD_RESOURCES: return "AUDCLNT_E_OUT_OF_OFFLOAD_RESOURCES";
        case AUDCLNT_E_OFFLOAD_MODE_ONLY: return "AUDCLNT_E_OFFLOAD_MODE_ONLY";
        case AUDCLNT_E_NONOFFLOAD_MODE_ONLY: return "AUDCLNT_E_NONOFFLOAD_MODE_ONLY";
        case AUDCLNT_E_RESOURCES_INVALIDATED: return "AUDCLNT_E_RESOURCES_INVALIDATED";
        case AUDCLNT_S_BUFFER_EMPTY: return "AUDCLNT_S_BUFFER_EMPTY";
        case AUDCLNT_S_THREAD_ALREADY_REGISTERED: return "AUDCLNT_S_THREAD_ALREADY_REGISTERED";
        case AUDCLNT_S_POSITION_STALLED: return "AUDCLNT_S_POSITION_STALLED";

        case E_POINTER: return "E_POINTER";
        case E_INVALIDARG: return "E_INVALIDARG";
        case E_OUTOFMEMORY: return "E_OUTOFMEMORY";
    }
}
*/

// converts a windows wide string to a UTF-8 encoded char *
// Possible errors:
//  * SoundIoErrorNoMem
//  * SoundIoErrorEncodingString
static int from_lpwstr(LPWSTR lpwstr, char **out_str, int *out_str_len) {
    DWORD flags = 0;
    int buf_size = WideCharToMultiByte(CP_UTF8, flags, lpwstr, -1, NULL, 0, NULL, NULL);

    if (buf_size == 0)
        return SoundIoErrorEncodingString;

    char *buf = ALLOCATE(char, buf_size);
    if (!buf)
        return SoundIoErrorNoMem;

    if (WideCharToMultiByte(CP_UTF8, flags, lpwstr, -1, buf, buf_size, NULL, NULL) != buf_size) {
        free(buf);
        return SoundIoErrorEncodingString;
    }

    *out_str = buf;
    *out_str_len = buf_size - 1;

    return 0;
}

static int to_lpwstr(const char *str, int str_len, LPWSTR *out_lpwstr) {
    DWORD flags = 0;
    int w_len = MultiByteToWideChar(CP_UTF8, flags, str, str_len, NULL, 0);
    if (w_len <= 0)
        return SoundIoErrorEncodingString;

    LPWSTR buf = ALLOCATE(wchar_t, w_len + 1);
    if (!buf)
        return SoundIoErrorNoMem;

    if (MultiByteToWideChar(CP_UTF8, flags, str, str_len, buf, w_len) != w_len) {
        free(buf);
        return SoundIoErrorEncodingString;
    }

    *out_lpwstr = buf;
    return 0;
}

static void from_channel_mask_layout(UINT channel_mask, struct SoundIoChannelLayout *layout) {
    layout->channel_count = 0;
    if (channel_mask & SPEAKER_FRONT_LEFT)
        layout->channels[layout->channel_count++] = SoundIoChannelIdFrontLeft;
    if (channel_mask & SPEAKER_FRONT_RIGHT)
        layout->channels[layout->channel_count++] = SoundIoChannelIdFrontRight;
    if (channel_mask & SPEAKER_FRONT_CENTER)
        layout->channels[layout->channel_count++] = SoundIoChannelIdFrontCenter;
    if (channel_mask & SPEAKER_LOW_FREQUENCY)
        layout->channels[layout->channel_count++] = SoundIoChannelIdLfe;
    if (channel_mask & SPEAKER_BACK_LEFT)
        layout->channels[layout->channel_count++] = SoundIoChannelIdBackLeft;
    if (channel_mask & SPEAKER_BACK_RIGHT)
        layout->channels[layout->channel_count++] = SoundIoChannelIdBackRight;
    if (channel_mask & SPEAKER_FRONT_LEFT_OF_CENTER)
        layout->channels[layout->channel_count++] = SoundIoChannelIdFrontLeftCenter;
    if (channel_mask & SPEAKER_FRONT_RIGHT_OF_CENTER)
        layout->channels[layout->channel_count++] = SoundIoChannelIdFrontRightCenter;
    if (channel_mask & SPEAKER_BACK_CENTER)
        layout->channels[layout->channel_count++] = SoundIoChannelIdBackCenter;
    if (channel_mask & SPEAKER_SIDE_LEFT)
        layout->channels[layout->channel_count++] = SoundIoChannelIdSideLeft;
    if (channel_mask & SPEAKER_SIDE_RIGHT)
        layout->channels[layout->channel_count++] = SoundIoChannelIdSideRight;
    if (channel_mask & SPEAKER_TOP_CENTER)
        layout->channels[layout->channel_count++] = SoundIoChannelIdTopCenter;
    if (channel_mask & SPEAKER_TOP_FRONT_LEFT)
        layout->channels[layout->channel_count++] = SoundIoChannelIdTopFrontLeft;
    if (channel_mask & SPEAKER_TOP_FRONT_CENTER)
        layout->channels[layout->channel_count++] = SoundIoChannelIdTopFrontCenter;
    if (channel_mask & SPEAKER_TOP_FRONT_RIGHT)
        layout->channels[layout->channel_count++] = SoundIoChannelIdTopFrontRight;
    if (channel_mask & SPEAKER_TOP_BACK_LEFT)
        layout->channels[layout->channel_count++] = SoundIoChannelIdTopBackLeft;
    if (channel_mask & SPEAKER_TOP_BACK_CENTER)
        layout->channels[layout->channel_count++] = SoundIoChannelIdTopBackCenter;
    if (channel_mask & SPEAKER_TOP_BACK_RIGHT)
        layout->channels[layout->channel_count++] = SoundIoChannelIdTopBackRight;

    soundio_channel_layout_detect_builtin(layout);
}

static void from_wave_format_layout(WAVEFORMATEXTENSIBLE *wave_format, struct SoundIoChannelLayout *layout) {
    assert(wave_format->Format.wFormatTag == WAVE_FORMAT_EXTENSIBLE);
    layout->channel_count = 0;
    from_channel_mask_layout(wave_format->dwChannelMask, layout);
}

static enum SoundIoFormat from_wave_format_format(WAVEFORMATEXTENSIBLE *wave_format) {
    assert(wave_format->Format.wFormatTag == WAVE_FORMAT_EXTENSIBLE);
    bool is_pcm = IS_EQUAL_GUID(&wave_format->SubFormat, &SOUNDIO_KSDATAFORMAT_SUBTYPE_PCM);
    bool is_float = IS_EQUAL_GUID(&wave_format->SubFormat, &SOUNDIO_KSDATAFORMAT_SUBTYPE_IEEE_FLOAT);

    if (wave_format->Samples.wValidBitsPerSample == wave_format->Format.wBitsPerSample) {
        if (wave_format->Format.wBitsPerSample == 8) {
            if (is_pcm)
                return SoundIoFormatU8;
        } else if (wave_format->Format.wBitsPerSample == 16) {
            if (is_pcm)
                return SoundIoFormatS16LE;
        } else if (wave_format->Format.wBitsPerSample == 32) {
            if (is_pcm)
                return SoundIoFormatS32LE;
            else if (is_float)
                return SoundIoFormatFloat32LE;
        } else if (wave_format->Format.wBitsPerSample == 64) {
            if (is_float)
                return SoundIoFormatFloat64LE;
        }
    } else if (wave_format->Format.wBitsPerSample == 32 &&
            wave_format->Samples.wValidBitsPerSample == 24)
    {
        return SoundIoFormatS24LE;
    }

    return SoundIoFormatInvalid;
}

// only needs to support the layouts in test_layouts
static void to_wave_format_layout(const struct SoundIoChannelLayout *layout, WAVEFORMATEXTENSIBLE *wave_format) {
    wave_format->dwChannelMask = 0;
    wave_format->Format.nChannels = layout->channel_count;
    for (int i = 0; i < layout->channel_count; i += 1) {
        enum SoundIoChannelId channel_id = layout->channels[i];
        switch (channel_id) {
            case SoundIoChannelIdFrontLeft:
                wave_format->dwChannelMask |= SPEAKER_FRONT_LEFT;
                break;
            case SoundIoChannelIdFrontRight:
                wave_format->dwChannelMask |= SPEAKER_FRONT_RIGHT;
                break;
            case SoundIoChannelIdFrontCenter:
                wave_format->dwChannelMask |= SPEAKER_FRONT_CENTER;
                break;
            case SoundIoChannelIdLfe:
                wave_format->dwChannelMask |= SPEAKER_LOW_FREQUENCY;
                break;
            case SoundIoChannelIdBackLeft:
                wave_format->dwChannelMask |= SPEAKER_BACK_LEFT;
                break;
            case SoundIoChannelIdBackRight:
                wave_format->dwChannelMask |= SPEAKER_BACK_RIGHT;
                break;
            case SoundIoChannelIdFrontLeftCenter:
                wave_format->dwChannelMask |= SPEAKER_FRONT_LEFT_OF_CENTER;
                break;
            case SoundIoChannelIdFrontRightCenter:
                wave_format->dwChannelMask |= SPEAKER_FRONT_RIGHT_OF_CENTER;
                break;
            case SoundIoChannelIdBackCenter:
                wave_format->dwChannelMask |= SPEAKER_BACK_CENTER;
                break;
            case SoundIoChannelIdSideLeft:
                wave_format->dwChannelMask |= SPEAKER_SIDE_LEFT;
                break;
            case SoundIoChannelIdSideRight:
                wave_format->dwChannelMask |= SPEAKER_SIDE_RIGHT;
                break;
            case SoundIoChannelIdTopCenter:
                wave_format->dwChannelMask |= SPEAKER_TOP_CENTER;
                break;
            case SoundIoChannelIdTopFrontLeft:
                wave_format->dwChannelMask |= SPEAKER_TOP_FRONT_LEFT;
                break;
            case SoundIoChannelIdTopFrontCenter:
                wave_format->dwChannelMask |= SPEAKER_TOP_FRONT_CENTER;
                break;
            case SoundIoChannelIdTopFrontRight:
                wave_format->dwChannelMask |= SPEAKER_TOP_FRONT_RIGHT;
                break;
            case SoundIoChannelIdTopBackLeft:
                wave_format->dwChannelMask |= SPEAKER_TOP_BACK_LEFT;
                break;
            case SoundIoChannelIdTopBackCenter:
                wave_format->dwChannelMask |= SPEAKER_TOP_BACK_CENTER;
                break;
            case SoundIoChannelIdTopBackRight:
                wave_format->dwChannelMask |= SPEAKER_TOP_BACK_RIGHT;
                break;
            default:
                soundio_panic("to_wave_format_layout: unsupported channel id");
        }
    }
}

// only needs to support the formats in test_formats
static void to_wave_format_format(enum SoundIoFormat format, WAVEFORMATEXTENSIBLE *wave_format) {
    switch (format) {
    case SoundIoFormatU8:
        wave_format->SubFormat = SOUNDIO_KSDATAFORMAT_SUBTYPE_PCM;
        wave_format->Format.wBitsPerSample = 8;
        wave_format->Samples.wValidBitsPerSample = 8;
        break;
    case SoundIoFormatS16LE:
        wave_format->SubFormat = SOUNDIO_KSDATAFORMAT_SUBTYPE_PCM;
        wave_format->Format.wBitsPerSample = 16;
        wave_format->Samples.wValidBitsPerSample = 16;
        break;
    case SoundIoFormatS24LE:
        wave_format->SubFormat = SOUNDIO_KSDATAFORMAT_SUBTYPE_PCM;
        wave_format->Format.wBitsPerSample = 32;
        wave_format->Samples.wValidBitsPerSample = 24;
        break;
    case SoundIoFormatS32LE:
        wave_format->SubFormat = SOUNDIO_KSDATAFORMAT_SUBTYPE_PCM;
        wave_format->Format.wBitsPerSample = 32;
        wave_format->Samples.wValidBitsPerSample = 32;
        break;
    case SoundIoFormatFloat32LE:
        wave_format->SubFormat = SOUNDIO_KSDATAFORMAT_SUBTYPE_IEEE_FLOAT;
        wave_format->Format.wBitsPerSample = 32;
        wave_format->Samples.wValidBitsPerSample = 32;
        break;
    case SoundIoFormatFloat64LE:
        wave_format->SubFormat = SOUNDIO_KSDATAFORMAT_SUBTYPE_IEEE_FLOAT;
        wave_format->Format.wBitsPerSample = 64;
        wave_format->Samples.wValidBitsPerSample = 64;
        break;
    default:
        soundio_panic("to_wave_format_format: unsupported format");
    }
}

static void complete_wave_format_data(WAVEFORMATEXTENSIBLE *wave_format) {
    wave_format->Format.nBlockAlign = (wave_format->Format.wBitsPerSample * wave_format->Format.nChannels) / 8;
    wave_format->Format.nAvgBytesPerSec = wave_format->Format.nSamplesPerSec * wave_format->Format.nBlockAlign;
}

static enum SoundIoDeviceAim data_flow_to_aim(EDataFlow data_flow) {
    return (data_flow == eRender) ? SoundIoDeviceAimOutput : SoundIoDeviceAimInput;
}


static double from_reference_time(REFERENCE_TIME rt) {
    return ((double)rt) / 10000000.0;
}

static REFERENCE_TIME to_reference_time(double seconds) {
    return (REFERENCE_TIME)(seconds * 10000000.0 + 0.5);
}

static void destruct_device(struct SoundIoDevicePrivate *dev) {
    struct SoundIoDeviceWasapi *dw = &dev->backend_data.wasapi;
    if (dw->mm_device)
        IMMDevice_Release(dw->mm_device);
}

struct RefreshDevices {
    IMMDeviceCollection *collection;
    IMMDevice *mm_device;
    IMMDevice *default_render_device;
    IMMDevice *default_capture_device;
    IMMEndpoint *endpoint;
    IPropertyStore *prop_store;
    IAudioClient *audio_client;
    LPWSTR lpwstr;
    PROPVARIANT prop_variant_value;
    WAVEFORMATEXTENSIBLE *wave_format;
    bool prop_variant_value_inited;
    struct SoundIoDevicesInfo *devices_info;
    struct SoundIoDevice *device_shared;
    struct SoundIoDevice *device_raw;
    char *default_render_id;
    int default_render_id_len;
    char *default_capture_id;
    int default_capture_id_len;
};

static void deinit_refresh_devices(struct RefreshDevices *rd) {
    soundio_destroy_devices_info(rd->devices_info);
    soundio_device_unref(rd->device_shared);
    soundio_device_unref(rd->device_raw);
    if (rd->mm_device)
        IMMDevice_Release(rd->mm_device);
    if (rd->default_render_device)
        IMMDevice_Release(rd->default_render_device);
    if (rd->default_capture_device)
        IMMDevice_Release(rd->default_capture_device);
    if (rd->collection)
        IMMDeviceCollection_Release(rd->collection);
    if (rd->lpwstr)
        CoTaskMemFree(rd->lpwstr);
    if (rd->endpoint)
        IMMEndpoint_Release(rd->endpoint);
    if (rd->prop_store)
        IPropertyStore_Release(rd->prop_store);
    if (rd->prop_variant_value_inited)
        PropVariantClear(&rd->prop_variant_value);
    if (rd->wave_format)
        CoTaskMemFree(rd->wave_format);
    if (rd->audio_client)
        IUnknown_Release(rd->audio_client);
}

static int detect_valid_layouts(struct RefreshDevices *rd, WAVEFORMATEXTENSIBLE *wave_format,
        struct SoundIoDevicePrivate *dev, AUDCLNT_SHAREMODE share_mode)
{
    struct SoundIoDevice *device = &dev->pub;
    HRESULT hr;

    device->layout_count = 0;
    device->layouts = ALLOCATE(struct SoundIoChannelLayout, ARRAY_LENGTH(test_layouts));
    if (!device->layouts)
        return SoundIoErrorNoMem;

    WAVEFORMATEX *closest_match = NULL;
    WAVEFORMATEXTENSIBLE orig_wave_format = *wave_format;

    for (int i = 0; i < ARRAY_LENGTH(test_formats); i += 1) {
        enum SoundIoChannelLayoutId test_layout_id = test_layouts[i];
        const struct SoundIoChannelLayout *test_layout = soundio_channel_layout_get_builtin(test_layout_id);
        to_wave_format_layout(test_layout, wave_format);
        complete_wave_format_data(wave_format);

        hr = IAudioClient_IsFormatSupported(rd->audio_client, share_mode,
                (WAVEFORMATEX*)wave_format, &closest_match);
        if (closest_match) {
            CoTaskMemFree(closest_match);
            closest_match = NULL;
        }
        if (hr == S_OK) {
            device->layouts[device->layout_count++] = *test_layout;
        } else if (hr == AUDCLNT_E_UNSUPPORTED_FORMAT || hr == S_FALSE || hr == E_INVALIDARG) {
            continue;
        } else {
            *wave_format = orig_wave_format;
            return SoundIoErrorOpeningDevice;
        }
    }

    *wave_format = orig_wave_format;
    return 0;
}

static int detect_valid_formats(struct RefreshDevices *rd, WAVEFORMATEXTENSIBLE *wave_format,
        struct SoundIoDevicePrivate *dev, AUDCLNT_SHAREMODE share_mode)
{
    struct SoundIoDevice *device = &dev->pub;
    HRESULT hr;

    device->format_count = 0;
    device->formats = ALLOCATE(enum SoundIoFormat, ARRAY_LENGTH(test_formats));
    if (!device->formats)
        return SoundIoErrorNoMem;

    WAVEFORMATEX *closest_match = NULL;
    WAVEFORMATEXTENSIBLE orig_wave_format = *wave_format;

    for (int i = 0; i < ARRAY_LENGTH(test_formats); i += 1) {
        enum SoundIoFormat test_format = test_formats[i];
        to_wave_format_format(test_format, wave_format);
        complete_wave_format_data(wave_format);

        hr = IAudioClient_IsFormatSupported(rd->audio_client, share_mode,
                (WAVEFORMATEX*)wave_format, &closest_match);
        if (closest_match) {
            CoTaskMemFree(closest_match);
            closest_match = NULL;
        }
        if (hr == S_OK) {
            device->formats[device->format_count++] = test_format;
        } else if (hr == AUDCLNT_E_UNSUPPORTED_FORMAT || hr == S_FALSE || hr == E_INVALIDARG) {
            continue;
        } else {
            *wave_format = orig_wave_format;
            return SoundIoErrorOpeningDevice;
        }
    }

    *wave_format = orig_wave_format;
    return 0;
}

static int add_sample_rate(struct SoundIoListSampleRateRange *sample_rates, int *current_min, int the_max) {
    int err;
    if ((err = SoundIoListSampleRateRange_add_one(sample_rates)))
        return err;

    struct SoundIoSampleRateRange *last_range = SoundIoListSampleRateRange_last_ptr(sample_rates);
    last_range->min = *current_min;
    last_range->max = the_max;
    return 0;
}

static int do_sample_rate_test(struct RefreshDevices *rd, struct SoundIoDevicePrivate *dev, WAVEFORMATEXTENSIBLE *wave_format,
        int test_sample_rate, AUDCLNT_SHAREMODE share_mode, int *current_min, int *last_success_rate)
{
    WAVEFORMATEX *closest_match = NULL;
    int err;

    wave_format->Format.nSamplesPerSec = test_sample_rate;
    HRESULT hr = IAudioClient_IsFormatSupported(rd->audio_client, share_mode,
            (WAVEFORMATEX*)wave_format, &closest_match);
    if (closest_match) {
        CoTaskMemFree(closest_match);
        closest_match = NULL;
    }
    if (hr == S_OK) {
        if (*current_min == -1) {
            *current_min = test_sample_rate;
        }
        *last_success_rate = test_sample_rate;
    } else if (hr == AUDCLNT_E_UNSUPPORTED_FORMAT || hr == S_FALSE || hr == E_INVALIDARG) {
        if (*current_min != -1) {
            if ((err = add_sample_rate(&dev->sample_rates, current_min, *last_success_rate)))
                return err;
            *current_min = -1;
        }
    } else {
        return SoundIoErrorOpeningDevice;
    }

    return 0;
}

static int detect_valid_sample_rates(struct RefreshDevices *rd, WAVEFORMATEXTENSIBLE *wave_format,
        struct SoundIoDevicePrivate *dev, AUDCLNT_SHAREMODE share_mode)
{
    int err;

    DWORD orig_sample_rate = wave_format->Format.nSamplesPerSec;

    assert(dev->sample_rates.length == 0);

    int current_min = -1;
    int last_success_rate = -1;
    for (int i = 0; i < ARRAY_LENGTH(test_sample_rates); i += 1) {
        for (int offset = -1; offset <= 1; offset += 1) {
            int test_sample_rate = test_sample_rates[i] + offset;
            if ((err = do_sample_rate_test(rd, dev, wave_format, test_sample_rate, share_mode,
                            &current_min, &last_success_rate)))
            {
                wave_format->Format.nSamplesPerSec = orig_sample_rate;
                return err;
            }
        }
    }

    if (current_min != -1) {
        if ((err = add_sample_rate(&dev->sample_rates, &current_min, last_success_rate))) {
            wave_format->Format.nSamplesPerSec = orig_sample_rate;
            return err;
        }
    }

    struct SoundIoDevice *device = &dev->pub;

    device->sample_rate_count = dev->sample_rates.length;
    device->sample_rates = dev->sample_rates.items;

    wave_format->Format.nSamplesPerSec = orig_sample_rate;
    return 0;
}


static int refresh_devices(struct SoundIoPrivate *si) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoWasapi *siw = &si->backend_data.wasapi;
    struct RefreshDevices rd = {0};
    int err;
    HRESULT hr;

    if (FAILED(hr = IMMDeviceEnumerator_GetDefaultAudioEndpoint(siw->device_enumerator, eRender,
                    eMultimedia, &rd.default_render_device)))
    {
        if(hr != E_NOTFOUND) {
            deinit_refresh_devices(&rd);
            if(hr == E_OUTOFMEMORY) {
                return SoundIoErrorNoMem;
            }
            return SoundIoErrorOpeningDevice;
        }
    }
    if(rd.default_render_device) {
        if (rd.lpwstr) {
            CoTaskMemFree(rd.lpwstr);
            rd.lpwstr = NULL;
        }
        if (FAILED(hr = IMMDevice_GetId(rd.default_render_device, &rd.lpwstr))) {
            deinit_refresh_devices(&rd);
            // MSDN states the IMMDevice_GetId can fail if the device is NULL, or if we're out of memory
            // We know the device point isn't NULL so we're necessarily out of memory
            return SoundIoErrorNoMem;
        }
        if ((err = from_lpwstr(rd.lpwstr, &rd.default_render_id, &rd.default_render_id_len))) {
            deinit_refresh_devices(&rd);
            return err;
        }
    }


    if (FAILED(hr = IMMDeviceEnumerator_GetDefaultAudioEndpoint(siw->device_enumerator, eCapture,
                    eMultimedia, &rd.default_capture_device)))
    {
        if(hr != E_NOTFOUND) {
            deinit_refresh_devices(&rd);
            if(hr == E_OUTOFMEMORY) {
                return SoundIoErrorNoMem;
            }
            return SoundIoErrorOpeningDevice;
        }
    }
    if(rd.default_capture_device) {
        if (rd.lpwstr) {
            CoTaskMemFree(rd.lpwstr);
            rd.lpwstr = NULL;
        }
        if (FAILED(hr = IMMDevice_GetId(rd.default_capture_device, &rd.lpwstr))) {
            deinit_refresh_devices(&rd);
            if(hr == E_OUTOFMEMORY) {
                return SoundIoErrorNoMem;
            }
            return SoundIoErrorOpeningDevice;
        }
        if ((err = from_lpwstr(rd.lpwstr, &rd.default_capture_id, &rd.default_capture_id_len))) {
            deinit_refresh_devices(&rd);
            return err;
        }
    }


    if (FAILED(hr = IMMDeviceEnumerator_EnumAudioEndpoints(siw->device_enumerator,
                    eAll, DEVICE_STATE_ACTIVE, &rd.collection)))
    {
        deinit_refresh_devices(&rd);
        if(hr == E_OUTOFMEMORY) {
            return SoundIoErrorNoMem;
        }
        return SoundIoErrorOpeningDevice;
    }

    UINT unsigned_count;
    if (FAILED(hr = IMMDeviceCollection_GetCount(rd.collection, &unsigned_count))) {
        // In theory this shouldn't happen since the only documented failure case is that
        // rd.collection is NULL, but then EnumAudioEndpoints should have failed.
        deinit_refresh_devices(&rd);
        return SoundIoErrorOpeningDevice;
    }

    if (unsigned_count > (UINT)INT_MAX) {
        deinit_refresh_devices(&rd);
        return SoundIoErrorIncompatibleDevice;
    }

    int device_count = unsigned_count;

    if (!(rd.devices_info = ALLOCATE(struct SoundIoDevicesInfo, 1))) {
        deinit_refresh_devices(&rd);
        return SoundIoErrorNoMem;
    }
    rd.devices_info->default_input_index = -1;
    rd.devices_info->default_output_index = -1;

    for (int device_i = 0; device_i < device_count; device_i += 1) {
        if (rd.mm_device) {
            IMMDevice_Release(rd.mm_device);
            rd.mm_device = NULL;
        }
        if (FAILED(hr = IMMDeviceCollection_Item(rd.collection, device_i, &rd.mm_device))) {
            continue;
        }
        if (rd.lpwstr) {
            CoTaskMemFree(rd.lpwstr);
            rd.lpwstr = NULL;
        }
        if (FAILED(hr = IMMDevice_GetId(rd.mm_device, &rd.lpwstr))) {
            continue;
        }



        struct SoundIoDevicePrivate *dev_shared = ALLOCATE(struct SoundIoDevicePrivate, 1);
        if (!dev_shared) {
            deinit_refresh_devices(&rd);
            return SoundIoErrorNoMem;
        }
        struct SoundIoDeviceWasapi *dev_w_shared = &dev_shared->backend_data.wasapi;
        dev_shared->destruct = destruct_device;
        assert(!rd.device_shared);
        rd.device_shared = &dev_shared->pub;
        rd.device_shared->ref_count = 1;
        rd.device_shared->soundio = soundio;
        rd.device_shared->is_raw = false;
        rd.device_shared->software_latency_max = 2.0;

        struct SoundIoDevicePrivate *dev_raw = ALLOCATE(struct SoundIoDevicePrivate, 1);
        if (!dev_raw) {
            deinit_refresh_devices(&rd);
            return SoundIoErrorNoMem;
        }
        struct SoundIoDeviceWasapi *dev_w_raw = &dev_raw->backend_data.wasapi;
        dev_raw->destruct = destruct_device;
        assert(!rd.device_raw);
        rd.device_raw = &dev_raw->pub;
        rd.device_raw->ref_count = 1;
        rd.device_raw->soundio = soundio;
        rd.device_raw->is_raw = true;
        rd.device_raw->software_latency_max = 0.5;

        int device_id_len;
        if ((err = from_lpwstr(rd.lpwstr, &rd.device_shared->id, &device_id_len))) {
            deinit_refresh_devices(&rd);
            return err;
        }

        rd.device_raw->id = soundio_str_dupe(rd.device_shared->id, device_id_len);
        if (!rd.device_raw->id) {
            deinit_refresh_devices(&rd);
            return SoundIoErrorNoMem;
        }

        if (rd.endpoint) {
            IMMEndpoint_Release(rd.endpoint);
            rd.endpoint = NULL;
        }
        if (FAILED(hr = IMMDevice_QueryInterface(rd.mm_device, IID_IMMENDPOINT, (void**)&rd.endpoint))) {
            rd.device_shared->probe_error = SoundIoErrorOpeningDevice;
            rd.device_raw->probe_error = SoundIoErrorOpeningDevice;
            rd.device_shared = NULL;
            rd.device_raw = NULL;
            continue;
        }

        EDataFlow data_flow;
        if (FAILED(hr = IMMEndpoint_GetDataFlow(rd.endpoint, &data_flow))) {
            rd.device_shared->probe_error = SoundIoErrorOpeningDevice;
            rd.device_raw->probe_error = SoundIoErrorOpeningDevice;
            rd.device_shared = NULL;
            rd.device_raw = NULL;
            continue;
        }

        rd.device_shared->aim = data_flow_to_aim(data_flow);
        rd.device_raw->aim = rd.device_shared->aim;

        struct SoundIoListDevicePtr *device_list;
        if (rd.device_shared->aim == SoundIoDeviceAimOutput) {
            device_list = &rd.devices_info->output_devices;
            if (soundio_streql(rd.device_shared->id, device_id_len,
                        rd.default_render_id, rd.default_render_id_len))
            {
                rd.devices_info->default_output_index = device_list->length;
            }
        } else {
            assert(rd.device_shared->aim == SoundIoDeviceAimInput);
            device_list = &rd.devices_info->input_devices;
            if (soundio_streql(rd.device_shared->id, device_id_len,
                        rd.default_capture_id, rd.default_capture_id_len))
            {
                rd.devices_info->default_input_index = device_list->length;
            }
        }

        if ((err = SoundIoListDevicePtr_append(device_list, rd.device_shared))) {
            deinit_refresh_devices(&rd);
            return err;
        }
        if ((err = SoundIoListDevicePtr_append(device_list, rd.device_raw))) {
            deinit_refresh_devices(&rd);
            return err;
        }

        if (rd.audio_client) {
            IUnknown_Release(rd.audio_client);
            rd.audio_client = NULL;
        }
        if (FAILED(hr = IMMDevice_Activate(rd.mm_device, IID_IAUDIOCLIENT,
                        CLSCTX_ALL, NULL, (void**)&rd.audio_client)))
        {
            rd.device_shared->probe_error = SoundIoErrorOpeningDevice;
            rd.device_raw->probe_error = SoundIoErrorOpeningDevice;
            rd.device_shared = NULL;
            rd.device_raw = NULL;
            continue;
        }

        REFERENCE_TIME default_device_period;
        REFERENCE_TIME min_device_period;
        if (FAILED(hr = IAudioClient_GetDevicePeriod(rd.audio_client,
                        &default_device_period, &min_device_period)))
        {
            rd.device_shared->probe_error = SoundIoErrorOpeningDevice;
            rd.device_raw->probe_error = SoundIoErrorOpeningDevice;
            rd.device_shared = NULL;
            rd.device_raw = NULL;
            continue;
        }
        dev_w_shared->period_duration = from_reference_time(default_device_period);
        rd.device_shared->software_latency_current = dev_w_shared->period_duration;

        dev_w_raw->period_duration = from_reference_time(min_device_period);
        rd.device_raw->software_latency_min = dev_w_raw->period_duration * 2;

        if (rd.prop_store) {
            IPropertyStore_Release(rd.prop_store);
            rd.prop_store = NULL;
        }
        if (FAILED(hr = IMMDevice_OpenPropertyStore(rd.mm_device, STGM_READ, &rd.prop_store))) {
            rd.device_shared->probe_error = SoundIoErrorOpeningDevice;
            rd.device_raw->probe_error = SoundIoErrorOpeningDevice;
            rd.device_shared = NULL;
            rd.device_raw = NULL;
            continue;
        }

        if (rd.prop_variant_value_inited) {
            PropVariantClear(&rd.prop_variant_value);
            rd.prop_variant_value_inited = false;
        }
        PropVariantInit(&rd.prop_variant_value);
        rd.prop_variant_value_inited = true;
        if (FAILED(hr = IPropertyStore_GetValue(rd.prop_store,
                        PKEY_DEVICE_FRIENDLYNAME, &rd.prop_variant_value)))
        {
            rd.device_shared->probe_error = SoundIoErrorOpeningDevice;
            rd.device_raw->probe_error = SoundIoErrorOpeningDevice;
            rd.device_shared = NULL;
            rd.device_raw = NULL;
            continue;
        }
        if (!rd.prop_variant_value.pwszVal) {
            rd.device_shared->probe_error = SoundIoErrorOpeningDevice;
            rd.device_raw->probe_error = SoundIoErrorOpeningDevice;
            rd.device_shared = NULL;
            rd.device_raw = NULL;
            continue;
        }
        int device_name_len;
        if ((err = from_lpwstr(rd.prop_variant_value.pwszVal, &rd.device_shared->name, &device_name_len))) {
            rd.device_shared->probe_error = err;
            rd.device_raw->probe_error = err;
            rd.device_shared = NULL;
            rd.device_raw = NULL;
            continue;
        }

        rd.device_raw->name = soundio_str_dupe(rd.device_shared->name, device_name_len);
        if (!rd.device_raw->name) {
            deinit_refresh_devices(&rd);
            return SoundIoErrorNoMem;
        }

        // Get the format that WASAPI opens the device with for shared streams.
        // This is guaranteed to work, so we use this to modulate the sample
        // rate while holding the format constant and vice versa.
        if (rd.prop_variant_value_inited) {
            PropVariantClear(&rd.prop_variant_value);
            rd.prop_variant_value_inited = false;
        }
        PropVariantInit(&rd.prop_variant_value);
        rd.prop_variant_value_inited = true;
        if (FAILED(hr = IPropertyStore_GetValue(rd.prop_store, PKEY_AUDIOENGINE_DEVICEFORMAT,
                        &rd.prop_variant_value)))
        {
            rd.device_shared->probe_error = SoundIoErrorOpeningDevice;
            rd.device_raw->probe_error = SoundIoErrorOpeningDevice;
            rd.device_shared = NULL;
            rd.device_raw = NULL;
            continue;
        }
        WAVEFORMATEXTENSIBLE *valid_wave_format = (WAVEFORMATEXTENSIBLE *)rd.prop_variant_value.blob.pBlobData;
        if (valid_wave_format->Format.wFormatTag != WAVE_FORMAT_EXTENSIBLE) {
            rd.device_shared->probe_error = SoundIoErrorOpeningDevice;
            rd.device_raw->probe_error = SoundIoErrorOpeningDevice;
            rd.device_shared = NULL;
            rd.device_raw = NULL;
            continue;
        }
        if ((err = detect_valid_sample_rates(&rd, valid_wave_format, dev_raw,
                        AUDCLNT_SHAREMODE_EXCLUSIVE)))
        {
            rd.device_raw->probe_error = err;
            rd.device_raw = NULL;
        }
        if (rd.device_raw && (err = detect_valid_formats(&rd, valid_wave_format, dev_raw,
                        AUDCLNT_SHAREMODE_EXCLUSIVE)))
        {
            rd.device_raw->probe_error = err;
            rd.device_raw = NULL;
        }
        if (rd.device_raw && (err = detect_valid_layouts(&rd, valid_wave_format, dev_raw,
            AUDCLNT_SHAREMODE_EXCLUSIVE)))
        {
            rd.device_raw->probe_error = err;
            rd.device_raw = NULL;
        }

        if (rd.wave_format) {
            CoTaskMemFree(rd.wave_format);
            rd.wave_format = NULL;
        }
        if (FAILED(hr = IAudioClient_GetMixFormat(rd.audio_client, (WAVEFORMATEX**)&rd.wave_format))) {
            // According to MSDN GetMixFormat only applies to shared-mode devices.
            rd.device_shared->probe_error = SoundIoErrorOpeningDevice;
            rd.device_shared = NULL;
        }
        else if(rd.wave_format && (rd.wave_format->Format.wFormatTag != WAVE_FORMAT_EXTENSIBLE)) {
            rd.device_shared->probe_error = SoundIoErrorOpeningDevice;
            rd.device_shared = NULL;
        }

        if(rd.device_shared) {
            rd.device_shared->sample_rate_current = rd.wave_format->Format.nSamplesPerSec;
            rd.device_shared->current_format = from_wave_format_format(rd.wave_format);

            if (rd.device_shared->aim == SoundIoDeviceAimOutput) {
                // For output streams in shared mode,
                // WASAPI performs resampling, so any value is valid.
                // Let's pick some reasonable min and max values.
                rd.device_shared->sample_rate_count = 1;
                rd.device_shared->sample_rates = &dev_shared->prealloc_sample_rate_range;
                rd.device_shared->sample_rates[0].min = soundio_int_min(SOUNDIO_MIN_SAMPLE_RATE,
                    rd.device_shared->sample_rate_current);
                rd.device_shared->sample_rates[0].max = soundio_int_max(SOUNDIO_MAX_SAMPLE_RATE,
                    rd.device_shared->sample_rate_current);
            }
            else {
                // Shared mode input stream: mix format is all we can do.
                rd.device_shared->sample_rate_count = 1;
                rd.device_shared->sample_rates = &dev_shared->prealloc_sample_rate_range;
                rd.device_shared->sample_rates[0].min = rd.device_shared->sample_rate_current;
                rd.device_shared->sample_rates[0].max = rd.device_shared->sample_rate_current;
            }

            if ((err = detect_valid_formats(&rd, rd.wave_format, dev_shared,
                AUDCLNT_SHAREMODE_SHARED)))
            {
                rd.device_shared->probe_error = err;
                rd.device_shared = NULL;
            }
            else {
                from_wave_format_layout(rd.wave_format, &rd.device_shared->current_layout);
                rd.device_shared->layout_count = 1;
                rd.device_shared->layouts = &rd.device_shared->current_layout;
            }
        }

        IMMDevice_AddRef(rd.mm_device);
        dev_w_shared->mm_device = rd.mm_device;
        dev_w_raw->mm_device = rd.mm_device;
        rd.mm_device = NULL;

        rd.device_shared = NULL;
        rd.device_raw = NULL;
    }

    soundio_os_mutex_lock(siw->mutex);
    soundio_destroy_devices_info(siw->ready_devices_info);
    siw->ready_devices_info = rd.devices_info;
    siw->have_devices_flag = true;
    soundio_os_cond_signal(siw->cond, siw->mutex);
    soundio->on_events_signal(soundio);
    soundio_os_mutex_unlock(siw->mutex);

    rd.devices_info = NULL;
    deinit_refresh_devices(&rd);

    return 0;
}


static void shutdown_backend(struct SoundIoPrivate *si, int err) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoWasapi *siw = &si->backend_data.wasapi;
    soundio_os_mutex_lock(siw->mutex);
    siw->shutdown_err = err;
    soundio_os_cond_signal(siw->cond, siw->mutex);
    soundio->on_events_signal(soundio);
    soundio_os_mutex_unlock(siw->mutex);
}

static void device_thread_run(void *arg) {
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)arg;
    struct SoundIoWasapi *siw = &si->backend_data.wasapi;
    int err;

    HRESULT hr = CoCreateInstance(CLSID_MMDEVICEENUMERATOR, NULL,
            CLSCTX_ALL, IID_IMMDEVICEENUMERATOR, (void**)&siw->device_enumerator);
    if (FAILED(hr)) {
        shutdown_backend(si, SoundIoErrorSystemResources);
        return;
    }

    if (FAILED(hr = IMMDeviceEnumerator_RegisterEndpointNotificationCallback(
                    siw->device_enumerator, &siw->device_events)))
    {
        shutdown_backend(si, SoundIoErrorSystemResources);
        return;
    }

    soundio_os_mutex_lock(siw->scan_devices_mutex);
    for (;;) {
        if (siw->abort_flag)
            break;
        if (siw->device_scan_queued) {
            siw->device_scan_queued = false;
            soundio_os_mutex_unlock(siw->scan_devices_mutex);
            err = refresh_devices(si);
            if (err) {
                shutdown_backend(si, err);
                return;
            }
            soundio_os_mutex_lock(siw->scan_devices_mutex);
            continue;
        }
        soundio_os_cond_wait(siw->scan_devices_cond, siw->scan_devices_mutex);
    }
    soundio_os_mutex_unlock(siw->scan_devices_mutex);

    IMMDeviceEnumerator_UnregisterEndpointNotificationCallback(siw->device_enumerator, &siw->device_events);
    IMMDeviceEnumerator_Release(siw->device_enumerator);
    siw->device_enumerator = NULL;
}

static void my_flush_events(struct SoundIoPrivate *si, bool wait) {
    struct SoundIo *soundio = &si->pub;
    struct SoundIoWasapi *siw = &si->backend_data.wasapi;

    bool change = false;
    bool cb_shutdown = false;
    struct SoundIoDevicesInfo *old_devices_info = NULL;

    soundio_os_mutex_lock(siw->mutex);

    // block until have devices
    while (wait || (!siw->have_devices_flag && !siw->shutdown_err)) {
        soundio_os_cond_wait(siw->cond, siw->mutex);
        wait = false;
    }

    if (siw->shutdown_err && !siw->emitted_shutdown_cb) {
        siw->emitted_shutdown_cb = true;
        cb_shutdown = true;
    } else if (siw->ready_devices_info) {
        old_devices_info = si->safe_devices_info;
        si->safe_devices_info = siw->ready_devices_info;
        siw->ready_devices_info = NULL;
        change = true;
    }

    soundio_os_mutex_unlock(siw->mutex);

    if (cb_shutdown)
        soundio->on_backend_disconnect(soundio, siw->shutdown_err);
    else if (change)
        soundio->on_devices_change(soundio);

    soundio_destroy_devices_info(old_devices_info);
}

static void flush_events_wasapi(struct SoundIoPrivate *si) {
    my_flush_events(si, false);
}

static void wait_events_wasapi(struct SoundIoPrivate *si) {
    my_flush_events(si, false);
    my_flush_events(si, true);
}

static void wakeup_wasapi(struct SoundIoPrivate *si) {
    struct SoundIoWasapi *siw = &si->backend_data.wasapi;
    soundio_os_cond_signal(siw->cond, siw->mutex);
}

static void force_device_scan_wasapi(struct SoundIoPrivate *si) {
    struct SoundIoWasapi *siw = &si->backend_data.wasapi;
    soundio_os_mutex_lock(siw->scan_devices_mutex);
    siw->device_scan_queued = true;
    soundio_os_cond_signal(siw->scan_devices_cond, siw->scan_devices_mutex);
    soundio_os_mutex_unlock(siw->scan_devices_mutex);
}

static void outstream_thread_deinit(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamWasapi *osw = &os->backend_data.wasapi;

    if (osw->audio_render_client)
        IUnknown_Release(osw->audio_render_client);
    if (osw->audio_session_control)
        IUnknown_Release(osw->audio_session_control);
    if (osw->audio_clock_adjustment)
        IUnknown_Release(osw->audio_clock_adjustment);
    if (osw->audio_client)
        IUnknown_Release(osw->audio_client);
}

static void outstream_destroy_wasapi(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamWasapi *osw = &os->backend_data.wasapi;

    if (osw->thread) {
        SOUNDIO_ATOMIC_FLAG_CLEAR(osw->thread_exit_flag);
        if (osw->h_event)
            SetEvent(osw->h_event);

        soundio_os_mutex_lock(osw->mutex);
        soundio_os_cond_signal(osw->cond, osw->mutex);
        soundio_os_cond_signal(osw->start_cond, osw->mutex);
        soundio_os_mutex_unlock(osw->mutex);

        soundio_os_thread_destroy(osw->thread);

        osw->thread = NULL;
    }

    if (osw->h_event) {
        CloseHandle(osw->h_event);
        osw->h_event = NULL;
    }

    free(osw->stream_name);
    osw->stream_name = NULL;

    soundio_os_cond_destroy(osw->cond);
    osw->cond = NULL;

    soundio_os_cond_destroy(osw->start_cond);
    osw->start_cond = NULL;

    soundio_os_mutex_destroy(osw->mutex);
    osw->mutex = NULL;
}

static int outstream_do_open(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamWasapi *osw = &os->backend_data.wasapi;
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoDevice *device = outstream->device;
    struct SoundIoDevicePrivate *dev = (struct SoundIoDevicePrivate *)device;
    struct SoundIoDeviceWasapi *dw = &dev->backend_data.wasapi;
    HRESULT hr;

    if (FAILED(hr = IMMDevice_Activate(dw->mm_device, IID_IAUDIOCLIENT,
                    CLSCTX_ALL, NULL, (void**)&osw->audio_client)))
    {
        return SoundIoErrorOpeningDevice;
    }


    AUDCLNT_SHAREMODE share_mode;
    DWORD flags;
    REFERENCE_TIME buffer_duration;
    REFERENCE_TIME periodicity;
    WAVEFORMATEXTENSIBLE wave_format = {0};
    wave_format.Format.wFormatTag = WAVE_FORMAT_EXTENSIBLE;
    wave_format.Format.cbSize = sizeof(WAVEFORMATEXTENSIBLE) - sizeof(WAVEFORMATEX);
    if (osw->is_raw) {
        wave_format.Format.nSamplesPerSec = outstream->sample_rate;
        flags = AUDCLNT_STREAMFLAGS_EVENTCALLBACK;
        share_mode = AUDCLNT_SHAREMODE_EXCLUSIVE;
        periodicity = to_reference_time(dw->period_duration);
        buffer_duration = periodicity;
    } else {
        WAVEFORMATEXTENSIBLE *mix_format;
        if (FAILED(hr = IAudioClient_GetMixFormat(osw->audio_client, (WAVEFORMATEX **)&mix_format))) {
            return SoundIoErrorOpeningDevice;
        }
        wave_format.Format.nSamplesPerSec = mix_format->Format.nSamplesPerSec;
        CoTaskMemFree(mix_format);
        mix_format = NULL;
        osw->need_resample = (wave_format.Format.nSamplesPerSec != (DWORD)outstream->sample_rate);
        flags = osw->need_resample ? AUDCLNT_STREAMFLAGS_RATEADJUST : 0;
        share_mode = AUDCLNT_SHAREMODE_SHARED;
        periodicity = 0;
        buffer_duration = to_reference_time(4.0);
    }
    to_wave_format_layout(&outstream->layout, &wave_format);
    to_wave_format_format(outstream->format, &wave_format);
    complete_wave_format_data(&wave_format);

    if (FAILED(hr = IAudioClient_Initialize(osw->audio_client, share_mode, flags,
            buffer_duration, periodicity, (WAVEFORMATEX*)&wave_format, NULL)))
    {
        if (hr == AUDCLNT_E_BUFFER_SIZE_NOT_ALIGNED) {
            if (FAILED(hr = IAudioClient_GetBufferSize(osw->audio_client, &osw->buffer_frame_count))) {
                return SoundIoErrorOpeningDevice;
            }
            IUnknown_Release(osw->audio_client);
            osw->audio_client = NULL;
            if (FAILED(hr = IMMDevice_Activate(dw->mm_device, IID_IAUDIOCLIENT,
                            CLSCTX_ALL, NULL, (void**)&osw->audio_client)))
            {
                return SoundIoErrorOpeningDevice;
            }
            if (!osw->is_raw) {
                WAVEFORMATEXTENSIBLE *mix_format;
                if (FAILED(hr = IAudioClient_GetMixFormat(osw->audio_client, (WAVEFORMATEX **)&mix_format))) {
                    return SoundIoErrorOpeningDevice;
                }
                wave_format.Format.nSamplesPerSec = mix_format->Format.nSamplesPerSec;
                CoTaskMemFree(mix_format);
                mix_format = NULL;
                osw->need_resample = (wave_format.Format.nSamplesPerSec != (DWORD)outstream->sample_rate);
                flags = osw->need_resample ? AUDCLNT_STREAMFLAGS_RATEADJUST : 0;
                to_wave_format_layout(&outstream->layout, &wave_format);
                to_wave_format_format(outstream->format, &wave_format);
                complete_wave_format_data(&wave_format);
            }

            buffer_duration = to_reference_time(osw->buffer_frame_count / (double)outstream->sample_rate);
            if (osw->is_raw)
                periodicity = buffer_duration;
            if (FAILED(hr = IAudioClient_Initialize(osw->audio_client, share_mode, flags,
                    buffer_duration, periodicity, (WAVEFORMATEX*)&wave_format, NULL)))
            {
                if (hr == AUDCLNT_E_UNSUPPORTED_FORMAT) {
                    return SoundIoErrorIncompatibleDevice;
                } else if (hr == E_OUTOFMEMORY) {
                    return SoundIoErrorNoMem;
                } else {
                    return SoundIoErrorOpeningDevice;
                }
            }
        } else if (hr == AUDCLNT_E_UNSUPPORTED_FORMAT) {
            return SoundIoErrorIncompatibleDevice;
        } else if (hr == E_OUTOFMEMORY) {
            return SoundIoErrorNoMem;
        } else {
            return SoundIoErrorOpeningDevice;
        }
    }
    REFERENCE_TIME max_latency_ref_time;
    if (FAILED(hr = IAudioClient_GetStreamLatency(osw->audio_client, &max_latency_ref_time))) {
        return SoundIoErrorOpeningDevice;
    }
    double max_latency_sec = from_reference_time(max_latency_ref_time);
    osw->min_padding_frames = (max_latency_sec * outstream->sample_rate) + 0.5;


    if (FAILED(hr = IAudioClient_GetBufferSize(osw->audio_client, &osw->buffer_frame_count))) {
        return SoundIoErrorOpeningDevice;
    }
    outstream->software_latency = osw->buffer_frame_count / (double)outstream->sample_rate;

    if (osw->is_raw) {
        if (FAILED(hr = IAudioClient_SetEventHandle(osw->audio_client, osw->h_event))) {
            return SoundIoErrorOpeningDevice;
        }
    } else if (osw->need_resample) {
        if (FAILED(hr = IAudioClient_GetService(osw->audio_client, IID_IAUDIOCLOCKADJUSTMENT,
                        (void**)&osw->audio_clock_adjustment)))
        {
            return SoundIoErrorOpeningDevice;
        }
        if (FAILED(hr = IAudioClockAdjustment_SetSampleRate(osw->audio_clock_adjustment,
                        outstream->sample_rate)))
        {
            return SoundIoErrorOpeningDevice;
        }
    }

    if (outstream->name) {
        if (FAILED(hr = IAudioClient_GetService(osw->audio_client, IID_IAUDIOSESSIONCONTROL,
                        (void **)&osw->audio_session_control)))
        {
            return SoundIoErrorOpeningDevice;
        }

        int err;
        if ((err = to_lpwstr(outstream->name, strlen(outstream->name), &osw->stream_name))) {
            return err;
        }
        if (FAILED(hr = IAudioSessionControl_SetDisplayName(osw->audio_session_control,
                        osw->stream_name, NULL)))
        {
            return SoundIoErrorOpeningDevice;
        }
    }

    if (FAILED(hr = IAudioClient_GetService(osw->audio_client, IID_IAUDIORENDERCLIENT,
                    (void **)&osw->audio_render_client)))
    {
        return SoundIoErrorOpeningDevice;
    }

    return 0;
}

static void outstream_shared_run(struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamWasapi *osw = &os->backend_data.wasapi;
    struct SoundIoOutStream *outstream = &os->pub;

    HRESULT hr;

    UINT32 frames_used;
    if (FAILED(hr = IAudioClient_GetCurrentPadding(osw->audio_client, &frames_used))) {
        outstream->error_callback(outstream, SoundIoErrorStreaming);
        return;
    }
    osw->writable_frame_count = osw->buffer_frame_count - frames_used;
    if (osw->writable_frame_count <= 0) {
        outstream->error_callback(outstream, SoundIoErrorStreaming);
        return;
    }
    int frame_count_min = soundio_int_max(0, (int)osw->min_padding_frames - (int)frames_used);
    outstream->write_callback(outstream, frame_count_min, osw->writable_frame_count);

    if (FAILED(hr = IAudioClient_Start(osw->audio_client))) {
        outstream->error_callback(outstream, SoundIoErrorStreaming);
        return;
    }

    for (;;) {
        if (FAILED(hr = IAudioClient_GetCurrentPadding(osw->audio_client, &frames_used))) {
            outstream->error_callback(outstream, SoundIoErrorStreaming);
            return;
        }
        osw->writable_frame_count = osw->buffer_frame_count - frames_used;
        double time_until_underrun = frames_used / (double)outstream->sample_rate;
        double wait_time = time_until_underrun / 2.0;
        soundio_os_mutex_lock(osw->mutex);
        soundio_os_cond_timed_wait(osw->cond, osw->mutex, wait_time);
        if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osw->thread_exit_flag)) {
            soundio_os_mutex_unlock(osw->mutex);
            return;
        }
        soundio_os_mutex_unlock(osw->mutex);
        bool reset_buffer = false;
        if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osw->clear_buffer_flag)) {
            if (!osw->is_paused) {
                if (FAILED(hr = IAudioClient_Stop(osw->audio_client))) {
                    outstream->error_callback(outstream, SoundIoErrorStreaming);
                    return;
                }
                osw->is_paused = true;
            }
            if (FAILED(hr = IAudioClient_Reset(osw->audio_client))) {
                outstream->error_callback(outstream, SoundIoErrorStreaming);
                return;
            }
            SOUNDIO_ATOMIC_FLAG_CLEAR(osw->pause_resume_flag);
            reset_buffer = true;
        }
        if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osw->pause_resume_flag)) {
            bool pause = SOUNDIO_ATOMIC_LOAD(osw->desired_pause_state);
            if (pause && !osw->is_paused) {
                if (FAILED(hr = IAudioClient_Stop(osw->audio_client))) {
                    outstream->error_callback(outstream, SoundIoErrorStreaming);
                    return;
                }
                osw->is_paused = true;
            } else if (!pause && osw->is_paused) {
                if (FAILED(hr = IAudioClient_Start(osw->audio_client))) {
                    outstream->error_callback(outstream, SoundIoErrorStreaming);
                    return;
                }
                osw->is_paused = false;
            }
        }

        if (FAILED(hr = IAudioClient_GetCurrentPadding(osw->audio_client, &frames_used))) {
            outstream->error_callback(outstream, SoundIoErrorStreaming);
            return;
        }
        osw->writable_frame_count = osw->buffer_frame_count - frames_used;
        if (osw->writable_frame_count > 0) {
            if (frames_used == 0 && !reset_buffer)
                outstream->underflow_callback(outstream);
            int frame_count_min = soundio_int_max(0, (int)osw->min_padding_frames - (int)frames_used);
            outstream->write_callback(outstream, frame_count_min, osw->writable_frame_count);
        }
    }
}

static void outstream_raw_run(struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamWasapi *osw = &os->backend_data.wasapi;
    struct SoundIoOutStream *outstream = &os->pub;

    HRESULT hr;

    outstream->write_callback(outstream, osw->buffer_frame_count, osw->buffer_frame_count);

    if (FAILED(hr = IAudioClient_Start(osw->audio_client))) {
        outstream->error_callback(outstream, SoundIoErrorStreaming);
        return;
    }

    for (;;) {
        WaitForSingleObject(osw->h_event, INFINITE);
        if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osw->thread_exit_flag))
            return;
        if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osw->pause_resume_flag)) {
            bool pause = SOUNDIO_ATOMIC_LOAD(osw->desired_pause_state);
            if (pause && !osw->is_paused) {
                if (FAILED(hr = IAudioClient_Stop(osw->audio_client))) {
                    outstream->error_callback(outstream, SoundIoErrorStreaming);
                    return;
                }
                osw->is_paused = true;
            } else if (!pause && osw->is_paused) {
                if (FAILED(hr = IAudioClient_Start(osw->audio_client))) {
                    outstream->error_callback(outstream, SoundIoErrorStreaming);
                    return;
                }
                osw->is_paused = false;
            }
        }

        outstream->write_callback(outstream, osw->buffer_frame_count, osw->buffer_frame_count);
    }
}

static void outstream_thread_run(void *arg) {
    struct SoundIoOutStreamPrivate *os = (struct SoundIoOutStreamPrivate *)arg;
    struct SoundIoOutStreamWasapi *osw = &os->backend_data.wasapi;
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoDevice *device = outstream->device;
    struct SoundIo *soundio = device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;

    int err;
    if ((err = outstream_do_open(si, os))) {
        outstream_thread_deinit(si, os);

        soundio_os_mutex_lock(osw->mutex);
        osw->open_err = err;
        osw->open_complete = true;
        soundio_os_cond_signal(osw->cond, osw->mutex);
        soundio_os_mutex_unlock(osw->mutex);
        return;
    }

    soundio_os_mutex_lock(osw->mutex);
    osw->open_complete = true;
    soundio_os_cond_signal(osw->cond, osw->mutex);
    for (;;) {
        if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osw->thread_exit_flag)) {
            soundio_os_mutex_unlock(osw->mutex);
            return;
        }
        if (osw->started) {
            soundio_os_mutex_unlock(osw->mutex);
            break;
        }
        soundio_os_cond_wait(osw->start_cond, osw->mutex);
    }

    if (osw->is_raw)
        outstream_raw_run(os);
    else
        outstream_shared_run(os);

    outstream_thread_deinit(si, os);
}

static int outstream_open_wasapi(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamWasapi *osw = &os->backend_data.wasapi;
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoDevice *device = outstream->device;
    struct SoundIo *soundio = &si->pub;

    SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osw->pause_resume_flag);
    SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osw->clear_buffer_flag);
    SOUNDIO_ATOMIC_STORE(osw->desired_pause_state, false);

    // All the COM functions are supposed to be called from the same thread. libsoundio API does not
    // restrict the calling thread context in this way. Furthermore, the user might have called
    // CoInitializeEx with a different threading model than Single Threaded Apartment.
    // So we create a thread to do all the initialization and teardown, and communicate state
    // via conditions and signals. The thread for initialization and teardown is also used
    // for the realtime code calls the user write_callback.

    osw->is_raw = device->is_raw;

    if (!(osw->cond = soundio_os_cond_create())) {
        outstream_destroy_wasapi(si, os);
        return SoundIoErrorNoMem;
    }

    if (!(osw->start_cond = soundio_os_cond_create())) {
        outstream_destroy_wasapi(si, os);
        return SoundIoErrorNoMem;
    }

    if (!(osw->mutex = soundio_os_mutex_create())) {
        outstream_destroy_wasapi(si, os);
        return SoundIoErrorNoMem;
    }

    if (osw->is_raw) {
        osw->h_event = CreateEvent(NULL, FALSE, FALSE, NULL);
        if (!osw->h_event) {
            outstream_destroy_wasapi(si, os);
            return SoundIoErrorOpeningDevice;
        }
    }

    SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(osw->thread_exit_flag);
    int err;
    if ((err = soundio_os_thread_create(outstream_thread_run, os,
                    soundio->emit_rtprio_warning, &osw->thread)))
    {
        outstream_destroy_wasapi(si, os);
        return err;
    }

    soundio_os_mutex_lock(osw->mutex);
    while (!osw->open_complete)
        soundio_os_cond_wait(osw->cond, osw->mutex);
    soundio_os_mutex_unlock(osw->mutex);

    if (osw->open_err) {
        outstream_destroy_wasapi(si, os);
        return osw->open_err;
    }

    return 0;
}

static int outstream_pause_wasapi(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os, bool pause) {
    struct SoundIoOutStreamWasapi *osw = &os->backend_data.wasapi;

    SOUNDIO_ATOMIC_STORE(osw->desired_pause_state, pause);
    SOUNDIO_ATOMIC_FLAG_CLEAR(osw->pause_resume_flag);
    if (osw->h_event) {
        SetEvent(osw->h_event);
    } else {
        soundio_os_mutex_lock(osw->mutex);
        soundio_os_cond_signal(osw->cond, osw->mutex);
        soundio_os_mutex_unlock(osw->mutex);
    }

    return 0;
}

static int outstream_start_wasapi(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamWasapi *osw = &os->backend_data.wasapi;

    soundio_os_mutex_lock(osw->mutex);
    osw->started = true;
    soundio_os_cond_signal(osw->start_cond, osw->mutex);
    soundio_os_mutex_unlock(osw->mutex);

    return 0;
}

static int outstream_begin_write_wasapi(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os,
        struct SoundIoChannelArea **out_areas, int *frame_count)
{
    struct SoundIoOutStreamWasapi *osw = &os->backend_data.wasapi;
    struct SoundIoOutStream *outstream = &os->pub;
    HRESULT hr;

    osw->write_frame_count = *frame_count;


    char *data;
    if (FAILED(hr = IAudioRenderClient_GetBuffer(osw->audio_render_client,
                    osw->write_frame_count, (BYTE**)&data)))
    {
        return SoundIoErrorStreaming;
    }

    for (int ch = 0; ch < outstream->layout.channel_count; ch += 1) {
        osw->areas[ch].ptr = data + ch * outstream->bytes_per_sample;
        osw->areas[ch].step = outstream->bytes_per_frame;
    }

    *out_areas = osw->areas;

    return 0;
}

static int outstream_end_write_wasapi(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamWasapi *osw = &os->backend_data.wasapi;
    HRESULT hr;
    if (FAILED(hr = IAudioRenderClient_ReleaseBuffer(osw->audio_render_client, osw->write_frame_count, 0))) {
        return SoundIoErrorStreaming;
    }
    return 0;
}

static int outstream_clear_buffer_wasapi(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os) {
    struct SoundIoOutStreamWasapi *osw = &os->backend_data.wasapi;

    if (osw->h_event) {
        return SoundIoErrorIncompatibleDevice;
    } else {
        SOUNDIO_ATOMIC_FLAG_CLEAR(osw->clear_buffer_flag);
        soundio_os_mutex_lock(osw->mutex);
        soundio_os_cond_signal(osw->cond, osw->mutex);
        soundio_os_mutex_unlock(osw->mutex);
    }

    return 0;
}

static int outstream_get_latency_wasapi(struct SoundIoPrivate *si, struct SoundIoOutStreamPrivate *os,
        double *out_latency)
{
    struct SoundIoOutStream *outstream = &os->pub;
    struct SoundIoOutStreamWasapi *osw = &os->backend_data.wasapi;

    HRESULT hr;
    UINT32 frames_used;
    if (FAILED(hr = IAudioClient_GetCurrentPadding(osw->audio_client, &frames_used))) {
        return SoundIoErrorStreaming;
    }

    *out_latency = frames_used / (double)outstream->sample_rate;
    return 0;
}

static void instream_thread_deinit(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamWasapi *isw = &is->backend_data.wasapi;

    if (isw->audio_capture_client)
        IUnknown_Release(isw->audio_capture_client);
    if (isw->audio_client)
        IUnknown_Release(isw->audio_client);
}


static void instream_destroy_wasapi(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamWasapi *isw = &is->backend_data.wasapi;

    if (isw->thread) {
        SOUNDIO_ATOMIC_FLAG_CLEAR(isw->thread_exit_flag);
        if (isw->h_event)
            SetEvent(isw->h_event);

        soundio_os_mutex_lock(isw->mutex);
        soundio_os_cond_signal(isw->cond, isw->mutex);
        soundio_os_cond_signal(isw->start_cond, isw->mutex);
        soundio_os_mutex_unlock(isw->mutex);
        soundio_os_thread_destroy(isw->thread);

        isw->thread = NULL;
    }

    if (isw->h_event) {
        CloseHandle(isw->h_event);
        isw->h_event = NULL;
    }

    soundio_os_cond_destroy(isw->cond);
    isw->cond = NULL;

    soundio_os_cond_destroy(isw->start_cond);
    isw->start_cond = NULL;

    soundio_os_mutex_destroy(isw->mutex);
    isw->mutex = NULL;
}

static int instream_do_open(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamWasapi *isw = &is->backend_data.wasapi;
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoDevice *device = instream->device;
    struct SoundIoDevicePrivate *dev = (struct SoundIoDevicePrivate *)device;
    struct SoundIoDeviceWasapi *dw = &dev->backend_data.wasapi;
    HRESULT hr;

    if (FAILED(hr = IMMDevice_Activate(dw->mm_device, IID_IAUDIOCLIENT,
                    CLSCTX_ALL, NULL, (void**)&isw->audio_client)))
    {
        return SoundIoErrorOpeningDevice;
    }

    AUDCLNT_SHAREMODE share_mode;
    DWORD flags;
    REFERENCE_TIME buffer_duration;
    REFERENCE_TIME periodicity;
    WAVEFORMATEXTENSIBLE wave_format = {0};
    wave_format.Format.wFormatTag = WAVE_FORMAT_EXTENSIBLE;
    wave_format.Format.cbSize = sizeof(WAVEFORMATEXTENSIBLE) - sizeof(WAVEFORMATEX);
    if (isw->is_raw) {
        wave_format.Format.nSamplesPerSec = instream->sample_rate;
        flags = AUDCLNT_STREAMFLAGS_EVENTCALLBACK;
        share_mode = AUDCLNT_SHAREMODE_EXCLUSIVE;
        periodicity = to_reference_time(dw->period_duration);
        buffer_duration = periodicity;
    } else {
        WAVEFORMATEXTENSIBLE *mix_format;
        if (FAILED(hr = IAudioClient_GetMixFormat(isw->audio_client, (WAVEFORMATEX **)&mix_format))) {
            return SoundIoErrorOpeningDevice;
        }
        wave_format.Format.nSamplesPerSec = mix_format->Format.nSamplesPerSec;
        CoTaskMemFree(mix_format);
        mix_format = NULL;
        if (wave_format.Format.nSamplesPerSec != (DWORD)instream->sample_rate) {
            return SoundIoErrorIncompatibleDevice;
        }
        flags = 0;
        share_mode = AUDCLNT_SHAREMODE_SHARED;
        periodicity = 0;
        buffer_duration = to_reference_time(4.0);
    }
    to_wave_format_layout(&instream->layout, &wave_format);
    to_wave_format_format(instream->format, &wave_format);
    complete_wave_format_data(&wave_format);

    if (FAILED(hr = IAudioClient_Initialize(isw->audio_client, share_mode, flags,
            buffer_duration, periodicity, (WAVEFORMATEX*)&wave_format, NULL)))
    {
        if (hr == AUDCLNT_E_BUFFER_SIZE_NOT_ALIGNED) {
            if (FAILED(hr = IAudioClient_GetBufferSize(isw->audio_client, &isw->buffer_frame_count))) {
                return SoundIoErrorOpeningDevice;
            }
            IUnknown_Release(isw->audio_client);
            isw->audio_client = NULL;
            if (FAILED(hr = IMMDevice_Activate(dw->mm_device, IID_IAUDIOCLIENT,
                            CLSCTX_ALL, NULL, (void**)&isw->audio_client)))
            {
                return SoundIoErrorOpeningDevice;
            }
            if (!isw->is_raw) {
                WAVEFORMATEXTENSIBLE *mix_format;
                if (FAILED(hr = IAudioClient_GetMixFormat(isw->audio_client, (WAVEFORMATEX **)&mix_format))) {
                    return SoundIoErrorOpeningDevice;
                }
                wave_format.Format.nSamplesPerSec = mix_format->Format.nSamplesPerSec;
                CoTaskMemFree(mix_format);
                mix_format = NULL;
                flags = 0;
                to_wave_format_layout(&instream->layout, &wave_format);
                to_wave_format_format(instream->format, &wave_format);
                complete_wave_format_data(&wave_format);
            }

            buffer_duration = to_reference_time(isw->buffer_frame_count / (double)instream->sample_rate);
            if (isw->is_raw)
                periodicity = buffer_duration;
            if (FAILED(hr = IAudioClient_Initialize(isw->audio_client, share_mode, flags,
                    buffer_duration, periodicity, (WAVEFORMATEX*)&wave_format, NULL)))
            {
                if (hr == AUDCLNT_E_UNSUPPORTED_FORMAT) {
                    return SoundIoErrorIncompatibleDevice;
                } else if (hr == E_OUTOFMEMORY) {
                    return SoundIoErrorNoMem;
                } else {
                    return SoundIoErrorOpeningDevice;
                }
            }
        } else if (hr == AUDCLNT_E_UNSUPPORTED_FORMAT) {
            return SoundIoErrorIncompatibleDevice;
        } else if (hr == E_OUTOFMEMORY) {
            return SoundIoErrorNoMem;
        } else {
            return SoundIoErrorOpeningDevice;
        }
    }
    if (FAILED(hr = IAudioClient_GetBufferSize(isw->audio_client, &isw->buffer_frame_count))) {
        return SoundIoErrorOpeningDevice;
    }
    if (instream->software_latency == 0.0)
        instream->software_latency = 1.0;
    instream->software_latency = soundio_double_clamp(device->software_latency_min,
            instream->software_latency, device->software_latency_max);
    if (isw->is_raw)
        instream->software_latency = isw->buffer_frame_count / (double)instream->sample_rate;

    if (isw->is_raw) {
        if (FAILED(hr = IAudioClient_SetEventHandle(isw->audio_client, isw->h_event))) {
            return SoundIoErrorOpeningDevice;
        }
    }

    if (instream->name) {
        if (FAILED(hr = IAudioClient_GetService(isw->audio_client, IID_IAUDIOSESSIONCONTROL,
                        (void **)&isw->audio_session_control)))
        {
            return SoundIoErrorOpeningDevice;
        }

        int err;
        if ((err = to_lpwstr(instream->name, strlen(instream->name), &isw->stream_name))) {
            return err;
        }
        if (FAILED(hr = IAudioSessionControl_SetDisplayName(isw->audio_session_control,
                        isw->stream_name, NULL)))
        {
            return SoundIoErrorOpeningDevice;
        }
    }

    if (FAILED(hr = IAudioClient_GetService(isw->audio_client, IID_IAUDIOCAPTURECLIENT,
                    (void **)&isw->audio_capture_client)))
    {
        return SoundIoErrorOpeningDevice;
    }

    return 0;
}

static void instream_raw_run(struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamWasapi *isw = &is->backend_data.wasapi;
    struct SoundIoInStream *instream = &is->pub;

    HRESULT hr;

    if (FAILED(hr = IAudioClient_Start(isw->audio_client))) {
        instream->error_callback(instream, SoundIoErrorStreaming);
        return;
    }

    for (;;) {
        WaitForSingleObject(isw->h_event, INFINITE);
        if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(isw->thread_exit_flag))
            return;

        instream->read_callback(instream, isw->buffer_frame_count, isw->buffer_frame_count);
    }
}

static void instream_shared_run(struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamWasapi *isw = &is->backend_data.wasapi;
    struct SoundIoInStream *instream = &is->pub;

    HRESULT hr;

    if (FAILED(hr = IAudioClient_Start(isw->audio_client))) {
        instream->error_callback(instream, SoundIoErrorStreaming);
        return;
    }

    for (;;) {
        soundio_os_mutex_lock(isw->mutex);
        soundio_os_cond_timed_wait(isw->cond, isw->mutex, instream->software_latency / 2.0);
        if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(isw->thread_exit_flag)) {
            soundio_os_mutex_unlock(isw->mutex);
            return;
        }
        soundio_os_mutex_unlock(isw->mutex);

        UINT32 frames_available;
        if (FAILED(hr = IAudioClient_GetCurrentPadding(isw->audio_client, &frames_available))) {
            instream->error_callback(instream, SoundIoErrorStreaming);
            return;
        }

        isw->readable_frame_count = frames_available;
        if (isw->readable_frame_count > 0)
            instream->read_callback(instream, 0, isw->readable_frame_count);
    }
}

static void instream_thread_run(void *arg) {
    struct SoundIoInStreamPrivate *is = (struct SoundIoInStreamPrivate *)arg;
    struct SoundIoInStreamWasapi *isw = &is->backend_data.wasapi;
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoDevice *device = instream->device;
    struct SoundIo *soundio = device->soundio;
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)soundio;

    int err;
    if ((err = instream_do_open(si, is))) {
        instream_thread_deinit(si, is);

        soundio_os_mutex_lock(isw->mutex);
        isw->open_err = err;
        isw->open_complete = true;
        soundio_os_cond_signal(isw->cond, isw->mutex);
        soundio_os_mutex_unlock(isw->mutex);
        return;
    }

    soundio_os_mutex_lock(isw->mutex);
    isw->open_complete = true;
    soundio_os_cond_signal(isw->cond, isw->mutex);
    for (;;) {
        if (!SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(isw->thread_exit_flag)) {
            soundio_os_mutex_unlock(isw->mutex);
            return;
        }
        if (isw->started) {
            soundio_os_mutex_unlock(isw->mutex);
            break;
        }
        soundio_os_cond_wait(isw->start_cond, isw->mutex);
    }

    if (isw->is_raw)
        instream_raw_run(is);
    else
        instream_shared_run(is);

    instream_thread_deinit(si, is);
}

static int instream_open_wasapi(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamWasapi *isw = &is->backend_data.wasapi;
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoDevice *device = instream->device;
    struct SoundIo *soundio = &si->pub;

    // All the COM functions are supposed to be called from the same thread. libsoundio API does not
    // restrict the calling thread context in this way. Furthermore, the user might have called
    // CoInitializeEx with a different threading model than Single Threaded Apartment.
    // So we create a thread to do all the initialization and teardown, and communicate state
    // via conditions and signals. The thread for initialization and teardown is also used
    // for the realtime code calls the user write_callback.

    isw->is_raw = device->is_raw;

    if (!(isw->cond = soundio_os_cond_create())) {
        instream_destroy_wasapi(si, is);
        return SoundIoErrorNoMem;
    }

    if (!(isw->start_cond = soundio_os_cond_create())) {
        instream_destroy_wasapi(si, is);
        return SoundIoErrorNoMem;
    }

    if (!(isw->mutex = soundio_os_mutex_create())) {
        instream_destroy_wasapi(si, is);
        return SoundIoErrorNoMem;
    }

    if (isw->is_raw) {
        isw->h_event = CreateEvent(NULL, FALSE, FALSE, NULL);
        if (!isw->h_event) {
            instream_destroy_wasapi(si, is);
            return SoundIoErrorOpeningDevice;
        }
    }

    SOUNDIO_ATOMIC_FLAG_TEST_AND_SET(isw->thread_exit_flag);
    int err;
    if ((err = soundio_os_thread_create(instream_thread_run, is,
                    soundio->emit_rtprio_warning, &isw->thread)))
    {
        instream_destroy_wasapi(si, is);
        return err;
    }

    soundio_os_mutex_lock(isw->mutex);
    while (!isw->open_complete)
        soundio_os_cond_wait(isw->cond, isw->mutex);
    soundio_os_mutex_unlock(isw->mutex);

    if (isw->open_err) {
        instream_destroy_wasapi(si, is);
        return isw->open_err;
    }

    return 0;
}

static int instream_pause_wasapi(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is, bool pause) {
    struct SoundIoInStreamWasapi *isw = &is->backend_data.wasapi;
    HRESULT hr;
    if (pause && !isw->is_paused) {
        if (FAILED(hr = IAudioClient_Stop(isw->audio_client)))
            return SoundIoErrorStreaming;
        isw->is_paused = true;
    } else if (!pause && isw->is_paused) {
        if (FAILED(hr = IAudioClient_Start(isw->audio_client)))
            return SoundIoErrorStreaming;
        isw->is_paused = false;
    }
    return 0;
}

static int instream_start_wasapi(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamWasapi *isw = &is->backend_data.wasapi;

    soundio_os_mutex_lock(isw->mutex);
    isw->started = true;
    soundio_os_cond_signal(isw->start_cond, isw->mutex);
    soundio_os_mutex_unlock(isw->mutex);

    return 0;
}

static int instream_begin_read_wasapi(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is,
        struct SoundIoChannelArea **out_areas, int *frame_count)
{
    struct SoundIoInStreamWasapi *isw = &is->backend_data.wasapi;
    struct SoundIoInStream *instream = &is->pub;
    HRESULT hr;

    if (isw->read_buf_frames_left <= 0) {
        UINT32 frames_to_read;
        DWORD flags;
        if (FAILED(hr = IAudioCaptureClient_GetBuffer(isw->audio_capture_client,
                        (BYTE**)&isw->read_buf, &frames_to_read, &flags, NULL, NULL)))
        {
            return SoundIoErrorStreaming;
        }
        isw->read_buf_frames_left = frames_to_read;
        if (flags & AUDCLNT_BUFFERFLAGS_SILENT)
            isw->read_buf = NULL;
    }

    isw->read_frame_count = soundio_int_min(*frame_count, isw->read_buf_frames_left);
    *frame_count = isw->read_frame_count;

    if (isw->read_buf) {
        for (int ch = 0; ch < instream->layout.channel_count; ch += 1) {
            isw->areas[ch].ptr = isw->read_buf + ch * instream->bytes_per_sample;
            isw->areas[ch].step = instream->bytes_per_frame;
        }

        *out_areas = isw->areas;
    } else {
        *out_areas = NULL;
    }

    return 0;
}

static int instream_end_read_wasapi(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is) {
    struct SoundIoInStreamWasapi *isw = &is->backend_data.wasapi;
    HRESULT hr;
    if (FAILED(hr = IAudioCaptureClient_ReleaseBuffer(isw->audio_capture_client, isw->read_frame_count))) {
        return SoundIoErrorStreaming;
    }
    isw->read_buf_frames_left -= isw->read_frame_count;
    return 0;
}

static int instream_get_latency_wasapi(struct SoundIoPrivate *si, struct SoundIoInStreamPrivate *is,
        double *out_latency)
{
    struct SoundIoInStream *instream = &is->pub;
    struct SoundIoInStreamWasapi *isw = &is->backend_data.wasapi;

    HRESULT hr;
    UINT32 frames_used;
    if (FAILED(hr = IAudioClient_GetCurrentPadding(isw->audio_client, &frames_used))) {
        return SoundIoErrorStreaming;
    }

    *out_latency = frames_used / (double)instream->sample_rate;
    return 0;
}


static void destroy_wasapi(struct SoundIoPrivate *si) {
    struct SoundIoWasapi *siw = &si->backend_data.wasapi;

    if (siw->thread) {
        soundio_os_mutex_lock(siw->scan_devices_mutex);
        siw->abort_flag = true;
        soundio_os_cond_signal(siw->scan_devices_cond, siw->scan_devices_mutex);
        soundio_os_mutex_unlock(siw->scan_devices_mutex);
        soundio_os_thread_destroy(siw->thread);
    }

    if (siw->cond)
        soundio_os_cond_destroy(siw->cond);

    if (siw->scan_devices_cond)
        soundio_os_cond_destroy(siw->scan_devices_cond);

    if (siw->scan_devices_mutex)
        soundio_os_mutex_destroy(siw->scan_devices_mutex);

    if (siw->mutex)
        soundio_os_mutex_destroy(siw->mutex);

    soundio_destroy_devices_info(siw->ready_devices_info);
}

static inline struct SoundIoPrivate *soundio_MMNotificationClient_si(IMMNotificationClient *client) {
    struct SoundIoWasapi *siw = (struct SoundIoWasapi *)(((char *)client) - offsetof(struct SoundIoWasapi, device_events));
    struct SoundIoPrivate *si = (struct SoundIoPrivate *)(((char *)siw) - offsetof(struct SoundIoPrivate, backend_data));
    return si;
}

static STDMETHODIMP soundio_MMNotificationClient_QueryInterface(IMMNotificationClient *client,
        REFIID riid, void **ppv)
{
    if (IS_EQUAL_IID(riid, &IID_IUnknown) || IS_EQUAL_IID(riid, &IID_IMMNotificationClient)) {
        *ppv = client;
        IUnknown_AddRef(client);
        return S_OK;
    } else {
       *ppv = NULL;
        return E_NOINTERFACE;
    }
}

static STDMETHODIMP_(ULONG) soundio_MMNotificationClient_AddRef(IMMNotificationClient *client) {
    struct SoundIoPrivate *si = soundio_MMNotificationClient_si(client);
    struct SoundIoWasapi *siw = &si->backend_data.wasapi;
    return InterlockedIncrement(&siw->device_events_refs);
}

static STDMETHODIMP_(ULONG) soundio_MMNotificationClient_Release(IMMNotificationClient *client) {
    struct SoundIoPrivate *si = soundio_MMNotificationClient_si(client);
    struct SoundIoWasapi *siw = &si->backend_data.wasapi;
    return InterlockedDecrement(&siw->device_events_refs);
}

static HRESULT queue_device_scan(IMMNotificationClient *client) {
    struct SoundIoPrivate *si = soundio_MMNotificationClient_si(client);
    force_device_scan_wasapi(si);
    return S_OK;
}

static STDMETHODIMP soundio_MMNotificationClient_OnDeviceStateChanged(IMMNotificationClient *client,
        LPCWSTR wid, DWORD state)
{
    return queue_device_scan(client);
}

static STDMETHODIMP soundio_MMNotificationClient_OnDeviceAdded(IMMNotificationClient *client, LPCWSTR wid) {
    return queue_device_scan(client);
}

static STDMETHODIMP soundio_MMNotificationClient_OnDeviceRemoved(IMMNotificationClient *client, LPCWSTR wid) {
    return queue_device_scan(client);
}

static STDMETHODIMP soundio_MMNotificationClient_OnDefaultDeviceChange(IMMNotificationClient *client,
        EDataFlow flow, ERole role, LPCWSTR wid)
{
    return queue_device_scan(client);
}

static STDMETHODIMP soundio_MMNotificationClient_OnPropertyValueChanged(IMMNotificationClient *client,
        LPCWSTR wid, const PROPERTYKEY key)
{
    return queue_device_scan(client);
}


static struct IMMNotificationClientVtbl soundio_MMNotificationClient = {
    soundio_MMNotificationClient_QueryInterface,
    soundio_MMNotificationClient_AddRef,
    soundio_MMNotificationClient_Release,
    soundio_MMNotificationClient_OnDeviceStateChanged,
    soundio_MMNotificationClient_OnDeviceAdded,
    soundio_MMNotificationClient_OnDeviceRemoved,
    soundio_MMNotificationClient_OnDefaultDeviceChange,
    soundio_MMNotificationClient_OnPropertyValueChanged,
};

int soundio_wasapi_init(struct SoundIoPrivate *si) {
    struct SoundIoWasapi *siw = &si->backend_data.wasapi;
    int err;

    siw->device_scan_queued = true;

    siw->mutex = soundio_os_mutex_create();
    if (!siw->mutex) {
        destroy_wasapi(si);
        return SoundIoErrorNoMem;
    }

    siw->scan_devices_mutex = soundio_os_mutex_create();
    if (!siw->scan_devices_mutex) {
        destroy_wasapi(si);
        return SoundIoErrorNoMem;
    }

    siw->cond = soundio_os_cond_create();
    if (!siw->cond) {
        destroy_wasapi(si);
        return SoundIoErrorNoMem;
    }

    siw->scan_devices_cond = soundio_os_cond_create();
    if (!siw->scan_devices_cond) {
        destroy_wasapi(si);
        return SoundIoErrorNoMem;
    }

    siw->device_events.lpVtbl = &soundio_MMNotificationClient;
    siw->device_events_refs = 1;

    if ((err = soundio_os_thread_create(device_thread_run, si, NULL, &siw->thread))) {
        destroy_wasapi(si);
        return err;
    }

    si->destroy = destroy_wasapi;
    si->flush_events = flush_events_wasapi;
    si->wait_events = wait_events_wasapi;
    si->wakeup = wakeup_wasapi;
    si->force_device_scan = force_device_scan_wasapi;

    si->outstream_open = outstream_open_wasapi;
    si->outstream_destroy = outstream_destroy_wasapi;
    si->outstream_start = outstream_start_wasapi;
    si->outstream_begin_write = outstream_begin_write_wasapi;
    si->outstream_end_write = outstream_end_write_wasapi;
    si->outstream_clear_buffer = outstream_clear_buffer_wasapi;
    si->outstream_pause = outstream_pause_wasapi;
    si->outstream_get_latency = outstream_get_latency_wasapi;

    si->instream_open = instream_open_wasapi;
    si->instream_destroy = instream_destroy_wasapi;
    si->instream_start = instream_start_wasapi;
    si->instream_begin_read = instream_begin_read_wasapi;
    si->instream_end_read = instream_end_read_wasapi;
    si->instream_pause = instream_pause_wasapi;
    si->instream_get_latency = instream_get_latency_wasapi;

    return 0;
}
#endif
