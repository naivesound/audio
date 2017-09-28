#include "portmidi.h"
/* pmutil.c -- some helpful utilities for building midi
               applications that use PortMidi
 */
#include <stdlib.h>
#include <assert.h>
#include <string.h>
// (amalg) #include "portmidi.h"
// (amalg) #include "pmutil.h"
// (amalg) #include "pminternal.h"

#ifdef WIN32
#define bzero(addr, siz) memset(addr, 0, siz)
#endif

// #define QUEUE_DEBUG 1
#ifdef QUEUE_DEBUG
// (amalg) #include "stdio.h"
#endif

typedef struct {
    long head;
    long tail;
    long len;
    long overflow;
    int32_t msg_size; /* number of int32_t in a message including extra word */
    int32_t peek_overflow;
    int32_t *buffer;
    int32_t *peek;
    int32_t peek_flag;
} PmQueueRep;


PMEXPORT PmQueue *Pm_QueueCreate(long num_msgs, int32_t bytes_per_msg)
{
    int32_t int32s_per_msg = 
            (int32_t) (((bytes_per_msg + sizeof(int32_t) - 1) &
                       ~(sizeof(int32_t) - 1)) / sizeof(int32_t));
    PmQueueRep *queue = (PmQueueRep *) pm_alloc(sizeof(PmQueueRep));
    if (!queue) /* memory allocation failed */
        return NULL;

    /* need extra word per message for non-zero encoding */
    queue->len = num_msgs * (int32s_per_msg + 1);
    queue->buffer = (int32_t *) pm_alloc(queue->len * sizeof(int32_t));
    bzero(queue->buffer, queue->len * sizeof(int32_t));
    if (!queue->buffer) {
        pm_free(queue);
        return NULL;
    } else { /* allocate the "peek" buffer */
        queue->peek = (int32_t *) pm_alloc(int32s_per_msg * sizeof(int32_t));
        if (!queue->peek) {
            /* free everything allocated so far and return */
            pm_free(queue->buffer);
            pm_free(queue);
            return NULL;
        }
    }
    bzero(queue->buffer, queue->len * sizeof(int32_t));
    queue->head = 0;
    queue->tail = 0;
    /* msg_size is in words */
    queue->msg_size = int32s_per_msg + 1; /* note extra word is counted */
    queue->overflow = FALSE;
    queue->peek_overflow = FALSE;
    queue->peek_flag = FALSE;
    return queue;
}


PMEXPORT PmError Pm_QueueDestroy(PmQueue *q)
{
    PmQueueRep *queue = (PmQueueRep *) q;
        
    /* arg checking */
    if (!queue || !queue->buffer || !queue->peek) 
                return pmBadPtr;
    
    pm_free(queue->peek);
    pm_free(queue->buffer);
    pm_free(queue);
    return pmNoError;
}


PMEXPORT PmError Pm_Dequeue(PmQueue *q, void *msg)
{
    long head;
    PmQueueRep *queue = (PmQueueRep *) q;
    int i;
    int32_t *msg_as_int32 = (int32_t *) msg;

    /* arg checking */
    if (!queue)
        return pmBadPtr;
    /* a previous peek operation encountered an overflow, but the overflow
     * has not yet been reported to client, so do it now. No message is
     * returned, but on the next call, we will return the peek buffer.
     */
    if (queue->peek_overflow) {
        queue->peek_overflow = FALSE;
        return pmBufferOverflow;
    }
    if (queue->peek_flag) {
        memcpy(msg, queue->peek, (queue->msg_size - 1) * sizeof(int32_t));
        queue->peek_flag = FALSE;
        return pmGotData;
    }

    head = queue->head;
    /* if writer overflows, it writes queue->overflow = tail+1 so that
     * when the reader gets to that position in the buffer, it can 
     * return the overflow condition to the reader. The problem is that
     * at overflow, things have wrapped around, so tail == head, and the
     * reader will detect overflow immediately instead of waiting until
     * it reads everything in the buffer, wrapping around again to the
     * point where tail == head. So the condition also checks that
     * queue->buffer[head] is zero -- if so, then the buffer is now
     * empty, and we're at the point in the msg stream where overflow
     * occurred. It's time to signal overflow to the reader. If 
     * queue->buffer[head] is non-zero, there's a message there and we
     * should read all the way around the buffer before signalling overflow.
     * There is a write-order dependency here, but to fail, the overflow
     * field would have to be written while an entire buffer full of 
     * writes are still pending. I'm assuming out-of-order writes are
     * possible, but not that many.
     */
    if (queue->overflow == head + 1 && !queue->buffer[head]) {
        queue->overflow = 0; /* non-overflow condition */
        return pmBufferOverflow;
    }

    /* test to see if there is data in the queue -- test from back
     * to front so if writer is simultaneously writing, we don't
     * waste time discovering the write is not finished 
     */
    for (i = queue->msg_size - 1; i >= 0; i--) {
        if (!queue->buffer[head + i]) {
            return pmNoData;
        }
    }
    memcpy(msg, (char *) &queue->buffer[head + 1], 
           sizeof(int32_t) * (queue->msg_size - 1));
    /* fix up zeros */
    i = queue->buffer[head];
    while (i < queue->msg_size) {
        int32_t j;
        i--; /* msg does not have extra word so shift down */
        j = msg_as_int32[i];
        msg_as_int32[i] = 0;
        i = j;
    }
    /* signal that data has been removed by zeroing: */
    bzero((char *) &queue->buffer[head], sizeof(int32_t) * queue->msg_size);

    /* update head */
    head += queue->msg_size;
    if (head == queue->len) head = 0;
    queue->head = head;
    return pmGotData; /* success */
}



PMEXPORT PmError Pm_SetOverflow(PmQueue *q)
{
    PmQueueRep *queue = (PmQueueRep *) q;
    long tail;
    /* arg checking */
    if (!queue)
        return pmBadPtr;
    /* no more enqueue until receiver acknowledges overflow */
    if (queue->overflow) return pmBufferOverflow;
    tail = queue->tail;
    queue->overflow = tail + 1;
    return pmBufferOverflow;
}


PMEXPORT PmError Pm_Enqueue(PmQueue *q, void *msg)
{
    PmQueueRep *queue = (PmQueueRep *) q;
    long tail;
    int i;
    int32_t *src = (int32_t *) msg;
    int32_t *ptr;
    int32_t *dest;
    int rslt;
    if (!queue) 
        return pmBadPtr;
    /* no more enqueue until receiver acknowledges overflow */
    if (queue->overflow) return pmBufferOverflow;
    rslt = Pm_QueueFull(q);
    /* already checked above: if (rslt == pmBadPtr) return rslt; */
    tail = queue->tail;
    if (rslt) {
        queue->overflow = tail + 1;
        return pmBufferOverflow;
    }

    /* queue is has room for message, and overflow flag is cleared */
    ptr = &queue->buffer[tail];
    dest = ptr + 1;
    for (i = 1; i < queue->msg_size; i++) {
        int32_t j = src[i - 1];
        if (!j) {
            *ptr = i;
            ptr = dest;
        } else {
            *dest = j;
        }
        dest++;
    }
    *ptr = i;
    tail += queue->msg_size;
    if (tail == queue->len) tail = 0;
    queue->tail = tail;
    return pmNoError;
}


PMEXPORT int Pm_QueueEmpty(PmQueue *q)
{
    PmQueueRep *queue = (PmQueueRep *) q;
    return (!queue) ||  /* null pointer -> return "empty" */
           (queue->buffer[queue->head] == 0 && !queue->peek_flag);
}


PMEXPORT int Pm_QueueFull(PmQueue *q)
{
    long tail;
    int i; 
    PmQueueRep *queue = (PmQueueRep *) q;
    /* arg checking */
    if (!queue)
        return pmBadPtr;
    tail = queue->tail;
    /* test to see if there is space in the queue */
    for (i = 0; i < queue->msg_size; i++) {
        if (queue->buffer[tail + i]) {
            return TRUE;
        }
    }
    return FALSE;
}


PMEXPORT void *Pm_QueuePeek(PmQueue *q)
{
    PmError rslt;
    int32_t temp;
    PmQueueRep *queue = (PmQueueRep *) q;
    /* arg checking */
    if (!queue)
        return NULL;

    if (queue->peek_flag) {
        return queue->peek;
    }
    /* this is ugly: if peek_overflow is set, then Pm_Dequeue() 
     * returns immediately with pmBufferOverflow, but here, we
     * want Pm_Dequeue() to really check for data. If data is
     * there, we can return it
     */
    temp = queue->peek_overflow;
    queue->peek_overflow = FALSE;
    rslt = Pm_Dequeue(q, queue->peek);
    queue->peek_overflow = temp;

    if (rslt == 1) {
        queue->peek_flag = TRUE;
        return queue->peek;
    } else if (rslt == pmBufferOverflow) {
        /* when overflow is indicated, the queue is empty and the 
         * first message that was dropped by Enqueue (signalling
         * pmBufferOverflow to its caller) would have been the next
         * message in the queue. Pm_QueuePeek will return NULL, but
         * remember that an overflow occurred. (see Pm_Dequeue)
         */
        queue->peek_overflow = TRUE;
    }
    return NULL;
}
#ifdef _MSC_VER
 #pragma warning(disable: 4244) // stop warnings about downsize typecasts
 #pragma warning(disable: 4018) // stop warnings about signed/unsigned
#endif

// (amalg) #include "stdlib.h"
// (amalg) #include "string.h"
// (amalg) #include "portmidi.h"
// (amalg) #include "porttime.h"
// (amalg) #include "pmutil.h"
// (amalg) #include "pminternal.h"
#include <assert.h>

#define MIDI_CLOCK      0xf8
#define MIDI_ACTIVE     0xfe
#define MIDI_STATUS_MASK 0x80
#define MIDI_SYSEX      0xf0
#define MIDI_EOX        0xf7
#define MIDI_START      0xFA
#define MIDI_STOP       0xFC
#define MIDI_CONTINUE   0xFB
#define MIDI_F9         0xF9
#define MIDI_FD         0xFD
#define MIDI_RESET      0xFF
#define MIDI_NOTE_ON    0x90
#define MIDI_NOTE_OFF   0x80
#define MIDI_CHANNEL_AT 0xD0
#define MIDI_POLY_AT    0xA0
#define MIDI_PROGRAM    0xC0
#define MIDI_CONTROL    0xB0
#define MIDI_PITCHBEND  0xE0
#define MIDI_MTC        0xF1
#define MIDI_SONGPOS    0xF2
#define MIDI_SONGSEL    0xF3
#define MIDI_TUNE       0xF6

#define is_empty(midi) ((midi)->tail == (midi)->head)

/* this is not static so that pm_init can set it directly if
 *   (see pmmac.c:pm_init())
 */
int pm_initialized = FALSE;

int pm_hosterror;
char pm_hosterror_text[PM_HOST_ERROR_MSG_LEN];

#ifdef PM_CHECK_ERRORS

#include <stdio.h>

#define STRING_MAX 80

static void prompt_and_exit(void)
{
    char line[STRING_MAX];
    printf("type ENTER...");
    fgets(line, STRING_MAX, stdin);
    /* this will clean up open ports: */
    exit(-1);
}


static PmError pm_errmsg(PmError err)
{
    if (err == pmHostError) {
        /* it seems pointless to allocate memory and copy the string,
         * so I will do the work of Pm_GetHostErrorText directly
         */
        printf("PortMidi found host error...\n  %s\n", pm_hosterror_text);
        pm_hosterror = FALSE;
        pm_hosterror_text[0] = 0; /* clear the message */
        prompt_and_exit();
    } else if (err < 0) {
        printf("PortMidi call failed...\n  %s\n", Pm_GetErrorText(err));
        prompt_and_exit();
    }
    return err;
}
#else
#define pm_errmsg(err) err
#endif

/*
====================================================================
system implementation of portmidi interface
====================================================================
*/

int pm_descriptor_max = 0;
int pm_descriptor_index = 0;
descriptor_type descriptors = NULL;

/* pm_add_device -- describe interface/device pair to library 
 *
 * This is called at intialization time, once for each 
 * interface (e.g. DirectSound) and device (e.g. SoundBlaster 1)
 * The strings are retained but NOT COPIED, so do not destroy them!
 *
 * returns pmInvalidDeviceId if device memory is exceeded
 * otherwise returns pmNoError
 */
PmError pm_add_device(char *interf, char *name, int input, 
                      void *descriptor, pm_fns_type dictionary) {
    if (pm_descriptor_index >= pm_descriptor_max) {
        // expand descriptors
        descriptor_type new_descriptors = (descriptor_type) 
            pm_alloc(sizeof(descriptor_node) * (pm_descriptor_max + 32));
        if (!new_descriptors) return pmInsufficientMemory;
        if (descriptors) {
            memcpy(new_descriptors, descriptors, 
                   sizeof(descriptor_node) * pm_descriptor_max);
            free(descriptors);
        }
        pm_descriptor_max += 32;
        descriptors = new_descriptors;
    }
    descriptors[pm_descriptor_index].pub.interf = interf;
    descriptors[pm_descriptor_index].pub.name = name;
    descriptors[pm_descriptor_index].pub.input = input;
    descriptors[pm_descriptor_index].pub.output = !input;

    /* default state: nothing to close (for automatic device closing) */
    descriptors[pm_descriptor_index].pub.opened = FALSE;

    /* ID number passed to win32 multimedia API open */
    descriptors[pm_descriptor_index].descriptor = descriptor;
    
    /* points to PmInternal, allows automatic device closing */
    descriptors[pm_descriptor_index].internalDescriptor = NULL;

    descriptors[pm_descriptor_index].dictionary = dictionary;
    
    pm_descriptor_index++;
    
    return pmNoError;
}


/* utility to look up device, given a pattern, 
   note: pattern is modified
 */
int pm_find_default_device(char *pattern, int is_input)
{
    int id = pmNoDevice;
    int i;
    /* first parse pattern into name, interf parts */
    char *interf_pref = ""; /* initially assume it is not there */
    char *name_pref = strstr(pattern, ", ");

    if (name_pref) { /* found separator, adjust the pointer */
        interf_pref = pattern;
        name_pref[0] = 0;
        name_pref += 2;
    } else {
        name_pref = pattern; /* whole string is the name pattern */
    }
    for (i = 0; i < pm_descriptor_index; i++) {
        const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
        if (info->input == is_input &&
            strstr(info->name, name_pref) &&
            strstr(info->interf, interf_pref)) {
            id = i;
            break;
        }
    }    
    return id;
}


/*
====================================================================
portmidi implementation
====================================================================
*/

PMEXPORT int Pm_CountDevices( void ) {
    Pm_Initialize();
    /* no error checking -- Pm_Initialize() does not fail */
    return pm_descriptor_index;
}


PMEXPORT const PmDeviceInfo* Pm_GetDeviceInfo( PmDeviceID id ) {
    Pm_Initialize(); /* no error check needed */
    if (id >= 0 && id < pm_descriptor_index) {
        return &descriptors[id].pub;
    }
    return NULL;
}

/* pm_success_fn -- "noop" function pointer */
PmError pm_success_fn(PmInternal *midi) {
    return pmNoError;
}

/* none_write -- returns an error if called */
PmError none_write_short(PmInternal *midi, PmEvent *buffer) {
    return pmBadPtr;
}

/* pm_fail_timestamp_fn -- placeholder for begin_sysex and flush */
PmError pm_fail_timestamp_fn(PmInternal *midi, PmTimestamp timestamp) {
    return pmBadPtr;
}

PmError none_write_byte(PmInternal *midi, unsigned char byte, 
                        PmTimestamp timestamp) {
    return pmBadPtr;
}

/* pm_fail_fn -- generic function, returns error if called */
PmError pm_fail_fn(PmInternal *midi) {
    return pmBadPtr;
}

static PmError none_open(PmInternal *midi, void *driverInfo) {
    return pmBadPtr;
}
static void none_get_host_error(PmInternal * midi, char * msg, unsigned int len) {
    *msg = 0; // empty string
}
static unsigned int none_has_host_error(PmInternal * midi) {
    return FALSE;
}
PmTimestamp none_synchronize(PmInternal *midi) {
    return 0;
}

#define none_abort pm_fail_fn
#define none_close pm_fail_fn

pm_fns_node pm_none_dictionary = {
    none_write_short,
    none_sysex,
    none_sysex,
    none_write_byte,
    none_write_short,
    none_write_flush,
    none_synchronize,
    none_open,
    none_abort, 
    none_close,
    none_poll,
    none_has_host_error,
    none_get_host_error 
};


PMEXPORT const char *Pm_GetErrorText( PmError errnum ) {
    const char *msg;

    switch(errnum)
    {
    case pmNoError:                  
        msg = ""; 
        break;
    case pmHostError:                
        msg = "PortMidi: `Host error'"; 
        break;
    case pmInvalidDeviceId:          
        msg = "PortMidi: `Invalid device ID'"; 
        break;
    case pmInsufficientMemory:       
        msg = "PortMidi: `Insufficient memory'"; 
        break;
    case pmBufferTooSmall:           
        msg = "PortMidi: `Buffer too small'"; 
        break;
    case pmBadPtr:                   
        msg = "PortMidi: `Bad pointer'"; 
        break;
    case pmInternalError:            
        msg = "PortMidi: `Internal PortMidi Error'"; 
        break;
    case pmBufferOverflow:
        msg = "PortMidi: `Buffer overflow'";
        break;
    case pmBadData:
        msg = "PortMidi: `Invalid MIDI message Data'";
        break;
    case pmBufferMaxSize:
        msg = "PortMidi: `Buffer cannot be made larger'";
        break;
    default:                         
        msg = "PortMidi: `Illegal error number'"; 
        break;
    }
    return msg;
}


/* This can be called whenever you get a pmHostError return value.
 * The error will always be in the global pm_hosterror_text.
 */
PMEXPORT void Pm_GetHostErrorText(char * msg, unsigned int len) {
    assert(msg);
    assert(len > 0);
    if (pm_hosterror) {
        strncpy(msg, (char *) pm_hosterror_text, len);
        pm_hosterror = FALSE;
        pm_hosterror_text[0] = 0; /* clear the message; not necessary, but it
                                     might help with debugging */
        msg[len - 1] = 0; /* make sure string is terminated */
    } else {
        msg[0] = 0; /* no string to return */
    }
}


PMEXPORT int Pm_HasHostError(PortMidiStream * stream) {
    if (pm_hosterror) return TRUE;
    if (stream) {
        PmInternal * midi = (PmInternal *) stream;
        pm_hosterror = (*midi->dictionary->has_host_error)(midi);
        if (pm_hosterror) {
            midi->dictionary->host_error(midi, pm_hosterror_text, 
                                         PM_HOST_ERROR_MSG_LEN);
            /* now error message is global */
            return TRUE;
        }
    }
    return FALSE;
}


PMEXPORT PmError Pm_Initialize( void ) {
    if (!pm_initialized) {
        pm_hosterror = FALSE;
        pm_hosterror_text[0] = 0; /* the null string */
        pm_init();
        pm_initialized = TRUE;
    }
    return pmNoError;
}


PMEXPORT PmError Pm_Terminate( void ) {
    if (pm_initialized) {
        pm_term();
        // if there are no devices, descriptors might still be NULL
        if (descriptors != NULL) {
            free(descriptors);
            descriptors = NULL;
        }
        pm_descriptor_index = 0;
        pm_descriptor_max = 0;
        pm_initialized = FALSE;
    }
    return pmNoError;
}


/* Pm_Read -- read up to length messages from source into buffer */
/*
 * returns number of messages actually read, or error code
 */
PMEXPORT int Pm_Read(PortMidiStream *stream, PmEvent *buffer, int32_t length) {
    PmInternal *midi = (PmInternal *) stream;
    int n = 0;
    PmError err = pmNoError;
    pm_hosterror = FALSE;
    /* arg checking */
    if(midi == NULL)
        err = pmBadPtr;
    else if(!descriptors[midi->device_id].pub.opened)
        err = pmBadPtr;
    else if(!descriptors[midi->device_id].pub.input)
        err = pmBadPtr;    
    /* First poll for data in the buffer...
     * This either simply checks for data, or attempts first to fill the buffer
     * with data from the MIDI hardware; this depends on the implementation.
     * We could call Pm_Poll here, but that would redo a lot of redundant
     * parameter checking, so I copied some code from Pm_Poll to here: */
    else err = (*(midi->dictionary->poll))(midi);

    if (err != pmNoError) {
        if (err == pmHostError) {
            midi->dictionary->host_error(midi, pm_hosterror_text, 
                                         PM_HOST_ERROR_MSG_LEN);
          pm_hosterror = TRUE;
        }
        return pm_errmsg(err);
    }

    while (n < length) {
        PmError err = Pm_Dequeue(midi->queue, buffer++);
        if (err == pmBufferOverflow) {
            /* ignore the data we have retreived so far */
            return pm_errmsg(pmBufferOverflow);
        } else if (err == 0) { /* empty queue */
            break;
        }
        n++;
    }
    return n;
}

PMEXPORT PmError Pm_Poll( PortMidiStream *stream )
{
    PmInternal *midi = (PmInternal *) stream;
    PmError err;

    pm_hosterror = FALSE;
    /* arg checking */
    if(midi == NULL)
        err = pmBadPtr;
    else if (!descriptors[midi->device_id].pub.opened)
        err = pmBadPtr;
    else if (!descriptors[midi->device_id].pub.input)
        err = pmBadPtr;
    else
        err = (*(midi->dictionary->poll))(midi);

    if (err != pmNoError) {
        if (err == pmHostError) {
            midi->dictionary->host_error(midi, pm_hosterror_text, 
                                         PM_HOST_ERROR_MSG_LEN);
           pm_hosterror = TRUE;
        }
        return pm_errmsg(err);
    }

    return (PmError) !Pm_QueueEmpty(midi->queue);
}


/* this is called from Pm_Write and Pm_WriteSysEx to issue a
 * call to the system-dependent end_sysex function and handle 
 * the error return
 */
static PmError pm_end_sysex(PmInternal *midi)
{
    PmError err = (*midi->dictionary->end_sysex)(midi, 0);
    midi->sysex_in_progress = FALSE;
    if (err == pmHostError) {
        midi->dictionary->host_error(midi, pm_hosterror_text, 
                                     PM_HOST_ERROR_MSG_LEN);
        pm_hosterror = TRUE;
    }
    return err;
}


/* to facilitate correct error-handling, Pm_Write, Pm_WriteShort, and
   Pm_WriteSysEx all operate a state machine that "outputs" calls to
   write_short, begin_sysex, write_byte, end_sysex, and write_realtime */

PMEXPORT PmError Pm_Write( PortMidiStream *stream, PmEvent *buffer, int32_t length)
{
    PmInternal *midi = (PmInternal *) stream;
    PmError err = pmNoError;
    int i;
    int bits;
    
    pm_hosterror = FALSE;
    /* arg checking */
    if(midi == NULL)
        err = pmBadPtr;
    else if(!descriptors[midi->device_id].pub.opened)
        err = pmBadPtr;
    else if(!descriptors[midi->device_id].pub.output)
        err = pmBadPtr;
    else
        err = pmNoError;
    
    if (err != pmNoError) goto pm_write_error;
    
    if (midi->latency == 0) {
        midi->now = 0;
    } else {
        midi->now = (*(midi->time_proc))(midi->time_info);
        if (midi->first_message || midi->sync_time + 100 /*ms*/ < midi->now) {
            /* time to resync */
            midi->now = (*midi->dictionary->synchronize)(midi);
            midi->first_message = FALSE;
        }
    }
    /* error recovery: when a sysex is detected, we call
     *   dictionary->begin_sysex() followed by calls to
     *   dictionary->write_byte() and dictionary->write_realtime()
     *   until an end-of-sysex is detected, when we call
     *   dictionary->end_sysex(). After an error occurs, 
     *   Pm_Write() continues to call functions. For example,
     *   it will continue to call write_byte() even after
     *   an error sending a sysex message, and end_sysex() will be
     *   called when an EOX or non-real-time status is found.
     * When errors are detected, Pm_Write() returns immediately, 
     *   so it is possible that this will drop data and leave
     *   sysex messages in a partially transmitted state.
     */
    for (i = 0; i < length; i++) {
        uint32_t msg = buffer[i].message;
        bits = 0;
        /* is this a sysex message? */
        if (Pm_MessageStatus(msg) == MIDI_SYSEX) {
            if (midi->sysex_in_progress) {
                /* error: previous sysex was not terminated by EOX */
                midi->sysex_in_progress = FALSE;
                err = pmBadData;
                goto pm_write_error;
            }
            midi->sysex_in_progress = TRUE;
            if ((err = (*midi->dictionary->begin_sysex)(midi, 
                               buffer[i].timestamp)) != pmNoError)
                goto pm_write_error;
            if ((err = (*midi->dictionary->write_byte)(midi, MIDI_SYSEX,
                               buffer[i].timestamp)) != pmNoError) 
                goto pm_write_error;
            bits = 8;
            /* fall through to continue sysex processing */
        } else if ((msg & MIDI_STATUS_MASK) && 
                   (Pm_MessageStatus(msg) != MIDI_EOX)) {
            /* a non-sysex message */
            if (midi->sysex_in_progress) {
                /* this should be a realtime message */
                if (is_real_time(msg)) {
                    if ((err = (*midi->dictionary->write_realtime)(midi, 
                                       &(buffer[i]))) != pmNoError)
                        goto pm_write_error;
                } else {
                    midi->sysex_in_progress = FALSE;
                    err = pmBadData;
                    /* ignore any error from this, because we already have one */
                    /* pass 0 as timestamp -- it's ignored */
                    (*midi->dictionary->end_sysex)(midi, 0);
                    goto pm_write_error;
                }
            } else { /* regular short midi message */
                if ((err = (*midi->dictionary->write_short)(midi, 
                                   &(buffer[i]))) != pmNoError)
                    goto pm_write_error;
                continue;
            }
        }
        if (midi->sysex_in_progress) { /* send sysex bytes until EOX */
            /* see if we can accelerate data transfer */
            if (bits == 0 && midi->fill_base && /* 4 bytes to copy */
                (*midi->fill_offset_ptr) + 4 <= midi->fill_length &&
                (msg & 0x80808080) == 0) { /* all data */
                    /* copy 4 bytes from msg to fill_base + fill_offset */
                    unsigned char *ptr = midi->fill_base + 
                                         *(midi->fill_offset_ptr);
                    ptr[0] = msg; ptr[1] = msg >> 8; 
                    ptr[2] = msg >> 16; ptr[3] = msg >> 24;
                    (*midi->fill_offset_ptr) += 4;
                     continue;
            }
            /* no acceleration, so do byte-by-byte copying */
            while (bits < 32) {
                unsigned char midi_byte = (unsigned char) (msg >> bits);
                if ((err = (*midi->dictionary->write_byte)(midi, midi_byte, 
                                   buffer[i].timestamp)) != pmNoError)
                    goto pm_write_error;
                if (midi_byte == MIDI_EOX) {
                    err = pm_end_sysex(midi);
                    if (err != pmNoError) goto error_exit;
                    break; /* from while loop */
                }
                bits += 8;
            }
        } else {
            /* not in sysex mode, but message did not start with status */
            err = pmBadData;
            goto pm_write_error;
        }
    }
    /* after all messages are processed, send the data */
    if (!midi->sysex_in_progress)
        err = (*midi->dictionary->write_flush)(midi, 0);
pm_write_error:
    if (err == pmHostError) {
        midi->dictionary->host_error(midi, pm_hosterror_text, 
                                     PM_HOST_ERROR_MSG_LEN);
        pm_hosterror = TRUE;
    }
error_exit:
    return pm_errmsg(err);
}


PMEXPORT PmError Pm_WriteShort(PortMidiStream *stream, PmTimestamp when, PmMessage msg)
{
    PmEvent event;
    
    event.timestamp = when;
    event.message = msg;
    return Pm_Write(stream, &event, 1);
}


PMEXPORT PmError Pm_WriteSysEx(PortMidiStream *stream, PmTimestamp when, 
                      unsigned char *msg)
{
    /* allocate buffer space for PM_DEFAULT_SYSEX_BUFFER_SIZE bytes */
    /* each PmEvent holds sizeof(PmMessage) bytes of sysex data */
    #define BUFLEN ((int) (PM_DEFAULT_SYSEX_BUFFER_SIZE / sizeof(PmMessage)))
    PmEvent buffer[BUFLEN];
    int buffer_size = 1; /* first time, send 1. After that, it's BUFLEN */
    PmInternal *midi = (PmInternal *) stream;
    /* the next byte in the buffer is represented by an index, bufx, and
       a shift in bits */
    int shift = 0;
    int bufx = 0;
    buffer[0].message = 0;
    buffer[0].timestamp = when;

    while (1) {
        /* insert next byte into buffer */
        buffer[bufx].message |= ((*msg) << shift);
        shift += 8;
        if (*msg++ == MIDI_EOX) break;
        if (shift == 32) {
            shift = 0;
            bufx++;
            if (bufx == buffer_size) {
                PmError err = Pm_Write(stream, buffer, buffer_size);
                /* note: Pm_Write has already called errmsg() */
                if (err) return err;
                /* prepare to fill another buffer */
                bufx = 0;
                buffer_size = BUFLEN;
                /* optimization: maybe we can just copy bytes */
                if (midi->fill_base) {
                    PmError err;
                    while (*(midi->fill_offset_ptr) < midi->fill_length) {
                        midi->fill_base[(*midi->fill_offset_ptr)++] = *msg;
                        if (*msg++ == MIDI_EOX) {
                            err = pm_end_sysex(midi);
                            if (err != pmNoError) return pm_errmsg(err);
                            goto end_of_sysex;
                        }
                    }
                    /* I thought that I could do a pm_Write here and
                     * change this if to a loop, avoiding calls in Pm_Write
                     * to the slower write_byte, but since 
                     * sysex_in_progress is true, this will not flush
                     * the buffer, and we'll infinite loop: */
                    /* err = Pm_Write(stream, buffer, 0);
                       if (err) return err; */
                    /* instead, the way this works is that Pm_Write calls
                     * write_byte on 4 bytes. The first, since the buffer
                     * is full, will flush the buffer and allocate a new
                     * one. This primes the buffer so
                     * that we can return to the loop above and fill it
                     * efficiently without a lot of function calls.
                     */
                    buffer_size = 1; /* get another message started */
                }
            }
            buffer[bufx].message = 0;
            buffer[bufx].timestamp = when;
        } 
        /* keep inserting bytes until you find MIDI_EOX */
    }
end_of_sysex:
    /* we're finished sending full buffers, but there may
     * be a partial one left.
     */
    if (shift != 0) bufx++; /* add partial message to buffer len */
    if (bufx) { /* bufx is number of PmEvents to send from buffer */
        PmError err = Pm_Write(stream, buffer, bufx);
        if (err) return err;
    }
    return pmNoError;
}



PMEXPORT PmError Pm_OpenInput(PortMidiStream** stream,
                     PmDeviceID inputDevice,
                     void *inputDriverInfo,
                     int32_t bufferSize,
                     PmTimeProcPtr time_proc,
                     void *time_info)
{
    PmInternal *midi;
    PmError err = pmNoError;
    pm_hosterror = FALSE;
    *stream = NULL;
    
    /* arg checking */
    if (inputDevice < 0 || inputDevice >= pm_descriptor_index) 
        err = pmInvalidDeviceId;
    else if (!descriptors[inputDevice].pub.input) 
        err =  pmInvalidDeviceId;
    else if(descriptors[inputDevice].pub.opened)
        err =  pmInvalidDeviceId;
    
    if (err != pmNoError) 
        goto error_return;

    /* create portMidi internal data */
    midi = (PmInternal *) pm_alloc(sizeof(PmInternal)); 
    *stream = midi;
    if (!midi) {
        err = pmInsufficientMemory;
        goto error_return;
    }
    midi->device_id = inputDevice;
    midi->write_flag = FALSE;
    midi->time_proc = time_proc;
    midi->time_info = time_info;
    /* windows adds timestamps in the driver and these are more accurate than
       using a time_proc, so do not automatically provide a time proc. Non-win
       implementations may want to provide a default time_proc in their
       system-specific midi_out_open() method.
     */
    if (bufferSize <= 0) bufferSize = 256; /* default buffer size */
    midi->queue = Pm_QueueCreate(bufferSize, (int32_t) sizeof(PmEvent));
    if (!midi->queue) {
        /* free portMidi data */
        *stream = NULL;
        pm_free(midi); 
        err = pmInsufficientMemory;
        goto error_return;
    }
    midi->buffer_len = bufferSize; /* portMidi input storage */
    midi->latency = 0; /* not used */
    midi->sysex_in_progress = FALSE;
    midi->sysex_message = 0; 
    midi->sysex_message_count = 0; 
    midi->filters = PM_FILT_ACTIVE;
    midi->channel_mask = 0xFFFF;
    midi->sync_time = 0;
    midi->first_message = TRUE;
    midi->dictionary = descriptors[inputDevice].dictionary;
    midi->fill_base = NULL;
    midi->fill_offset_ptr = NULL;
    midi->fill_length = 0;
    descriptors[inputDevice].internalDescriptor = midi;
    /* open system dependent input device */
    err = (*midi->dictionary->open)(midi, inputDriverInfo);
    if (err) {
        *stream = NULL;
        descriptors[inputDevice].internalDescriptor = NULL;
        /* free portMidi data */
        Pm_QueueDestroy(midi->queue);
        pm_free(midi);
    } else {
        /* portMidi input open successful */
        descriptors[inputDevice].pub.opened = TRUE;
    }
error_return:
    /* note: if there is a pmHostError, it is the responsibility
     * of the system-dependent code (*midi->dictionary->open)()
     * to set pm_hosterror and pm_hosterror_text
     */
    return pm_errmsg(err);
}


PMEXPORT PmError Pm_OpenOutput(PortMidiStream** stream,
                      PmDeviceID outputDevice,
                      void *outputDriverInfo,
                      int32_t bufferSize,
                      PmTimeProcPtr time_proc,
                      void *time_info,
                      int32_t latency ) 
{
    PmInternal *midi;
    PmError err = pmNoError;
    pm_hosterror = FALSE;
    *stream =  NULL;
    
    /* arg checking */
    if (outputDevice < 0 || outputDevice >= pm_descriptor_index)
        err = pmInvalidDeviceId;
    else if (!descriptors[outputDevice].pub.output) 
        err = pmInvalidDeviceId;
    else if (descriptors[outputDevice].pub.opened)
        err = pmInvalidDeviceId;
    if (err != pmNoError) 
        goto error_return;

    /* create portMidi internal data */
    midi = (PmInternal *) pm_alloc(sizeof(PmInternal)); 
    *stream = midi;                 
    if (!midi) {
        err = pmInsufficientMemory;
        goto error_return;
    }
    midi->device_id = outputDevice;
    midi->write_flag = TRUE;
    midi->time_proc = time_proc;
    /* if latency > 0, we need a time reference. If none is provided,
       use PortTime library */
    if (time_proc == NULL && latency != 0) {
        if (!Pt_Started()) 
            Pt_Start(1, 0, 0);
        /* time_get does not take a parameter, so coerce */
        midi->time_proc = (PmTimeProcPtr) Pt_Time;
    }
    midi->time_info = time_info;
    midi->buffer_len = bufferSize;
    midi->queue = NULL; /* unused by output */
    /* if latency zero, output immediate (timestamps ignored) */
    /* if latency < 0, use 0 but don't return an error */
    if (latency < 0) latency = 0;
    midi->latency = latency;
    midi->sysex_in_progress = FALSE;
    midi->sysex_message = 0; /* unused by output */
    midi->sysex_message_count = 0; /* unused by output */
    midi->filters = 0; /* not used for output */
    midi->channel_mask = 0xFFFF;
    midi->sync_time = 0;
    midi->first_message = TRUE;
    midi->dictionary = descriptors[outputDevice].dictionary;
    midi->fill_base = NULL;
    midi->fill_offset_ptr = NULL;
    midi->fill_length = 0;
    descriptors[outputDevice].internalDescriptor = midi;
    /* open system dependent output device */
    err = (*midi->dictionary->open)(midi, outputDriverInfo);
    if (err) {
        *stream = NULL;
        descriptors[outputDevice].internalDescriptor = NULL;
        /* free portMidi data */
        pm_free(midi); 
    } else {
        /* portMidi input open successful */
        descriptors[outputDevice].pub.opened = TRUE;
    }
error_return:
    /* note: system-dependent code must set pm_hosterror and
     * pm_hosterror_text if a pmHostError occurs
     */
    return pm_errmsg(err);
}


PMEXPORT PmError Pm_SetChannelMask(PortMidiStream *stream, int mask)
{
    PmInternal *midi = (PmInternal *) stream;
    PmError err = pmNoError;

    if (midi == NULL)
        err = pmBadPtr;
    else
        midi->channel_mask = mask;

    return pm_errmsg(err);
}


PMEXPORT PmError Pm_SetFilter(PortMidiStream *stream, int32_t filters) {
    PmInternal *midi = (PmInternal *) stream;
    PmError err = pmNoError;

    /* arg checking */
    if (midi == NULL)
        err = pmBadPtr;
    else if (!descriptors[midi->device_id].pub.opened)
        err = pmBadPtr;
    else
        midi->filters = filters;
    return pm_errmsg(err);
}


PMEXPORT PmError Pm_Close( PortMidiStream *stream ) {
    PmInternal *midi = (PmInternal *) stream;
    PmError err = pmNoError;

    pm_hosterror = FALSE;
    /* arg checking */
    if (midi == NULL) /* midi must point to something */
        err = pmBadPtr;
    /* if it is an open device, the device_id will be valid */
    else if (midi->device_id < 0 || midi->device_id >= pm_descriptor_index)
        err = pmBadPtr;
    /* and the device should be in the opened state */
    else if (!descriptors[midi->device_id].pub.opened)
        err = pmBadPtr;
    
    if (err != pmNoError) 
        goto error_return;

    /* close the device */
    err = (*midi->dictionary->close)(midi);
    /* even if an error occurred, continue with cleanup */
    descriptors[midi->device_id].internalDescriptor = NULL;
    descriptors[midi->device_id].pub.opened = FALSE;
    if (midi->queue) Pm_QueueDestroy(midi->queue);
    pm_free(midi); 
error_return:
    /* system dependent code must set pm_hosterror and
     * pm_hosterror_text if a pmHostError occurs.
     */
    return pm_errmsg(err);
}

PmError Pm_Synchronize( PortMidiStream* stream ) {
    PmInternal *midi = (PmInternal *) stream;
    PmError err = pmNoError;
    if (midi == NULL)
        err = pmBadPtr;
    else if (!descriptors[midi->device_id].pub.output)
        err = pmBadPtr;
    else if (!descriptors[midi->device_id].pub.opened)
        err = pmBadPtr;
    else
        midi->first_message = TRUE;
    return err;
}

PMEXPORT PmError Pm_Abort( PortMidiStream* stream ) {
    PmInternal *midi = (PmInternal *) stream;
    PmError err;
    /* arg checking */
    if (midi == NULL)
        err = pmBadPtr;
    else if (!descriptors[midi->device_id].pub.output)
        err = pmBadPtr;
    else if (!descriptors[midi->device_id].pub.opened)
        err = pmBadPtr;
    else
        err = (*midi->dictionary->abort)(midi);

    if (err == pmHostError) {
        midi->dictionary->host_error(midi, pm_hosterror_text, 
                                     PM_HOST_ERROR_MSG_LEN);
        pm_hosterror = TRUE;
    }
    return pm_errmsg(err);
}



/* pm_channel_filtered returns non-zero if the channel mask is blocking the current channel */
#define pm_channel_filtered(status, mask) \
    ((((status) & 0xF0) != 0xF0) && (!(Pm_Channel((status) & 0x0F) & (mask))))


/* The following two functions will checks to see if a MIDI message matches
   the filtering criteria.  Since the sysex routines only want to filter realtime messages,
   we need to have separate routines.
 */


/* pm_realtime_filtered returns non-zero if the filter will kill the current message.
   Note that only realtime messages are checked here.
 */
#define pm_realtime_filtered(status, filters) \
    ((((status) & 0xF0) == 0xF0) && ((1 << ((status) & 0xF)) & (filters)))

/*
    return ((status == MIDI_ACTIVE) && (filters & PM_FILT_ACTIVE))
            ||  ((status == MIDI_CLOCK) && (filters & PM_FILT_CLOCK))
            ||  ((status == MIDI_START) && (filters & PM_FILT_PLAY))
            ||  ((status == MIDI_STOP) && (filters & PM_FILT_PLAY))
            ||  ((status == MIDI_CONTINUE) && (filters & PM_FILT_PLAY))
            ||  ((status == MIDI_F9) && (filters & PM_FILT_F9))
            ||  ((status == MIDI_FD) && (filters & PM_FILT_FD))
            ||  ((status == MIDI_RESET) && (filters & PM_FILT_RESET))
            ||  ((status == MIDI_MTC) && (filters & PM_FILT_MTC))
            ||  ((status == MIDI_SONGPOS) && (filters & PM_FILT_SONG_POSITION))
            ||  ((status == MIDI_SONGSEL) && (filters & PM_FILT_SONG_SELECT))
            ||  ((status == MIDI_TUNE) && (filters & PM_FILT_TUNE));
}*/


/* pm_status_filtered returns non-zero if a filter will kill the current message, based on status.
   Note that sysex and real time are not checked.  It is up to the subsystem (winmm, core midi, alsa)
   to filter sysex, as it is handled more easily and efficiently at that level.
   Realtime message are filtered in pm_realtime_filtered.
 */
#define pm_status_filtered(status, filters) ((1 << (16 + ((status) >> 4))) & (filters))


/*
    return  ((status == MIDI_NOTE_ON) && (filters & PM_FILT_NOTE))
            ||  ((status == MIDI_NOTE_OFF) && (filters & PM_FILT_NOTE))
            ||  ((status == MIDI_CHANNEL_AT) && (filters & PM_FILT_CHANNEL_AFTERTOUCH))
            ||  ((status == MIDI_POLY_AT) && (filters & PM_FILT_POLY_AFTERTOUCH))
            ||  ((status == MIDI_PROGRAM) && (filters & PM_FILT_PROGRAM))
            ||  ((status == MIDI_CONTROL) && (filters & PM_FILT_CONTROL))
            ||  ((status == MIDI_PITCHBEND) && (filters & PM_FILT_PITCHBEND));

}
*/

static void pm_flush_sysex(PmInternal *midi, PmTimestamp timestamp)
{
    PmEvent event;
    
    /* there may be nothing in the buffer */
    if (midi->sysex_message_count == 0) return; /* nothing to flush */
    
    event.message = midi->sysex_message;
    event.timestamp = timestamp;
    /* copied from pm_read_short, avoids filtering */
    if (Pm_Enqueue(midi->queue, &event) == pmBufferOverflow) {
        midi->sysex_in_progress = FALSE;
    }
    midi->sysex_message_count = 0;
    midi->sysex_message = 0;
}


/* pm_read_short and pm_read_bytes
   are the interface between system-dependent MIDI input handlers
   and the system-independent PortMIDI code.
   The input handler MUST obey these rules:
   1) all short input messages must be sent to pm_read_short, which
      enqueues them to a FIFO for the application.
   2) each buffer of sysex bytes should be reported by calling pm_read_bytes
      (which sets midi->sysex_in_progress). After the eox byte, 
      pm_read_bytes will clear sysex_in_progress
 */

/* pm_read_short is the place where all input messages arrive from 
   system-dependent code such as pmwinmm.c. Here, the messages
   are entered into the PortMidi input buffer. 
 */
void pm_read_short(PmInternal *midi, PmEvent *event)
{ 
    int status;
    /* arg checking */
    assert(midi != NULL);
    /* midi filtering is applied here */
    status = Pm_MessageStatus(event->message);
    if (!pm_status_filtered(status, midi->filters)
        && (!is_real_time(status) || 
            !pm_realtime_filtered(status, midi->filters))
        && !pm_channel_filtered(status, midi->channel_mask)) {
        /* if sysex is in progress and we get a status byte, it had
           better be a realtime message or the starting SYSEX byte;
           otherwise, we exit the sysex_in_progress state
         */
        if (midi->sysex_in_progress && (status & MIDI_STATUS_MASK)) {
            /* two choices: real-time or not. If it's real-time, then
             * this should be delivered as a sysex byte because it is
             * embedded in a sysex message
             */
            if (is_real_time(status)) {
                midi->sysex_message |= 
                        (status << (8 * midi->sysex_message_count++));
                if (midi->sysex_message_count == 4) {
                    pm_flush_sysex(midi, event->timestamp);
                }
            } else { /* otherwise, it's not real-time. This interrupts
                      * a sysex message in progress */
                midi->sysex_in_progress = FALSE;
            }
        } else if (Pm_Enqueue(midi->queue, event) == pmBufferOverflow) {
            midi->sysex_in_progress = FALSE;
        }
    }
}

/* pm_read_bytes -- read one (partial) sysex msg from MIDI data */
/*
 * returns how many bytes processed
 */
unsigned int pm_read_bytes(PmInternal *midi, const unsigned char *data, 
                    int len, PmTimestamp timestamp)
{
    int i = 0; /* index into data, must not be unsigned (!) */
    PmEvent event;
    event.timestamp = timestamp;
    assert(midi);
    /* note that since buffers may not have multiples of 4 bytes,
     * pm_read_bytes may be called in the middle of an outgoing
     * 4-byte PortMidi message. sysex_in_progress indicates that
     * a sysex has been sent but no eox.
     */
    if (len == 0) return 0; /* sanity check */
    if (!midi->sysex_in_progress) {
        while (i < len) { /* process all data */
            unsigned char byte = data[i++];
            if (byte == MIDI_SYSEX &&
                !pm_realtime_filtered(byte, midi->filters)) {
                midi->sysex_in_progress = TRUE;
                i--; /* back up so code below will get SYSEX byte */
                break; /* continue looping below to process msg */
            } else if (byte == MIDI_EOX) {
                midi->sysex_in_progress = FALSE;
                return i; /* done with one message */
            } else if (byte & MIDI_STATUS_MASK) {
                /* We're getting MIDI but no sysex in progress.
                 * Either the SYSEX status byte was dropped or
                 * the message was filtered. Drop the data, but
                 * send any embedded realtime bytes.
                 */
                /* assume that this is a real-time message:
                 * it is an error to pass non-real-time messages
                 * to pm_read_bytes
                 */
                event.message = byte;
                pm_read_short(midi, &event);
            }
        } /* all bytes in the buffer are processed */
    }
    /* Now, i<len implies sysex_in_progress. If sysex_in_progress
     * becomes false in the loop, there must have been an overflow
     * and we can just drop all remaining bytes 
     */
    while (i < len && midi->sysex_in_progress) {
        if (midi->sysex_message_count == 0 && i <= len - 4 &&
            ((event.message = (((PmMessage) data[i]) | 
                             (((PmMessage) data[i+1]) << 8) |
                             (((PmMessage) data[i+2]) << 16) |
                             (((PmMessage) data[i+3]) << 24))) &
             0x80808080) == 0) { /* all data, no status */ 
            if (Pm_Enqueue(midi->queue, &event) == pmBufferOverflow) {
                midi->sysex_in_progress = FALSE;
            }
            i += 4;
        } else {
            while (i < len) {
                /* send one byte at a time */
                unsigned char byte = data[i++];
                if (is_real_time(byte) && 
                    pm_realtime_filtered(byte, midi->filters)) {
                    continue; /* real-time data is filtered, so omit */
                }
                midi->sysex_message |= 
                    (byte << (8 * midi->sysex_message_count++));
                if (byte == MIDI_EOX) {
                    midi->sysex_in_progress = FALSE;
                    pm_flush_sysex(midi, event.timestamp);
                    return i;
                } else if (midi->sysex_message_count == 4) {
                    pm_flush_sysex(midi, event.timestamp);
                    /* after handling at least one non-data byte
                     * and reaching a 4-byte message boundary,
                     * resume trying to send 4 at a time in outer loop
                     */
                    break;
                }
            }
        }
    }
    return i;
}
/* porttime.c -- portable API for millisecond timer */

/* There is no machine-independent implementation code to put here */
#if __linux__
#include <sys/timeb.h>
#include <sys/time.h>
#include <sys/resource.h>
/* ptlinux.c -- portable timer implementation for linux */


/* IMPLEMENTATION NOTES (by Mark Nelson): 

Unlike Windows, Linux has no system call to request a periodic callback,
so if Pt_Start() receives a callback parameter, it must create a thread
that wakes up periodically and calls the provided callback function.
If running as superuser, use setpriority() to renice thread to -20.  
One could also set the timer thread to a real-time priority (SCHED_FIFO
and SCHED_RR), but this is dangerous for This is necessary because  
if the callback hangs it'll never return. A more serious reason
is that the current scheduler implementation busy-waits instead 
of sleeping when realtime threads request a sleep of <=2ms (as a way 
to get around the 10ms granularity), which means the thread would never 
let anyone else on the CPU.

CHANGE LOG

18-Jul-03 Roger Dannenberg -- Simplified code to set priority of timer
            thread. Simplified implementation notes. 

*/
/* stdlib, stdio, unistd, and sys/types were added because they appeared
 * in a Gentoo patch, but I'm not sure why they are needed. -RBD
 */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
// (amalg) #include "porttime.h"
// (amalg) #include "sys/time.h"
// (amalg) #include "sys/resource.h"
// (amalg) #include "sys/timeb.h"
// (amalg) #include "pthread.h"

#define TRUE 1
#define FALSE 0

static int time_started_flag = FALSE;
static struct timeb time_offset = {0, 0, 0, 0};
static pthread_t pt_thread_pid;
static int pt_thread_created = FALSE;

/* note that this is static data -- we only need one copy */
typedef struct {
    int id;
    int resolution;
    PtCallback *callback;
    void *userData;
} pt_callback_parameters;

static int pt_callback_proc_id = 0;

static void *Pt_CallbackProc(void *p)
{
    pt_callback_parameters *parameters = (pt_callback_parameters *) p;
    int mytime = 1;
    /* to kill a process, just increment the pt_callback_proc_id */
    /* printf("pt_callback_proc_id %d, id %d\n", pt_callback_proc_id,
           parameters->id); */
    if (geteuid() == 0) setpriority(PRIO_PROCESS, 0, -20);
    while (pt_callback_proc_id == parameters->id) {
        /* wait for a multiple of resolution ms */
        struct timeval timeout;
        int delay = mytime++ * parameters->resolution - Pt_Time();
        if (delay < 0) delay = 0;
        timeout.tv_sec = 0;
        timeout.tv_usec = delay * 1000;
        select(0, NULL, NULL, NULL, &timeout);
        (*(parameters->callback))(Pt_Time(), parameters->userData);
    }
    /* printf("Pt_CallbackProc exiting\n"); */
    // free(parameters);
    return NULL;
}


PtError Pt_Start(int resolution, PtCallback *callback, void *userData)
{
    if (time_started_flag) return ptNoError;
    ftime(&time_offset); /* need this set before process runs */
    if (callback) {
        int res;
        pt_callback_parameters *parms = (pt_callback_parameters *) 
            malloc(sizeof(pt_callback_parameters));
        if (!parms) return ptInsufficientMemory;
        parms->id = pt_callback_proc_id;
        parms->resolution = resolution;
        parms->callback = callback;
        parms->userData = userData;
        res = pthread_create(&pt_thread_pid, NULL, 
                             Pt_CallbackProc, parms);
        if (res != 0) return ptHostError;
        pt_thread_created = TRUE;
    }
    time_started_flag = TRUE;
    return ptNoError;
}


PtError Pt_Stop()
{
    /* printf("Pt_Stop called\n"); */
    pt_callback_proc_id++;
    if (pt_thread_created) {
        pthread_join(pt_thread_pid, NULL);
        pt_thread_created = FALSE;
    }
    time_started_flag = FALSE;
    return ptNoError;
}


int Pt_Started()
{
    return time_started_flag;
}


PtTimestamp Pt_Time()
{
    long seconds, milliseconds;
    struct timeb now;
    ftime(&now);
    seconds = now.time - time_offset.time;
    milliseconds = now.millitm - time_offset.millitm;
    return seconds * 1000 + milliseconds;
}


void Pt_Sleep(int32_t duration)
{
    usleep(duration * 1000);
}



/* pmlinux.c -- PortMidi os-dependent code */

/* This file only needs to implement pm_init(), which calls various
   routines to register the available midi devices. This file must
   be separate from the main portmidi.c file because it is system
   dependent, and it is separate from, pmlinuxalsa.c, because it
   might need to register non-alsa devices as well.

   NOTE: if you add non-ALSA support, you need to fix :alsa_poll()
   in pmlinuxalsa.c, which assumes all input devices are ALSA.
 */

// (amalg) #include "stdlib.h"
// (amalg) #include "portmidi.h"
// (amalg) #include "pmutil.h"
// (amalg) #include "pminternal.h"

#ifdef PMALSA
  // (amalg) #include "pmlinuxalsa.h"
#endif

#ifdef PMNULL
  // (amalg) #include "pmlinuxnull.h"
#endif

PmDeviceID pm_default_input_device_id = -1;
PmDeviceID pm_default_output_device_id = -1;

void pm_init()
{
    /* Note: it is not an error for PMALSA to fail to initialize. 
     * It may be a design error that the client cannot query what subsystems
     * are working properly other than by looking at the list of available
     * devices.
     */
    #ifdef PMALSA
	pm_linuxalsa_init();
    #endif
    #ifdef PMNULL
        pm_linuxnull_init();
    #endif
    // this is set when we return to Pm_Initialize, but we need it
    // now in order to (successfully) call Pm_CountDevices()
    pm_initialized = TRUE;      
    pm_default_input_device_id = find_default_device(
        "/PortMidi/PM_RECOMMENDED_INPUT_DEVICE", TRUE,
        pm_default_input_device_id);
    pm_default_output_device_id = find_default_device(
        "/PortMidi/PM_RECOMMENDED_OUTPUT_DEVICE", FALSE,
        pm_default_output_device_id);
}

void pm_term(void)
{
    #ifdef PMALSA
        pm_linuxalsa_term();
    #endif
}

PmDeviceID Pm_GetDefaultInputDeviceID() { 
    Pm_Initialize();
    return pm_default_input_device_id; 
}

PmDeviceID Pm_GetDefaultOutputDeviceID() { 
    Pm_Initialize();
    return pm_default_output_device_id; 
}

void *pm_alloc(size_t s) { return malloc(s); }

void pm_free(void *ptr) { free(ptr); }

/*
 * pmlinuxalsa.c -- system specific definitions
 * 
 * written by:
 *  Roger Dannenberg (port to Alsa 0.9.x)
 *  Clemens Ladisch (provided code examples and invaluable consulting)
 *  Jason Cohen, Rico Colon, Matt Filippone (Alsa 0.5.x implementation)
 */ 

// (amalg) #include "stdlib.h"
// (amalg) #include "portmidi.h"
// (amalg) #include "pmutil.h"
// (amalg) #include "pminternal.h"
// (amalg) #include "pmlinuxalsa.h"
// (amalg) #include "string.h"
// (amalg) #include "porttime.h"
// (amalg) #include "pmlinux.h"

#include <alsa/asoundlib.h>

/* I used many print statements to debug this code. I left them in the
 * source, and you can turn them on by changing false to true below:
 */
#define VERBOSE_ON 0
#define VERBOSE if (VERBOSE_ON)

#define MIDI_SYSEX      0xf0
#define MIDI_EOX        0xf7

#if SND_LIB_MAJOR == 0 && SND_LIB_MINOR < 9
#error needs ALSA 0.9.0 or later
#endif

/* to store client/port in the device descriptor */
#define MAKE_DESCRIPTOR(client, port) ((void*)(((client) << 8) | (port)))
#define GET_DESCRIPTOR_CLIENT(info) ((((int)(info)) >> 8) & 0xff)
#define GET_DESCRIPTOR_PORT(info) (((int)(info)) & 0xff)

#define BYTE unsigned char

extern pm_fns_node pm_linuxalsa_in_dictionary;
extern pm_fns_node pm_linuxalsa_out_dictionary;

static snd_seq_t *seq = NULL; // all input comes here, 
                              // output queue allocated on seq
static int queue, queue_used; /* one for all ports, reference counted */

typedef struct alsa_descriptor_struct {
    int client;
    int port;
    int this_port;
    int in_sysex;
    snd_midi_event_t *parser;
    int error; /* host error code */
} alsa_descriptor_node, *alsa_descriptor_type;


/* get_alsa_error_text -- copy error text to potentially short string */
/**/
static void get_alsa_error_text(char *msg, int len, int err)
{
    int errlen = strlen(snd_strerror(err));
    if (errlen < len) {
        strcpy(msg, snd_strerror(err));
    } else if (len > 20) {
        sprintf(msg, "Alsa error %d", err);
    } else if (len > 4) {
        strcpy(msg, "Alsa");
    } else {
        msg[0] = 0;
    }
}


/* queue is shared by both input and output, reference counted */
static PmError alsa_use_queue(void)
{
    if (queue_used == 0) {
        snd_seq_queue_tempo_t *tempo;

        queue = snd_seq_alloc_queue(seq);
        if (queue < 0) {
            pm_hosterror = queue;
            return pmHostError;
        }
        snd_seq_queue_tempo_alloca(&tempo);
        snd_seq_queue_tempo_set_tempo(tempo, 480000);
        snd_seq_queue_tempo_set_ppq(tempo, 480);
        pm_hosterror = snd_seq_set_queue_tempo(seq, queue, tempo);
        if (pm_hosterror < 0)
            return pmHostError;

        snd_seq_start_queue(seq, queue, NULL);
        snd_seq_drain_output(seq);
    }
    ++queue_used;
    return pmNoError;
}


static void alsa_unuse_queue(void)
{
    if (--queue_used == 0) {
        snd_seq_stop_queue(seq, queue, NULL);
        snd_seq_drain_output(seq);
        snd_seq_free_queue(seq, queue);
        VERBOSE printf("queue freed\n");
    }
}


/* midi_message_length -- how many bytes in a message? */
static int midi_message_length(PmMessage message)
{
    message &= 0xff;
    if (message < 0x80) {
        return 0;
    } else if (message < 0xf0) {
        static const int length[] = {3, 3, 3, 3, 2, 2, 3};
        return length[(message - 0x80) >> 4];
    } else {
        static const int length[] = {
            -1, 2, 3, 2, 0, 0, 1, -1, 1, 0, 1, 1, 1, 0, 1, 1};
        return length[message - 0xf0];
    }
}


static PmError alsa_out_open(PmInternal *midi, void *driverInfo) 
{
    void *client_port = descriptors[midi->device_id].descriptor;
    alsa_descriptor_type desc = (alsa_descriptor_type) 
        pm_alloc(sizeof(alsa_descriptor_node));
    snd_seq_port_info_t *info;
    int err;

    if (!desc) return pmInsufficientMemory;
    
    snd_seq_port_info_alloca(&info);
    snd_seq_port_info_set_port(info, midi->device_id);
    snd_seq_port_info_set_capability(info, SND_SEQ_PORT_CAP_WRITE |
                                     SND_SEQ_PORT_CAP_READ);
    snd_seq_port_info_set_type(info, SND_SEQ_PORT_TYPE_MIDI_GENERIC | 
                                     SND_SEQ_PORT_TYPE_APPLICATION);
    snd_seq_port_info_set_port_specified(info, 1);
    err = snd_seq_create_port(seq, info);
    if (err < 0) goto free_desc;

    /* fill in fields of desc, which is passed to pm_write routines */
    midi->descriptor = desc;
    desc->client = GET_DESCRIPTOR_CLIENT(client_port);
    desc->port = GET_DESCRIPTOR_PORT(client_port);
    desc->this_port = midi->device_id;
    desc->in_sysex = 0;

    desc->error = 0;

    err = snd_midi_event_new(PM_DEFAULT_SYSEX_BUFFER_SIZE, &desc->parser);
    if (err < 0) goto free_this_port;

    if (midi->latency > 0) { /* must delay output using a queue */
        err = alsa_use_queue();
        if (err < 0) goto free_parser;

        err = snd_seq_connect_to(seq, desc->this_port, desc->client, desc->port);
        if (err < 0) goto unuse_queue;  /* clean up and return on error */
    } else {
        err = snd_seq_connect_to(seq, desc->this_port, desc->client, desc->port);
        if (err < 0) goto free_parser;  /* clean up and return on error */
    }        
    return pmNoError;

 unuse_queue:
    alsa_unuse_queue();
 free_parser:
    snd_midi_event_free(desc->parser);
 free_this_port:
    snd_seq_delete_port(seq, desc->this_port);
 free_desc:
    pm_free(desc);
    pm_hosterror = err;
    if (err < 0) {
        get_alsa_error_text(pm_hosterror_text, PM_HOST_ERROR_MSG_LEN, err);
    }
    return pmHostError;
}
    

static PmError alsa_write_byte(PmInternal *midi, unsigned char byte, 
                        PmTimestamp timestamp)
{
    alsa_descriptor_type desc = (alsa_descriptor_type) midi->descriptor;
    snd_seq_event_t ev;
    int err;

    snd_seq_ev_clear(&ev);
    if (snd_midi_event_encode_byte(desc->parser, byte, &ev) == 1) {
        snd_seq_ev_set_dest(&ev, desc->client, desc->port);
        snd_seq_ev_set_source(&ev, desc->this_port);
        if (midi->latency > 0) {
            /* compute relative time of event = timestamp - now + latency */
            PmTimestamp now = (midi->time_proc ? 
                               midi->time_proc(midi->time_info) : 
                               Pt_Time(NULL));
            int when = timestamp;
            /* if timestamp is zero, send immediately */
            /* otherwise compute time delay and use delay if positive */
            if (when == 0) when = now;
            when = (when - now) + midi->latency;
            if (when < 0) when = 0;
            VERBOSE printf("timestamp %d now %d latency %d, ", 
                           (int) timestamp, (int) now, midi->latency);
            VERBOSE printf("scheduling event after %d\n", when);
            /* message is sent in relative ticks, where 1 tick = 1 ms */
            snd_seq_ev_schedule_tick(&ev, queue, 1, when);
            /* NOTE: for cases where the user does not supply a time function,
               we could optimize the code by not starting Pt_Time and using
               the alsa tick time instead. I didn't do this because it would
               entail changing the queue management to start the queue tick
               count when PortMidi is initialized and keep it running until
               PortMidi is terminated. (This should be simple, but it's not
               how the code works now.) -RBD */
        } else { /* send event out without queueing */
            VERBOSE printf("direct\n");
            /* ev.queue = SND_SEQ_QUEUE_DIRECT;
               ev.dest.client = SND_SEQ_ADDRESS_SUBSCRIBERS; */
            snd_seq_ev_set_direct(&ev);
        }
        VERBOSE printf("sending event\n");
        err = snd_seq_event_output(seq, &ev);
        if (err < 0) {
            desc->error = err;
            return pmHostError;
        }
    }
    return pmNoError;
}


static PmError alsa_out_close(PmInternal *midi)
{
    alsa_descriptor_type desc = (alsa_descriptor_type) midi->descriptor;
    if (!desc) return pmBadPtr;

    if ((pm_hosterror = snd_seq_disconnect_to(seq, desc->this_port, 
                                              desc->client, desc->port))) {
        // if there's an error, try to delete the port anyway, but don't
        // change the pm_hosterror value so we retain the first error
        snd_seq_delete_port(seq, desc->this_port);
    } else { // if there's no error, delete the port and retain any error
        pm_hosterror = snd_seq_delete_port(seq, desc->this_port);
    }
    if (midi->latency > 0) alsa_unuse_queue();
    snd_midi_event_free(desc->parser);
    midi->descriptor = NULL; /* destroy the pointer to signify "closed" */
    pm_free(desc);
    if (pm_hosterror) {
        get_alsa_error_text(pm_hosterror_text, PM_HOST_ERROR_MSG_LEN, 
                            pm_hosterror);
        return pmHostError;
    }
    return pmNoError;
}


static PmError alsa_in_open(PmInternal *midi, void *driverInfo)
{
    void *client_port = descriptors[midi->device_id].descriptor;
    alsa_descriptor_type desc = (alsa_descriptor_type) 
        pm_alloc(sizeof(alsa_descriptor_node));
    snd_seq_port_info_t *info;
    snd_seq_port_subscribe_t *sub;
    snd_seq_addr_t addr;
    int err;

    if (!desc) return pmInsufficientMemory;
    
    err = alsa_use_queue();
    if (err < 0) goto free_desc;

    snd_seq_port_info_alloca(&info);
    snd_seq_port_info_set_port(info, midi->device_id);
    snd_seq_port_info_set_capability(info, SND_SEQ_PORT_CAP_WRITE |
                                     SND_SEQ_PORT_CAP_READ);
    snd_seq_port_info_set_type(info, SND_SEQ_PORT_TYPE_MIDI_GENERIC | 
                                     SND_SEQ_PORT_TYPE_APPLICATION);
    snd_seq_port_info_set_port_specified(info, 1);
    err = snd_seq_create_port(seq, info);
    if (err < 0) goto free_queue;

    /* fill in fields of desc, which is passed to pm_write routines */
    midi->descriptor = desc;
    desc->client = GET_DESCRIPTOR_CLIENT(client_port);
    desc->port = GET_DESCRIPTOR_PORT(client_port);
    desc->this_port = midi->device_id;
    desc->in_sysex = 0;

    desc->error = 0;

    VERBOSE printf("snd_seq_connect_from: %d %d %d\n", 
                   desc->this_port, desc->client, desc->port);
    snd_seq_port_subscribe_alloca(&sub);
    addr.client = snd_seq_client_id(seq);
    addr.port = desc->this_port;
    snd_seq_port_subscribe_set_dest(sub, &addr);
    addr.client = desc->client;
    addr.port = desc->port;
    snd_seq_port_subscribe_set_sender(sub, &addr);
    snd_seq_port_subscribe_set_time_update(sub, 1);
    /* this doesn't seem to work: messages come in with real timestamps */
    snd_seq_port_subscribe_set_time_real(sub, 0);
    err = snd_seq_subscribe_port(seq, sub);
    /* err = 
       snd_seq_connect_from(seq, desc->this_port, desc->client, desc->port); */
    if (err < 0) goto free_this_port;  /* clean up and return on error */
    return pmNoError;

 free_this_port:
    snd_seq_delete_port(seq, desc->this_port);
 free_queue:
    alsa_unuse_queue();
 free_desc:
    pm_free(desc);
    pm_hosterror = err;
    if (err < 0) {
        get_alsa_error_text(pm_hosterror_text, PM_HOST_ERROR_MSG_LEN, err);
    }
    return pmHostError;
}

static PmError alsa_in_close(PmInternal *midi)
{
    alsa_descriptor_type desc = (alsa_descriptor_type) midi->descriptor;
    if (!desc) return pmBadPtr;
    if ((pm_hosterror = snd_seq_disconnect_from(seq, desc->this_port, 
                                                desc->client, desc->port))) {
        snd_seq_delete_port(seq, desc->this_port); /* try to close port */
    } else {
        pm_hosterror = snd_seq_delete_port(seq, desc->this_port);
    }
    alsa_unuse_queue();
    midi->descriptor = NULL;
    pm_free(desc);
    if (pm_hosterror) {
        get_alsa_error_text(pm_hosterror_text, PM_HOST_ERROR_MSG_LEN, 
                            pm_hosterror);
        return pmHostError;
    }
    return pmNoError;
}
        

static PmError alsa_abort(PmInternal *midi)
{
    /* NOTE: ALSA documentation is vague. This is supposed to 
     * remove any pending output messages. If you can test and 
     * confirm this code is correct, please update this comment. -RBD
     */
    /* Unfortunately, I can't even compile it -- my ALSA version 
     * does not implement snd_seq_remove_events_t, so this does
     * not compile. I'll try again, but it looks like I'll need to
     * upgrade my entire Linux OS -RBD
     */
    /*
    alsa_descriptor_type desc = (alsa_descriptor_type) midi->descriptor;
    snd_seq_remove_events_t info;
    snd_seq_addr_t addr;
    addr.client = desc->client;
    addr.port = desc->port;
    snd_seq_remove_events_set_dest(&info, &addr);
    snd_seq_remove_events_set_condition(&info, SND_SEQ_REMOVE_DEST);
    pm_hosterror = snd_seq_remove_events(seq, &info);
    if (pm_hosterror) {
        get_alsa_error_text(pm_hosterror_text, PM_HOST_ERROR_MSG_LEN, 
                            pm_hosterror);
        return pmHostError;
    }
    */
    printf("WARNING: alsa_abort not implemented\n");
    return pmNoError;
}


#ifdef GARBAGE
This is old code here temporarily for reference
static PmError alsa_write(PmInternal *midi, PmEvent *buffer, int32_t length)
{
    alsa_descriptor_type desc = (alsa_descriptor_type) midi->descriptor;
    int i, bytes;
    unsigned char byte;
    PmMessage msg;

    desc->error = 0;
    for (; length > 0; length--, buffer++) {
        VERBOSE printf("message 0x%x\n", buffer->message);
        if (Pm_MessageStatus(buffer->message) == MIDI_SYSEX)
            desc->in_sysex = TRUE;
        if (desc->in_sysex) {
            msg = buffer->message;
            for (i = 0; i < 4; i++) {
                byte = msg;  /* extract next byte to send */
                alsa_write_byte(midi, byte, buffer->timestamp);
                if (byte == MIDI_EOX) {
                    desc->in_sysex = FALSE;
                    break;
                }
                if (desc->error < 0) break;
                msg >>= 8; /* shift next byte into position */
            }
        } else {
            bytes = midi_message_length(buffer->message);
            msg = buffer->message;
            for (i = 0; i < bytes; i++) {
                byte = msg; /* extract next byte to send */
                VERBOSE printf("sending 0x%x\n", byte);
                alsa_write_byte(midi, byte, buffer->timestamp);
                if (desc->error < 0) break;
                msg >>= 8; /* shift next byte into position */
            }
        }
    }
    if (desc->error < 0) return pmHostError;

    VERBOSE printf("snd_seq_drain_output: 0x%x\n", (unsigned int) seq);
    desc->error = snd_seq_drain_output(seq);
    if (desc->error < 0) return pmHostError;

    desc->error = pmNoError;
    return pmNoError;
}
#endif


static PmError alsa_write_flush(PmInternal *midi, PmTimestamp timestamp)
{
    alsa_descriptor_type desc = (alsa_descriptor_type) midi->descriptor;
    if (!desc) return pmBadPtr;
    VERBOSE printf("snd_seq_drain_output: 0x%x\n", (unsigned int) seq);
    desc->error = snd_seq_drain_output(seq);
    if (desc->error < 0) return pmHostError;

    desc->error = pmNoError;
    return pmNoError;
}


static PmError alsa_write_short(PmInternal *midi, PmEvent *event)
{
    int bytes = midi_message_length(event->message);
    PmMessage msg = event->message;
    int i;
    alsa_descriptor_type desc = (alsa_descriptor_type) midi->descriptor;
    if (!desc) return pmBadPtr;
    for (i = 0; i < bytes; i++) {
        unsigned char byte = msg;
        VERBOSE printf("sending 0x%x\n", byte);
        alsa_write_byte(midi, byte, event->timestamp);
        if (desc->error < 0) break;
        msg >>= 8; /* shift next byte into position */
    }
    if (desc->error < 0) return pmHostError;
    desc->error = pmNoError;
    return pmNoError;
}


/* alsa_sysex -- implements begin_sysex and end_sysex */
PmError alsa_sysex(PmInternal *midi, PmTimestamp timestamp) {
    return pmNoError;
}


static PmTimestamp alsa_synchronize(PmInternal *midi)
{
    return 0; /* linux implementation does not use this synchronize function */
    /* Apparently, Alsa data is relative to the time you send it, and there
       is no reference. If this is true, this is a serious shortcoming of
       Alsa. If not true, then PortMidi has a serious shortcoming -- it 
       should be scheduling relative to Alsa's time reference. */
}


static void handle_event(snd_seq_event_t *ev)
{
    int device_id = ev->dest.port;
    PmInternal *midi = descriptors[device_id].internalDescriptor;
    // There is a race condition when closing a device and
    // continuing to poll other open devices. The closed device may
    // have outstanding events from before the close operation.
    if (!midi) {
        return;
    }
    PmEvent pm_ev;
    PmTimeProcPtr time_proc = midi->time_proc;
    PmTimestamp timestamp;

    /* time stamp should be in ticks, using our queue where 1 tick = 1ms */
    assert((ev->flags & SND_SEQ_TIME_STAMP_MASK) == SND_SEQ_TIME_STAMP_TICK);

    /* if no time_proc, just return "native" ticks (ms) */
    if (time_proc == NULL) {
        timestamp = ev->time.tick;
    } else { /* translate time to time_proc basis */
        snd_seq_queue_status_t *queue_status;
        snd_seq_queue_status_alloca(&queue_status);
        snd_seq_get_queue_status(seq, queue, queue_status);
        /* return (now - alsa_now) + alsa_timestamp */
        timestamp = (*time_proc)(midi->time_info) + ev->time.tick -
                    snd_seq_queue_status_get_tick_time(queue_status);
    }
    pm_ev.timestamp = timestamp;
    switch (ev->type) {
    case SND_SEQ_EVENT_NOTEON:
        pm_ev.message = Pm_Message(0x90 | ev->data.note.channel,
                                   ev->data.note.note & 0x7f,
                                   ev->data.note.velocity & 0x7f);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_NOTEOFF:
        pm_ev.message = Pm_Message(0x80 | ev->data.note.channel,
                                   ev->data.note.note & 0x7f,
                                   ev->data.note.velocity & 0x7f);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_KEYPRESS:
        pm_ev.message = Pm_Message(0xa0 | ev->data.note.channel,
                                   ev->data.note.note & 0x7f,
                                   ev->data.note.velocity & 0x7f);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_CONTROLLER:
        pm_ev.message = Pm_Message(0xb0 | ev->data.note.channel,
                                   ev->data.control.param & 0x7f,
                                   ev->data.control.value & 0x7f);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_PGMCHANGE:
        pm_ev.message = Pm_Message(0xc0 | ev->data.note.channel,
                                   ev->data.control.value & 0x7f, 0);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_CHANPRESS:
        pm_ev.message = Pm_Message(0xd0 | ev->data.note.channel,
                                   ev->data.control.value & 0x7f, 0);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_PITCHBEND:
        pm_ev.message = Pm_Message(0xe0 | ev->data.note.channel,
                            (ev->data.control.value + 0x2000) & 0x7f,
                            ((ev->data.control.value + 0x2000) >> 7) & 0x7f);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_CONTROL14:
        if (ev->data.control.param < 0x20) {
            pm_ev.message = Pm_Message(0xb0 | ev->data.note.channel,
                                       ev->data.control.param,
                                       (ev->data.control.value >> 7) & 0x7f);
            pm_read_short(midi, &pm_ev);
            pm_ev.message = Pm_Message(0xb0 | ev->data.note.channel,
                                       ev->data.control.param + 0x20,
                                       ev->data.control.value & 0x7f);
            pm_read_short(midi, &pm_ev);
        } else {
            pm_ev.message = Pm_Message(0xb0 | ev->data.note.channel,
                                       ev->data.control.param & 0x7f,
                                       ev->data.control.value & 0x7f);

            pm_read_short(midi, &pm_ev);
        }
        break;
    case SND_SEQ_EVENT_SONGPOS:
        pm_ev.message = Pm_Message(0xf2,
                                   ev->data.control.value & 0x7f,
                                   (ev->data.control.value >> 7) & 0x7f);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_SONGSEL:
        pm_ev.message = Pm_Message(0xf3,
                                   ev->data.control.value & 0x7f, 0);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_QFRAME:
        pm_ev.message = Pm_Message(0xf1,
                                   ev->data.control.value & 0x7f, 0);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_START:
        pm_ev.message = Pm_Message(0xfa, 0, 0);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_CONTINUE:
        pm_ev.message = Pm_Message(0xfb, 0, 0);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_STOP:
        pm_ev.message = Pm_Message(0xfc, 0, 0);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_CLOCK:
        pm_ev.message = Pm_Message(0xf8, 0, 0);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_TUNE_REQUEST:
        pm_ev.message = Pm_Message(0xf6, 0, 0);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_RESET:
        pm_ev.message = Pm_Message(0xff, 0, 0);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_SENSING:
        pm_ev.message = Pm_Message(0xfe, 0, 0);
        pm_read_short(midi, &pm_ev);
        break;
    case SND_SEQ_EVENT_SYSEX: {
        const BYTE *ptr = (const BYTE *) ev->data.ext.ptr;
        /* assume there is one sysex byte to process */
        pm_read_bytes(midi, ptr, ev->data.ext.len, timestamp);
        break;
    }
    }
}


static PmError alsa_poll(PmInternal *midi)
{
    snd_seq_event_t *ev;
    /* expensive check for input data, gets data from device: */
    while (snd_seq_event_input_pending(seq, TRUE) > 0) {
        /* cheap check on local input buffer */
        while (snd_seq_event_input_pending(seq, FALSE) > 0) {
            /* check for and ignore errors, e.g. input overflow */
            /* note: if there's overflow, this should be reported
             * all the way through to client. Since input from all
             * devices is merged, we need to find all input devices
             * and set all to the overflow state.
             * NOTE: this assumes every input is ALSA based.
             */
            int rslt = snd_seq_event_input(seq, &ev);
            if (rslt >= 0) {
                handle_event(ev);
            } else if (rslt == -ENOSPC) {
                int i;
                for (i = 0; i < pm_descriptor_index; i++) {
                    if (descriptors[i].pub.input) {
                        PmInternal *midi = (PmInternal *) 
                                descriptors[i].internalDescriptor;
                        /* careful, device may not be open! */
                        if (midi) Pm_SetOverflow(midi->queue);
                    }
                }
            }
        }
    }
    return pmNoError;
}


static unsigned int alsa_has_host_error(PmInternal *midi)
{
    alsa_descriptor_type desc = (alsa_descriptor_type) midi->descriptor;
    if (!desc) return 0;
    return desc->error;
}


static void alsa_get_host_error(PmInternal *midi, char *msg, unsigned int len)
{
    alsa_descriptor_type desc = (alsa_descriptor_type) midi->descriptor;
    if (!desc) return;
    int err = (pm_hosterror || desc->error);
    get_alsa_error_text(msg, len, err);
}


pm_fns_node pm_linuxalsa_in_dictionary = {
    none_write_short,
    none_sysex,
    none_sysex,
    none_write_byte,
    none_write_short,
    none_write_flush,
    alsa_synchronize,
    alsa_in_open,
    alsa_abort,
    alsa_in_close,
    alsa_poll,
    alsa_has_host_error,
    alsa_get_host_error
};

pm_fns_node pm_linuxalsa_out_dictionary = {
    alsa_write_short,
    alsa_sysex,
    alsa_sysex,
    alsa_write_byte,
    alsa_write_short, /* short realtime message */
    alsa_write_flush,
    alsa_synchronize,
    alsa_out_open, 
    alsa_abort, 
    alsa_out_close,
    none_poll,
    alsa_has_host_error,
    alsa_get_host_error
};


/* pm_strdup -- copy a string to the heap. Use this rather than strdup so 
 *    that we call pm_alloc, not malloc. This allows portmidi to avoid 
 *    malloc which might cause priority inversion. Probably ALSA is going
 *    to call malloc anyway, so this extra work here may be pointless.
 */
char *pm_strdup(const char *s)
{
    int len = strlen(s);
    char *dup = (char *) pm_alloc(len + 1);
    strcpy(dup, s);
    return dup;
}


PmError pm_linuxalsa_init( void )
{
    int  err;
    snd_seq_client_info_t *cinfo;
    snd_seq_port_info_t *pinfo;
    unsigned int caps;

    /* Previously, the last parameter was SND_SEQ_NONBLOCK, but this 
     * would cause messages to be dropped if the ALSA buffer fills up.
     * The correct behavior is for writes to block until there is 
     * room to send all the data. The client should normally allocate
     * a large enough buffer to avoid blocking on output. 
     * Now that blocking is enabled, the seq_event_input() will block
     * if there is no input data. This is not what we want, so must
     * call seq_event_input_pending() to avoid blocking.
     */
    err = snd_seq_open(&seq, "default", SND_SEQ_OPEN_DUPLEX, 0);
    if (err < 0) return err;
    
    snd_seq_client_info_alloca(&cinfo);
    snd_seq_port_info_alloca(&pinfo);

    snd_seq_client_info_set_client(cinfo, -1);
    while (snd_seq_query_next_client(seq, cinfo) == 0) {
        snd_seq_port_info_set_client(pinfo, snd_seq_client_info_get_client(cinfo));
        snd_seq_port_info_set_port(pinfo, -1);
        while (snd_seq_query_next_port(seq, pinfo) == 0) {
            if (snd_seq_port_info_get_client(pinfo) == SND_SEQ_CLIENT_SYSTEM)
                continue; /* ignore Timer and Announce ports on client 0 */
            caps = snd_seq_port_info_get_capability(pinfo);
            if (!(caps & (SND_SEQ_PORT_CAP_SUBS_READ | SND_SEQ_PORT_CAP_SUBS_WRITE)))
                continue; /* ignore if you cannot read or write port */
            if (caps & SND_SEQ_PORT_CAP_SUBS_WRITE) {
                if (pm_default_output_device_id == -1) 
                    pm_default_output_device_id = pm_descriptor_index;
                pm_add_device("ALSA",
                              pm_strdup(snd_seq_port_info_get_name(pinfo)),
                              FALSE,
                              MAKE_DESCRIPTOR(snd_seq_port_info_get_client(pinfo),
                                              snd_seq_port_info_get_port(pinfo)),
                              &pm_linuxalsa_out_dictionary);
            }
            if (caps & SND_SEQ_PORT_CAP_SUBS_READ) {
                if (pm_default_input_device_id == -1) 
                    pm_default_input_device_id = pm_descriptor_index;
                pm_add_device("ALSA",
                              pm_strdup(snd_seq_port_info_get_name(pinfo)),
                              TRUE,
                              MAKE_DESCRIPTOR(snd_seq_port_info_get_client(pinfo),
                                              snd_seq_port_info_get_port(pinfo)),
                              &pm_linuxalsa_in_dictionary);
            }
        }
    }
    return pmNoError;
}
    

void pm_linuxalsa_term(void)
{
    if (seq) {
        snd_seq_close(seq);
        pm_free(descriptors);
        descriptors = NULL;
        pm_descriptor_index = 0;
        pm_descriptor_max = 0;
    }
}
/* finddefault.c -- find_default_device() implementation
   Roger Dannenberg, Jan 2009
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
// (amalg) #include "portmidi.h"

#define STRING_MAX 256

/* skip over spaces, return first non-space */
void skip_spaces(FILE *inf)
{
    char c;
    while (isspace(c = getc(inf))) ;
    ungetc(c, inf);
}

/* trim leading spaces and match a string */
int match_string(FILE *inf, char *s)
{
    skip_spaces(inf);
    while (*s && *s == getc(inf)) s++;
    return (*s == 0);
} 


/* 
/* Parse preference files, find default device, search devices --
 */
PmDeviceID find_default_device(char *path, int input, PmDeviceID id)
/* path -- the name of the preference we are searching for
   input -- true iff this is an input device
   id -- current default device id
   returns matching device id if found, otherwise id
*/
{
    static char *pref_2 = "/.java/.userPrefs/";
    static char *pref_3 = "prefs.xml";
    char *pref_1 = getenv("HOME");
    char *full_name, *path_ptr;
    FILE *inf;
    int c, i;
    if (!pref_1) goto nopref; // cannot find preference file
    // full_name will be larger than necessary
    full_name  = malloc(strlen(pref_1) + strlen(pref_2) + strlen(pref_3) +
                        strlen(path) + 2);
    strcpy(full_name, pref_1); 
    strcat(full_name, pref_2);
    // copy all but last path segment to full_name
    if (*path == '/') path++; // skip initial slash in path
    path_ptr = strrchr(path, '/'); 
    if (path_ptr) { // copy up to slash after full_name
        path_ptr++;
        int offset = strlen(full_name);
        memcpy(full_name + offset, path, path_ptr - path);
        full_name[offset + path_ptr - path] = 0; // end of string
    } else {
        path_ptr = path;
    }
    strcat(full_name, pref_3);
    inf = fopen(full_name, "r");
    if (!inf) goto nopref; // cannot open preference file
    // We're not going to build or link in a full XML parser.
    // Instead, find the path string and quoute. Then, look for
    // "value", "=", quote. Then get string up to quote.
    while ((c = getc(inf)) != EOF) {
        char pref_str[STRING_MAX];
        if (c != '"') continue; // scan up to quote
        // look for quote string quote
        if (!match_string(inf, path_ptr)) continue; // path not found
        if (getc(inf) != '"') continue; // path not found, keep scanning
        if (!match_string(inf, "value")) goto nopref; // value not found
        if (!match_string(inf, "=")) goto nopref; // = not found
        if (!match_string(inf, "\"")) goto nopref; // quote not found
        // now read the value up to the close quote
        for (i = 0; i < STRING_MAX; i++) {
            if ((c = getc(inf)) == '"') break;
            pref_str[i] = c;
        }
        if (i == STRING_MAX) continue; // value too long, ignore
        pref_str[i] = 0;
        i = pm_find_default_device(pref_str, input);
        if (i != pmNoDevice) {
            id = i;
	}
        break;
    }
 nopref:
    return id;
}
#elif _WIN32
/* ptwinmm.c -- portable timer implementation for win32 */


// (amalg) #include "porttime.h"
// (amalg) #include "windows.h"
// (amalg) #include "time.h"


TIMECAPS caps;

static long time_offset = 0;
static int time_started_flag = FALSE;
static long time_resolution;
static MMRESULT timer_id;
static PtCallback *time_callback;

void CALLBACK winmm_time_callback(UINT uID, UINT uMsg, DWORD_PTR dwUser, 
                                  DWORD_PTR dw1, DWORD_PTR dw2)
{
    (*time_callback)(Pt_Time(), (void *) dwUser);
}
 

PMEXPORT PtError Pt_Start(int resolution, PtCallback *callback, void *userData)
{
    if (time_started_flag) return ptAlreadyStarted;
    timeBeginPeriod(resolution);
    time_resolution = resolution;
    time_offset = timeGetTime();
    time_started_flag = TRUE;
    time_callback = callback;
    if (callback) {
        timer_id = timeSetEvent(resolution, 1, winmm_time_callback, 
            (DWORD_PTR) userData, TIME_PERIODIC | TIME_CALLBACK_FUNCTION);
        if (!timer_id) return ptHostError;
    }
    return ptNoError;
}


PMEXPORT PtError Pt_Stop()
{
    if (!time_started_flag) return ptAlreadyStopped;
    if (time_callback && timer_id) {
        timeKillEvent(timer_id);
        time_callback = NULL;
        timer_id = 0;
    }
    time_started_flag = FALSE;
    timeEndPeriod(time_resolution);
    return ptNoError;
}


PMEXPORT int Pt_Started()
{
    return time_started_flag;
}


PMEXPORT PtTimestamp Pt_Time()
{
    return timeGetTime() - time_offset;
}


PMEXPORT void Pt_Sleep(int32_t duration)
{
    Sleep(duration);
}
/* pmwin.c -- PortMidi os-dependent code */

/* This file only needs to implement:
       pm_init(), which calls various routines to register the 
           available midi devices,
       Pm_GetDefaultInputDeviceID(), and
       Pm_GetDefaultOutputDeviceID().
   This file must
   be separate from the main portmidi.c file because it is system
   dependent, and it is separate from, say, pmwinmm.c, because it
   might need to register devices for winmm, directx, and others.

 */

// (amalg) #include "stdlib.h"
// (amalg) #include "portmidi.h"
// (amalg) #include "pmutil.h"
// (amalg) #include "pminternal.h"
// (amalg) #include "pmwinmm.h"
#ifdef DEBUG
// (amalg) #include "stdio.h"
#endif
#include <windows.h>

/* pm_exit is called when the program exits.
   It calls pm_term to make sure PortMidi is properly closed.
   If DEBUG is on, we prompt for input to avoid losing error messages.
 */
static void pm_exit(void) {
    pm_term();
#ifdef DEBUG
#define STRING_MAX 80
    {
        char line[STRING_MAX];
        printf("Type ENTER...\n");
        /* note, w/o this prompting, client console application can not see one
           of its errors before closing. */
        fgets(line, STRING_MAX, stdin);
    }
#endif
}


/* pm_init is the windows-dependent initialization.*/
void pm_init(void)
{
    atexit(pm_exit);
#ifdef DEBUG
    printf("registered pm_exit with atexit()\n");
#endif
    pm_winmm_init();
    /* initialize other APIs (DirectX?) here */
}


void pm_term(void) {
    pm_winmm_term();
}


static PmDeviceID pm_get_default_device_id(int is_input, char *key) {
    HKEY hkey;
#define PATTERN_MAX 256
    char pattern[PATTERN_MAX];
    long pattern_max = PATTERN_MAX;
    DWORD dwType;
    /* Find first input or device -- this is the default. */
    PmDeviceID id = pmNoDevice;
    int i, j;
    Pm_Initialize(); /* make sure descriptors exist! */
    for (i = 0; i < pm_descriptor_index; i++) {
        if (descriptors[i].pub.input == is_input) {
            id = i;
            break;
        }
    }
    /* Look in registry for a default device name pattern. */
    if (RegOpenKeyEx(HKEY_CURRENT_USER, "Software", 0, KEY_READ, &hkey) != 
        ERROR_SUCCESS) {
        return id;
    }
    if (RegOpenKeyEx(hkey, "JavaSoft", 0, KEY_READ, &hkey) !=
        ERROR_SUCCESS) {
        return id;
    }
    if (RegOpenKeyEx(hkey, "Prefs", 0, KEY_READ, &hkey) !=
        ERROR_SUCCESS) {
        return id;
    }
    if (RegOpenKeyEx(hkey, "/Port/Midi", 0, KEY_READ, &hkey) !=
        ERROR_SUCCESS) {
        return id;
    }
    if (RegQueryValueEx(hkey, key, NULL, &dwType, (BYTE *) pattern, 
                        (DWORD *) &pattern_max) != 
	ERROR_SUCCESS) {
        return id;
    }

    /* decode pattern: upper case encoded with "/" prefix */
    i = j = 0;
    while (pattern[i]) {
        if (pattern[i] == '/' && pattern[i + 1]) {
            pattern[j++] = toupper(pattern[++i]);
	} else {
            pattern[j++] = tolower(pattern[i]);
	}
        i++;
    }
    pattern[j] = 0; /* end of string */

    /* now pattern is the string from the registry; search for match */
    i = pm_find_default_device(pattern, is_input);
    if (i != pmNoDevice) {
        id = i;
    }
    return id;
}


PmDeviceID Pm_GetDefaultInputDeviceID() {
    return pm_get_default_device_id(TRUE, 
           "/P/M_/R/E/C/O/M/M/E/N/D/E/D_/I/N/P/U/T_/D/E/V/I/C/E");
}


PmDeviceID Pm_GetDefaultOutputDeviceID() {
  return pm_get_default_device_id(FALSE,
          "/P/M_/R/E/C/O/M/M/E/N/D/E/D_/O/U/T/P/U/T_/D/E/V/I/C/E");
}


// (amalg) #include "stdio.h" 

void *pm_alloc(size_t s) {
    return malloc(s); 
}


void pm_free(void *ptr) { 
    free(ptr); 
}


/* pmwinmm.c -- system specific definitions */

#ifdef _MSC_VER
 #pragma warning(disable: 4133) // stop warnings about implicit typecasts
#endif

#ifndef _WIN32_WINNT
    /* without this define, InitializeCriticalSectionAndSpinCount is 
     * undefined. This version level means "Windows 2000 and higher" 
     */
    #define _WIN32_WINNT 0x0500
#endif

// (amalg) #include "windows.h"
// (amalg) #include "mmsystem.h"
// (amalg) #include "portmidi.h"
// (amalg) #include "pmutil.h"
// (amalg) #include "pminternal.h"
// (amalg) #include "pmwinmm.h"
#include <string.h>
// (amalg) #include "porttime.h"

/* asserts used to verify portMidi code logic is sound; later may want
    something more graceful */
#include <assert.h>
#ifdef DEBUG
/* this printf stuff really important for debugging client app w/host errors.
    probably want to do something else besides read/write from/to console
    for portability, however */
#define STRING_MAX 80
// (amalg) #include "stdio.h"
#endif

#define streql(x, y) (strcmp(x, y) == 0)

#define MIDI_SYSEX      0xf0
#define MIDI_EOX        0xf7

/* callback routines */
static void CALLBACK winmm_in_callback(HMIDIIN hMidiIn,
                                       UINT wMsg, DWORD_PTR dwInstance, 
                                       DWORD_PTR dwParam1, DWORD_PTR dwParam2);
static void CALLBACK winmm_streamout_callback(HMIDIOUT hmo, UINT wMsg,
                                              DWORD_PTR dwInstance, 
                                              DWORD_PTR dwParam1,
                                              DWORD_PTR dwParam2);

extern pm_fns_node pm_winmm_in_dictionary;
extern pm_fns_node pm_winmm_out_dictionary;

static void winmm_out_delete(PmInternal *midi); /* forward reference */

/*
A note about buffers: WinMM seems to hold onto buffers longer than
one would expect, e.g. when I tried using 2 small buffers to send
long sysex messages, at some point WinMM held both buffers. This problem
was fixed by making buffers bigger. Therefore, it seems that there should 
be enough buffer space to hold a whole sysex message. 

The bufferSize passed into Pm_OpenInput (passed into here as buffer_len)
will be used to estimate the largest sysex message (= buffer_len * 4 bytes).
Call that the max_sysex_len = buffer_len * 4.

For simple midi output (latency == 0), allocate 3 buffers, each with half
the size of max_sysex_len, but each at least 256 bytes.

For stream output, there will already be enough space in very short
buffers, so use them, but make sure there are at least 16.

For input, use many small buffers rather than 2 large ones so that when 
there are short sysex messages arriving frequently (as in control surfaces)
there will be more free buffers to fill. Use max_sysex_len / 64 buffers,
but at least 16, of size 64 bytes each.

The following constants help to represent these design parameters:
*/
#define NUM_SIMPLE_SYSEX_BUFFERS 3
#define MIN_SIMPLE_SYSEX_LEN 256

#define MIN_STREAM_BUFFERS 16
#define STREAM_BUFFER_LEN 24

#define INPUT_SYSEX_LEN 64
#define MIN_INPUT_BUFFERS 16

/* if we run out of space for output (assume this is due to a sysex msg,
   expand by up to NUM_EXPANSION_BUFFERS in increments of EXPANSION_BUFFER_LEN
 */
#define NUM_EXPANSION_BUFFERS 128
#define EXPANSION_BUFFER_LEN 1024

/* A sysex buffer has 3 DWORDS as a header plus the actual message size */
#define MIDIHDR_SYSEX_BUFFER_LENGTH(x) ((x) + sizeof(long)*3)
/* A MIDIHDR with a sysex message is the buffer length plus the header size */
#define MIDIHDR_SYSEX_SIZE(x) (MIDIHDR_SYSEX_BUFFER_LENGTH(x) + sizeof(MIDIHDR))

/*
==============================================================================
win32 mmedia system specific structure passed to midi callbacks
==============================================================================
*/

/* global winmm device info */
MIDIINCAPS *midi_in_caps = NULL;
MIDIINCAPS midi_in_mapper_caps;
UINT midi_num_inputs = 0;
MIDIOUTCAPS *midi_out_caps = NULL;
MIDIOUTCAPS midi_out_mapper_caps;
UINT midi_num_outputs = 0;

/* per device info */
typedef struct midiwinmm_struct {
    union {
        HMIDISTRM stream;   /* windows handle for stream */
        HMIDIOUT out;       /* windows handle for out calls */
        HMIDIIN in;         /* windows handle for in calls */
    } handle;

    /* midi output messages are sent in these buffers, which are allocated
     * in a round-robin fashion, using next_buffer as an index
     */
    LPMIDIHDR *buffers;     /* pool of buffers for midi in or out data */
    int max_buffers;        /* length of buffers array */
    int buffers_expanded;   /* buffers array expanded for extra msgs? */
    int num_buffers;        /* how many buffers allocated in buffers array */
    int next_buffer;        /* index of next buffer to send */
    HANDLE buffer_signal;   /* used to wait for buffer to become free */
    unsigned long last_time;    /* last output time */
    int first_message;          /* flag: treat first message differently */
    int sysex_mode;             /* middle of sending sysex */
    unsigned long sysex_word;   /* accumulate data when receiving sysex */
    unsigned int sysex_byte_count; /* count how many received */
    LPMIDIHDR hdr;              /* the message accumulating sysex to send */
    unsigned long sync_time;    /* when did we last determine delta? */
    long delta;                 /* difference between stream time and
                                       real time */
    int error;                  /* host error from doing port midi call */
    CRITICAL_SECTION lock;      /* prevents reentrant callbacks (input only) */
} midiwinmm_node, *midiwinmm_type;


/*
=============================================================================
general MIDI device queries
=============================================================================
*/
static void pm_winmm_general_inputs()
{
    UINT i;
    WORD wRtn;
    midi_num_inputs = midiInGetNumDevs();
    midi_in_caps = (MIDIINCAPS *) pm_alloc(sizeof(MIDIINCAPS) * 
                                           midi_num_inputs);
    if (midi_in_caps == NULL) {
        /* if you can't open a particular system-level midi interface
         * (such as winmm), we just consider that system or API to be
         * unavailable and move on without reporting an error.
         */
        return;
    }

    for (i = 0; i < midi_num_inputs; i++) {
        wRtn = midiInGetDevCaps(i, (LPMIDIINCAPS) & midi_in_caps[i],
                                sizeof(MIDIINCAPS));
        if (wRtn == MMSYSERR_NOERROR) {
            /* ignore errors here -- if pm_descriptor_max is exceeded, some
               devices will not be accessible. */
            pm_add_device("MMSystem", midi_in_caps[i].szPname, TRUE,
                          (void *) i, &pm_winmm_in_dictionary);
        }
    }
}


static void pm_winmm_mapper_input()
{
    WORD wRtn;
    /* Note: if MIDIMAPPER opened as input (documentation implies you
        can, but current system fails to retrieve input mapper
        capabilities) then you still should retrieve some formof
        setup info. */
    wRtn = midiInGetDevCaps((UINT) MIDIMAPPER,
                            (LPMIDIINCAPS) & midi_in_mapper_caps, 
                            sizeof(MIDIINCAPS));
    if (wRtn == MMSYSERR_NOERROR) {
        pm_add_device("MMSystem", midi_in_mapper_caps.szPname, TRUE,
                      (void *) MIDIMAPPER, &pm_winmm_in_dictionary);
    }
}


static void pm_winmm_general_outputs()
{
    UINT i;
    DWORD wRtn;
    midi_num_outputs = midiOutGetNumDevs();
    midi_out_caps = pm_alloc( sizeof(MIDIOUTCAPS) * midi_num_outputs );

    if (midi_out_caps == NULL) {
        /* no error is reported -- see pm_winmm_general_inputs */
        return ;
    }

    for (i = 0; i < midi_num_outputs; i++) {
        wRtn = midiOutGetDevCaps(i, (LPMIDIOUTCAPS) & midi_out_caps[i],
                                 sizeof(MIDIOUTCAPS));
        if (wRtn == MMSYSERR_NOERROR) {
            pm_add_device("MMSystem", midi_out_caps[i].szPname, FALSE,
                          (void *) i, &pm_winmm_out_dictionary);
        }
    }
}


static void pm_winmm_mapper_output()
{
    WORD wRtn;
    /* Note: if MIDIMAPPER opened as output (pseudo MIDI device
        maps device independent messages into device dependant ones,
        via NT midimapper program) you still should get some setup info */
    wRtn = midiOutGetDevCaps((UINT) MIDIMAPPER, (LPMIDIOUTCAPS)
                             & midi_out_mapper_caps, sizeof(MIDIOUTCAPS));
    if (wRtn == MMSYSERR_NOERROR) {
        pm_add_device("MMSystem", midi_out_mapper_caps.szPname, FALSE,
                      (void *) MIDIMAPPER, &pm_winmm_out_dictionary);
    }
}


/*
=========================================================================================
host error handling
=========================================================================================
*/
static unsigned int winmm_has_host_error(PmInternal * midi)
{
    midiwinmm_type m = (midiwinmm_type)midi->descriptor;
    return m->error;
}


/* str_copy_len -- like strcat, but won't overrun the destination string */
/*
 * returns length of resulting string
 */
static int str_copy_len(char *dst, char *src, int len)
{
    // Note: Visual C will suggest using a non-portable strncpy_s here
    strncpy(dst, src, len);
    /* just in case suffex is greater then len, terminate with zero */
    dst[len - 1] = 0;
    return strlen(dst);
}


static void winmm_get_host_error(PmInternal * midi, char * msg, UINT len)
{
    /* precondition: midi != NULL */
    midiwinmm_node * m = (midiwinmm_node *) midi->descriptor;
    char *hdr1 = "Host error: ";
    char *hdr2 = "Host callback error: ";

    msg[0] = 0; /* initialize result string to empty */

    if (descriptors[midi->device_id].pub.input) {
        /* input and output use different winmm API calls */
        if (m) { /* make sure there is an open device to examine */
            if (m->error != MMSYSERR_NOERROR) {
                int n = str_copy_len(msg, hdr1, len);
                /* read and record host error */
                int err = midiInGetErrorText(m->error, msg + n, len - n);
                assert(err == MMSYSERR_NOERROR);
                m->error = MMSYSERR_NOERROR;
            }
        }
    } else { /* output port */
        if (m) {
            if (m->error != MMSYSERR_NOERROR) {
                int n = str_copy_len(msg, hdr1, len);
                int err = midiOutGetErrorText(m->error, msg + n, len - n);
                assert(err == MMSYSERR_NOERROR);
                m->error = MMSYSERR_NOERROR;
            }
        }
    }
}


/*
=============================================================================
buffer handling
=============================================================================
*/
static MIDIHDR *allocate_buffer(long data_size)
{
    LPMIDIHDR hdr = (LPMIDIHDR) pm_alloc(MIDIHDR_SYSEX_SIZE(data_size));
    MIDIEVENT *evt;
    if (!hdr) return NULL;
    evt = (MIDIEVENT *) (hdr + 1); /* place MIDIEVENT after header */
    hdr->lpData = (LPSTR) evt;
    hdr->dwBufferLength = MIDIHDR_SYSEX_BUFFER_LENGTH(data_size);
    hdr->dwBytesRecorded = 0;
    hdr->dwFlags = 0;
    hdr->dwUser = hdr->dwBufferLength;
    return hdr;
}


static PmError allocate_buffers(midiwinmm_type m, long data_size, long count)
{
    int i;
    /* buffers is an array of count pointers to MIDIHDR/MIDIEVENT struct */
    m->num_buffers = 0; /* in case no memory can be allocated */
    m->buffers = (LPMIDIHDR *) pm_alloc(sizeof(LPMIDIHDR) * count);
    if (!m->buffers) return pmInsufficientMemory;
    m->max_buffers = count;
    for (i = 0; i < count; i++) {
        LPMIDIHDR hdr = allocate_buffer(data_size);
        if (!hdr) { /* free everything allocated so far and return */
            for (i = i - 1; i >= 0; i--) pm_free(m->buffers[i]);
            pm_free(m->buffers);
            m->max_buffers = 0;
            return pmInsufficientMemory;
        }
        m->buffers[i] = hdr; /* this may be NULL if allocation fails */
    }
    m->num_buffers = count;
    return pmNoError;
}


static LPMIDIHDR get_free_output_buffer(PmInternal *midi)
{
    LPMIDIHDR r = NULL;
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    while (TRUE) {
        int i;
        for (i = 0; i < m->num_buffers; i++) {
            /* cycle through buffers, modulo m->num_buffers */
            m->next_buffer++;
            if (m->next_buffer >= m->num_buffers) m->next_buffer = 0;
            r = m->buffers[m->next_buffer];
            if ((r->dwFlags & MHDR_PREPARED) == 0) goto found_buffer;
        }
        /* after scanning every buffer and not finding anything, block */
        if (WaitForSingleObject(m->buffer_signal, 1000) == WAIT_TIMEOUT) {
#ifdef DEBUG
            printf("PortMidi warning: get_free_output_buffer() wait timed out after 1000ms\n");
#endif
            /* if we're trying to send a sysex message, maybe the 
             * message is too big and we need more message buffers.
             * Expand the buffer pool by 128KB using 1024-byte buffers.
             */
            /* first, expand the buffers array if necessary */
            if (!m->buffers_expanded) {
                LPMIDIHDR *new_buffers = (LPMIDIHDR *) pm_alloc(
                        (m->num_buffers + NUM_EXPANSION_BUFFERS) * 
                        sizeof(LPMIDIHDR));
                /* if no memory, we could return a no-memory error, but user
                 * probably will be unprepared to deal with it. Maybe the
                 * MIDI driver is temporarily hung so we should just wait.
                 * I don't know the right answer, but waiting is easier.
                 */
                if (!new_buffers) continue;
                /* copy buffers to new_buffers and replace buffers */
                memcpy(new_buffers, m->buffers, 
                       m->num_buffers * sizeof(LPMIDIHDR));
                pm_free(m->buffers);
                m->buffers = new_buffers;
                m->max_buffers = m->num_buffers + NUM_EXPANSION_BUFFERS;
                m->buffers_expanded = TRUE;
            }
            /* next, add one buffer and return it */
            if (m->num_buffers < m->max_buffers) {
                r = allocate_buffer(EXPANSION_BUFFER_LEN);
                /* again, if there's no memory, we may not really be 
                 * dead -- maybe the system is temporarily hung and
                 * we can just wait longer for a message buffer */
                if (!r) continue;
                m->buffers[m->num_buffers++] = r;
                goto found_buffer; /* break out of 2 loops */
            }
            /* else, we've allocated all NUM_EXPANSION_BUFFERS buffers,
             * and we have no free buffers to send. We'll just keep
             * polling to see if any buffers show up.
             */
        }
    }
found_buffer:
    r->dwBytesRecorded = 0;
    /* actual buffer length is saved in dwUser field */
    r->dwBufferLength = (DWORD) r->dwUser;
    return r;
}

#ifdef EXPANDING_SYSEX_BUFFERS
note: this is not working code, but might be useful if you want
      to grow sysex buffers.
static PmError resize_sysex_buffer(PmInternal *midi, long old_size, long new_size)
{
    LPMIDIHDR big;
    int i;
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    /* buffer must be smaller than 64k, but be also a multiple of 4 */
    if (new_size > 65520) {
        if (old_size >= 65520)
            return pmBufferMaxSize;
        else
            new_size = 65520;
    }
    /* allocate a bigger message  */
    big = allocate_sysex_buffer(new_size);
    /* printf("expand to %d bytes\n", new_size);*/
    if (!big) return pmInsufficientMemory;
    m->error = midiOutPrepareHeader(m->handle.out, big, sizeof(MIDIHDR));
    if (m->error) {
        pm_free(big);
        return pmHostError;
    }
    /* make sure we're not going to overwrite any memory */
    assert(old_size <= new_size);
    memcpy(big->lpData, m->hdr->lpData, old_size);
    /* keep track of how many sysex bytes are in message so far */
    big->dwBytesRecorded = m->hdr->dwBytesRecorded;
    big->dwBufferLength = new_size;
    /* find which buffer this was, and replace it */
    for (i = 0; i < NUM_SYSEX_BUFFERS; i++) {
        if (m->sysex_buffers[i] == m->hdr) {
            m->sysex_buffers[i] = big;
            m->sysex_buffer_size[i] = new_size;
            pm_free(m->hdr);
            m->hdr = big;
            break;
        }
    }
    assert(i != NUM_SYSEX_BUFFERS);

    return pmNoError;
}
#endif

/*
=========================================================================================
begin midi input implementation
=========================================================================================
*/


static PmError allocate_input_buffer(HMIDIIN h, long buffer_len)
{
    LPMIDIHDR hdr = allocate_buffer(buffer_len);
    if (!hdr) return pmInsufficientMemory;
    pm_hosterror = midiInPrepareHeader(h, hdr, sizeof(MIDIHDR));
    if (pm_hosterror) {
        pm_free(hdr);
        return pm_hosterror;
    }
    pm_hosterror = midiInAddBuffer(h, hdr, sizeof(MIDIHDR));
    return pm_hosterror;
}


static PmError winmm_in_open(PmInternal *midi, void *driverInfo)
{
    DWORD dwDevice;
    int i = midi->device_id;
    int max_sysex_len = midi->buffer_len * 4;
    int num_input_buffers = max_sysex_len / INPUT_SYSEX_LEN;
    midiwinmm_type m;

    dwDevice = (DWORD) descriptors[i].descriptor;

    /* create system dependent device data */
    m = (midiwinmm_type) pm_alloc(sizeof(midiwinmm_node)); /* create */
    midi->descriptor = m;
    if (!m) goto no_memory;
    m->handle.in = NULL;
    m->buffers = NULL; /* not used for input */
    m->num_buffers = 0; /* not used for input */
    m->max_buffers = FALSE; /* not used for input */
    m->buffers_expanded = 0; /* not used for input */
    m->next_buffer = 0; /* not used for input */
    m->buffer_signal = 0; /* not used for input */
    m->last_time = 0;
    m->first_message = TRUE; /* not used for input */
    m->sysex_mode = FALSE;
    m->sysex_word = 0;
    m->sysex_byte_count = 0;
    m->hdr = NULL; /* not used for input */
    m->sync_time = 0;
    m->delta = 0;
    m->error = MMSYSERR_NOERROR;
    /* 4000 is based on Windows documentation -- that's the value used in the
       memory manager. It's small enough that it should not hurt performance even
       if it's not optimal.
     */
    InitializeCriticalSectionAndSpinCount(&m->lock, 4000);
    /* open device */
    pm_hosterror = midiInOpen(
	    &(m->handle.in),  /* input device handle */
	    dwDevice,  /* device ID */
	    (DWORD_PTR) winmm_in_callback,  /* callback address */
	    (DWORD_PTR) midi,  /* callback instance data */
	    CALLBACK_FUNCTION); /* callback is a procedure */
    if (pm_hosterror) goto free_descriptor;

    if (num_input_buffers < MIN_INPUT_BUFFERS)
        num_input_buffers = MIN_INPUT_BUFFERS;
    for (i = 0; i < num_input_buffers; i++) {
        if (allocate_input_buffer(m->handle.in, INPUT_SYSEX_LEN)) {
            /* either pm_hosterror was set, or the proper return code
               is pmInsufficientMemory */
            goto close_device;
        }
    }
    /* start device */
    pm_hosterror = midiInStart(m->handle.in);
    if (pm_hosterror) goto reset_device;
    return pmNoError;

    /* undo steps leading up to the detected error */
reset_device:
    /* ignore return code (we already have an error to report) */
    midiInReset(m->handle.in);
close_device:
    midiInClose(m->handle.in); /* ignore return code */
free_descriptor:
    midi->descriptor = NULL;
    pm_free(m);
no_memory:
    if (pm_hosterror) {
        int err = midiInGetErrorText(pm_hosterror, (char *) pm_hosterror_text,
                                     PM_HOST_ERROR_MSG_LEN);
        assert(err == MMSYSERR_NOERROR);
        return pmHostError;
    }
    /* if !pm_hosterror, then the error must be pmInsufficientMemory */
    return pmInsufficientMemory;
    /* note: if we return an error code, the device will be
       closed and memory will be freed. It's up to the caller
       to free the parameter midi */
}

static PmError winmm_in_poll(PmInternal *midi) {
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    return m->error;
}



/* winmm_in_close -- close an open midi input device */
/*
 * assume midi is non-null (checked by caller)
 */
static PmError winmm_in_close(PmInternal *midi)
{
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    if (!m) return pmBadPtr;
    /* device to close */
    if (pm_hosterror = midiInStop(m->handle.in)) {
        midiInReset(m->handle.in); /* try to reset and close port */
        midiInClose(m->handle.in);
    } else if (pm_hosterror = midiInReset(m->handle.in)) {
        midiInClose(m->handle.in); /* best effort to close midi port */
    } else {
        pm_hosterror = midiInClose(m->handle.in);
    }
    midi->descriptor = NULL;
    DeleteCriticalSection(&m->lock);
    pm_free(m); /* delete */
    if (pm_hosterror) {
        int err = midiInGetErrorText(pm_hosterror, (char *) pm_hosterror_text,
                                     PM_HOST_ERROR_MSG_LEN);
        assert(err == MMSYSERR_NOERROR);
        return pmHostError;
    }
    return pmNoError;
}


/* Callback function executed via midiInput SW interrupt (via midiInOpen). */
static void FAR PASCAL winmm_in_callback(
    HMIDIIN hMidiIn,       /* midiInput device Handle */
    UINT wMsg,             /* midi msg */
    DWORD_PTR dwInstance,  /* application data */
    DWORD_PTR dwParam1,    /* MIDI data */
    DWORD_PTR dwParam2)    /* device timestamp (wrt most recent midiInStart) */
{
    static int entry = 0;
    PmInternal *midi = (PmInternal *) dwInstance;
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;

    /* NOTE: we do not just EnterCriticalSection() here because an
     * MIM_CLOSE message arrives when the port is closed, but then
     * the m->lock has been destroyed.
     */

    switch (wMsg) {
    case MIM_DATA: {
        /* if this callback is reentered with data, we're in trouble. 
         * It's hard to imagine that Microsoft would allow callbacks 
         * to be reentrant -- isn't the model that this is like a 
         * hardware interrupt? -- but I've seen reentrant behavior 
         * using a debugger, so it happens.
         */
        EnterCriticalSection(&m->lock);

        /* dwParam1 is MIDI data received, packed into DWORD w/ 1st byte of
                message LOB;
           dwParam2 is time message received by input device driver, specified
            in [ms] from when midiInStart called.
           each message is expanded to include the status byte */

        if ((dwParam1 & 0x80) == 0) {
            /* not a status byte -- ignore it. This happened running the
               sysex.c test under Win2K with MidiMan USB 1x1 interface,
               but I can't reproduce it. -RBD
             */
            /* printf("non-status byte found\n"); */
        } else { /* data to process */
            PmEvent event;
            if (midi->time_proc)
                dwParam2 = (*midi->time_proc)(midi->time_info);
            event.timestamp = (PmTimestamp)dwParam2;
            event.message = (PmMessage)dwParam1;
            pm_read_short(midi, &event);
        }
        LeaveCriticalSection(&m->lock);
        break;
    }
    case MIM_LONGDATA: {
        MIDIHDR *lpMidiHdr = (MIDIHDR *) dwParam1;
        unsigned char *data = (unsigned char *) lpMidiHdr->lpData;
        unsigned int processed = 0;
        int remaining = lpMidiHdr->dwBytesRecorded;

        EnterCriticalSection(&m->lock);
        /* printf("midi_in_callback -- lpMidiHdr %x, %d bytes, %2x...\n", 
                lpMidiHdr, lpMidiHdr->dwBytesRecorded, *data); */
        if (midi->time_proc)
            dwParam2 = (*midi->time_proc)(midi->time_info);
        /* can there be more than one message in one buffer? */
        /* assume yes and iterate through them */
        while (remaining > 0) {
            unsigned int amt = pm_read_bytes(midi, data + processed, 
                                             remaining, (PmTimestamp)dwParam2);
            remaining -= amt;
            processed += amt;
        }

        /* when a device is closed, the pending MIM_LONGDATA buffers are
           returned to this callback with dwBytesRecorded == 0. In this
           case, we do not want to send them back to the interface (if
           we do, the interface will not close, and Windows OS may hang). */
        if (lpMidiHdr->dwBytesRecorded > 0) {
            MMRESULT rslt;
            lpMidiHdr->dwBytesRecorded = 0;
            lpMidiHdr->dwFlags = 0;
			
            /* note: no error checking -- can this actually fail? */
            rslt = midiInPrepareHeader(hMidiIn, lpMidiHdr, sizeof(MIDIHDR));
            assert(rslt == MMSYSERR_NOERROR);
            /* note: I don't think this can fail except possibly for
             * MMSYSERR_NOMEM, but the pain of reporting this
             * unlikely but probably catastrophic error does not seem
             * worth it.
             */
            rslt = midiInAddBuffer(hMidiIn, lpMidiHdr, sizeof(MIDIHDR));
            assert(rslt == MMSYSERR_NOERROR);
            LeaveCriticalSection(&m->lock);
        } else {
            midiInUnprepareHeader(hMidiIn,lpMidiHdr,sizeof(MIDIHDR));
            LeaveCriticalSection(&m->lock);
            pm_free(lpMidiHdr);
        }
        break;
    }
    case MIM_OPEN:
        break;
    case MIM_CLOSE:
        break;
    case MIM_ERROR:
        /* printf("MIM_ERROR\n"); */
        break;
    case MIM_LONGERROR:
        /* printf("MIM_LONGERROR\n"); */
        break;
    default:
        break;
    }
}

/*
=========================================================================================
begin midi output implementation
=========================================================================================
*/

/* begin helper routines used by midiOutStream interface */

/* add_to_buffer -- adds timestamped short msg to buffer, returns fullp */
static int add_to_buffer(midiwinmm_type m, LPMIDIHDR hdr,
                         unsigned long delta, unsigned long msg)
{
    unsigned long *ptr = (unsigned long *)
                         (hdr->lpData + hdr->dwBytesRecorded);
    *ptr++ = delta; /* dwDeltaTime */
    *ptr++ = 0;     /* dwStream */
    *ptr++ = msg;   /* dwEvent */
    hdr->dwBytesRecorded += 3 * sizeof(long);
    /* if the addition of three more words (a message) would extend beyond
       the buffer length, then return TRUE (full)
     */
    return hdr->dwBytesRecorded + 3 * sizeof(long) > hdr->dwBufferLength;
}


static PmTimestamp pm_time_get(midiwinmm_type m)
{
    MMTIME mmtime;
    MMRESULT wRtn;
    mmtime.wType = TIME_TICKS;
    mmtime.u.ticks = 0;
    wRtn = midiStreamPosition(m->handle.stream, &mmtime, sizeof(mmtime));
    assert(wRtn == MMSYSERR_NOERROR);
    return mmtime.u.ticks;
}


/* end helper routines used by midiOutStream interface */


static PmError winmm_out_open(PmInternal *midi, void *driverInfo)
{
    DWORD dwDevice;
    int i = midi->device_id;
    midiwinmm_type m;
    MIDIPROPTEMPO propdata;
    MIDIPROPTIMEDIV divdata;
    int max_sysex_len = midi->buffer_len * 4;
    int output_buffer_len;
    int num_buffers;
    dwDevice = (DWORD) descriptors[i].descriptor;

    /* create system dependent device data */
    m = (midiwinmm_type) pm_alloc(sizeof(midiwinmm_node)); /* create */
    midi->descriptor = m;
    if (!m) goto no_memory;
    m->handle.out = NULL;
    m->buffers = NULL;
    m->num_buffers = 0;
    m->max_buffers = 0;
    m->buffers_expanded = FALSE;
    m->next_buffer = 0;
    m->last_time = 0;
    m->first_message = TRUE; /* we treat first message as special case */
    m->sysex_mode = FALSE;
    m->sysex_word = 0;
    m->sysex_byte_count = 0;
    m->hdr = NULL;
    m->sync_time = 0;
    m->delta = 0;
    m->error = MMSYSERR_NOERROR;

    /* create a signal */
    m->buffer_signal = CreateEvent(NULL, FALSE, FALSE, NULL);

    /* this should only fail when there are very serious problems */
    assert(m->buffer_signal);

    /* open device */
    if (midi->latency == 0) {
        /* use simple midi out calls */
        pm_hosterror = midiOutOpen(
                (LPHMIDIOUT) & m->handle.out,  /* device Handle */
		dwDevice,  /* device ID  */
		/* note: same callback fn as for StreamOpen: */
		(DWORD_PTR) winmm_streamout_callback, /* callback fn */
		(DWORD_PTR) midi,  /* callback instance data */
		CALLBACK_FUNCTION); /* callback type */
    } else {
        /* use stream-based midi output (schedulable in future) */
        pm_hosterror = midiStreamOpen(
	        &m->handle.stream,  /* device Handle */
		(LPUINT) & dwDevice,  /* device ID pointer */
		1,  /* reserved, must be 1 */
		(DWORD_PTR) winmm_streamout_callback,
		(DWORD_PTR) midi,  /* callback instance data */
		CALLBACK_FUNCTION);
    }
    if (pm_hosterror != MMSYSERR_NOERROR) {
        goto free_descriptor;
    }

    if (midi->latency == 0) {
        num_buffers = NUM_SIMPLE_SYSEX_BUFFERS;
        output_buffer_len = max_sysex_len / num_buffers;
        if (output_buffer_len < MIN_SIMPLE_SYSEX_LEN)
            output_buffer_len = MIN_SIMPLE_SYSEX_LEN;
    } else {
        long dur = 0;
        num_buffers = max(midi->buffer_len, midi->latency / 2);
        if (num_buffers < MIN_STREAM_BUFFERS)
            num_buffers = MIN_STREAM_BUFFERS;
        output_buffer_len = STREAM_BUFFER_LEN;

        propdata.cbStruct = sizeof(MIDIPROPTEMPO);
        propdata.dwTempo = 480000; /* microseconds per quarter */
        pm_hosterror = midiStreamProperty(m->handle.stream,
                                          (LPBYTE) & propdata,
                                          MIDIPROP_SET | MIDIPROP_TEMPO);
        if (pm_hosterror) goto close_device;

        divdata.cbStruct = sizeof(MIDIPROPTEMPO);
        divdata.dwTimeDiv = 480;   /* divisions per quarter */
        pm_hosterror = midiStreamProperty(m->handle.stream,
                                          (LPBYTE) & divdata,
                                          MIDIPROP_SET | MIDIPROP_TIMEDIV);
        if (pm_hosterror) goto close_device;
    }
    /* allocate buffers */
    if (allocate_buffers(m, output_buffer_len, num_buffers)) 
        goto free_buffers;
    /* start device */
    if (midi->latency != 0) {
        pm_hosterror = midiStreamRestart(m->handle.stream);
        if (pm_hosterror != MMSYSERR_NOERROR) goto free_buffers;
    }
    return pmNoError;

free_buffers:
    /* buffers are freed below by winmm_out_delete */
close_device:
    midiOutClose(m->handle.out);
free_descriptor:
    midi->descriptor = NULL;
    winmm_out_delete(midi); /* frees buffers and m */
no_memory:
    if (pm_hosterror) {
        int err = midiOutGetErrorText(pm_hosterror, (char *) pm_hosterror_text,
                                      PM_HOST_ERROR_MSG_LEN);
        assert(err == MMSYSERR_NOERROR);
        return pmHostError;
    }
    return pmInsufficientMemory;
}


/* winmm_out_delete -- carefully free data associated with midi */
/**/
static void winmm_out_delete(PmInternal *midi)
{
    int i;
    /* delete system dependent device data */
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    if (m) {
        if (m->buffer_signal) {
            /* don't report errors -- better not to stop cleanup */
            CloseHandle(m->buffer_signal);
        }
        /* if using stream output, free buffers */
        for (i = 0; i < m->num_buffers; i++) {
            if (m->buffers[i]) pm_free(m->buffers[i]);
        }
        m->num_buffers = 0;
        pm_free(m->buffers);
        m->max_buffers = 0;
    }
    midi->descriptor = NULL;
    pm_free(m); /* delete */
}


/* see comments for winmm_in_close */
static PmError winmm_out_close(PmInternal *midi)
{
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    if (m->handle.out) {
        /* device to close */
        if (midi->latency == 0) {
            pm_hosterror = midiOutClose(m->handle.out);
        } else {
            pm_hosterror = midiStreamClose(m->handle.stream);
        }
        /* regardless of outcome, free memory */
        winmm_out_delete(midi);
    }
    if (pm_hosterror) {
        int err = midiOutGetErrorText(pm_hosterror,
                                      (char *) pm_hosterror_text,
                                      PM_HOST_ERROR_MSG_LEN);
        assert(err == MMSYSERR_NOERROR);
        return pmHostError;
    }
    return pmNoError;
}


static PmError winmm_out_abort(PmInternal *midi)
{
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    m->error = MMSYSERR_NOERROR;

    /* only stop output streams */
    if (midi->latency > 0) {
        m->error = midiStreamStop(m->handle.stream);
    }
    return m->error ? pmHostError : pmNoError;
}


static PmError winmm_write_flush(PmInternal *midi, PmTimestamp timestamp)
{
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    assert(m);
    if (m->hdr) {
        m->error = midiOutPrepareHeader(m->handle.out, m->hdr, 
                                        sizeof(MIDIHDR));
        if (m->error) {
            /* do not send message */
        } else if (midi->latency == 0) {
            /* As pointed out by Nigel Brown, 20Sep06, dwBytesRecorded
             * should be zero. This is set in get_free_sysex_buffer(). 
             * The msg length goes in dwBufferLength in spite of what
             * Microsoft documentation says (or doesn't say). */
            m->hdr->dwBufferLength = m->hdr->dwBytesRecorded;
            m->hdr->dwBytesRecorded = 0;
            m->error = midiOutLongMsg(m->handle.out, m->hdr, sizeof(MIDIHDR));
        } else {
            m->error = midiStreamOut(m->handle.stream, m->hdr, 
                                     sizeof(MIDIHDR));
        }
        midi->fill_base = NULL;
        m->hdr = NULL;
        if (m->error) {
            m->hdr->dwFlags = 0; /* release the buffer */
            return pmHostError;
        }
    }
    return pmNoError;
}


static PmError winmm_write_short(PmInternal *midi, PmEvent *event)
{
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    PmError rslt = pmNoError;
    assert(m);

    if (midi->latency == 0) { /* use midiOut interface, ignore timestamps */
        m->error = midiOutShortMsg(m->handle.out, event->message);
        if (m->error) rslt = pmHostError;
    } else {  /* use midiStream interface -- pass data through buffers */
        unsigned long when = event->timestamp;
        unsigned long delta;
        int full;
        if (when == 0) when = midi->now;
        /* when is in real_time; translate to intended stream time */
        when = when + m->delta + midi->latency;
        /* make sure we don't go backward in time */
        if (when < m->last_time) when = m->last_time;
        delta = when - m->last_time;
        m->last_time = when;
        /* before we insert any data, we must have a buffer */
        if (m->hdr == NULL) {
            /* stream interface: buffers allocated when stream is opened */
            m->hdr = get_free_output_buffer(midi);
        }
        full = add_to_buffer(m, m->hdr, delta, event->message);
        if (full) rslt = winmm_write_flush(midi, when);
    }
    return rslt;
}

#define winmm_begin_sysex winmm_write_flush
#ifndef winmm_begin_sysex
static PmError winmm_begin_sysex(PmInternal *midi, PmTimestamp timestamp)
{
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    PmError rslt = pmNoError;

    if (midi->latency == 0) {
        /* do nothing -- it's handled in winmm_write_byte */
    } else {
        /* sysex expects an empty sysex buffer, so send whatever is here */
        rslt = winmm_write_flush(midi);
    }
    return rslt;
}
#endif

static PmError winmm_end_sysex(PmInternal *midi, PmTimestamp timestamp)
{
    /* could check for callback_error here, but I haven't checked
     * what happens if we exit early and don't finish the sysex msg
     * and clean up
     */
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    PmError rslt = pmNoError;
    LPMIDIHDR hdr = m->hdr;
    if (!hdr) return rslt; /* something bad happened earlier,
            do not report an error because it would have been 
            reported (at least) once already */
    /* a(n old) version of MIDI YOKE requires a zero byte after
     * the sysex message, but do not increment dwBytesRecorded: */
    hdr->lpData[hdr->dwBytesRecorded] = 0;
    if (midi->latency == 0) {
#ifdef DEBUG_PRINT_BEFORE_SENDING_SYSEX
        /* DEBUG CODE: */
        { int i; int len = m->hdr->dwBufferLength;
          printf("OutLongMsg %d ", len);
          for (i = 0; i < len; i++) {
              printf("%2x ", (unsigned char) (m->hdr->lpData[i]));
          }
        }
#endif
    } else {
        /* Using stream interface. There are accumulated bytes in m->hdr
           to send using midiStreamOut
         */
        /* add bytes recorded to MIDIEVENT length, but don't
           count the MIDIEVENT data (3 longs) */
        MIDIEVENT *evt = (MIDIEVENT *) (hdr->lpData);
        evt->dwEvent += hdr->dwBytesRecorded - 3 * sizeof(long);
        /* round up BytesRecorded to multiple of 4 */
        hdr->dwBytesRecorded = (hdr->dwBytesRecorded + 3) & ~3;
    }
    rslt = winmm_write_flush(midi, timestamp);
    return rslt;
}


static PmError winmm_write_byte(PmInternal *midi, unsigned char byte,
                                PmTimestamp timestamp)
{
    /* write a sysex byte */
    PmError rslt = pmNoError;
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    LPMIDIHDR hdr = m->hdr;
    unsigned char *msg_buffer;
    assert(m);
    if (!hdr) {
        m->hdr = hdr = get_free_output_buffer(midi);
        assert(hdr);
        midi->fill_base = (unsigned char *) m->hdr->lpData;
        midi->fill_offset_ptr = &(hdr->dwBytesRecorded);
        /* when buffer fills, Pm_WriteSysEx will revert to calling
         * pmwin_write_byte, which expect to have space, so leave
         * one byte free for pmwin_write_byte. Leave another byte
         * of space for zero after message to make early version of 
         * MIDI YOKE driver happy -- therefore dwBufferLength - 2 */
        midi->fill_length = hdr->dwBufferLength - 2;
        if (midi->latency != 0) {
            unsigned long when = (unsigned long) timestamp;
            unsigned long delta;
            unsigned long *ptr;
            if (when == 0) when = midi->now;
            /* when is in real_time; translate to intended stream time */
            when = when + m->delta + midi->latency;
            /* make sure we don't go backward in time */
            if (when < m->last_time) when = m->last_time;
            delta = when - m->last_time;
            m->last_time = when;

            ptr = (unsigned long *) hdr->lpData;
            *ptr++ = delta;
            *ptr++ = 0;
            *ptr = MEVT_F_LONG;
            hdr->dwBytesRecorded = 3 * sizeof(long);
            /* data will be added at an offset of dwBytesRecorded ... */
        }
    }
    /* add the data byte */
    msg_buffer = (unsigned char *) (hdr->lpData);
    msg_buffer[hdr->dwBytesRecorded++] = byte;

    /* see if buffer is full, leave one byte extra for pad */
    if (hdr->dwBytesRecorded >= hdr->dwBufferLength - 1) {
        /* write what we've got and continue */
        rslt = winmm_end_sysex(midi, timestamp); 
    }
    return rslt;
}


static PmTimestamp winmm_synchronize(PmInternal *midi)
{
    midiwinmm_type m;
    unsigned long pm_stream_time_2;
    unsigned long real_time;
    unsigned long pm_stream_time;

    /* only synchronize if we are using stream interface */
    if (midi->latency == 0) return 0;

    /* figure out the time */
    m = (midiwinmm_type) midi->descriptor;
    pm_stream_time_2 = pm_time_get(m);

    do {
        /* read real_time between two reads of stream time */
        pm_stream_time = pm_stream_time_2;
        real_time = (*midi->time_proc)(midi->time_info);
        pm_stream_time_2 = pm_time_get(m);
        /* repeat if more than 1ms elapsed */
    } while (pm_stream_time_2 > pm_stream_time + 1);
    m->delta = pm_stream_time - real_time;
    m->sync_time = real_time;
    return real_time;
}


/* winmm_streamout_callback -- unprepare (free) buffer header */
static void CALLBACK winmm_streamout_callback(HMIDIOUT hmo, UINT wMsg,
        DWORD_PTR dwInstance, DWORD_PTR dwParam1, DWORD_PTR dwParam2)
{
    PmInternal *midi = (PmInternal *) dwInstance;
    midiwinmm_type m = (midiwinmm_type) midi->descriptor;
    LPMIDIHDR hdr = (LPMIDIHDR) dwParam1;
    int err;

    /* Even if an error is pending, I think we should unprepare msgs and
       signal their arrival
     */
    /* printf("streamout_callback: hdr %x, wMsg %x, MOM_DONE %x\n", 
           hdr, wMsg, MOM_DONE); */
    if (wMsg == MOM_DONE) {
        MMRESULT ret = midiOutUnprepareHeader(m->handle.out, hdr, 
                                              sizeof(MIDIHDR));
        assert(ret == MMSYSERR_NOERROR);
    }
    /* signal client in case it is blocked waiting for buffer */
    err = SetEvent(m->buffer_signal);
    assert(err); /* false -> error */
}


/*
=========================================================================================
begin exported functions
=========================================================================================
*/

#define winmm_in_abort pm_fail_fn
pm_fns_node pm_winmm_in_dictionary = {
                                         none_write_short,
                                         none_sysex,
                                         none_sysex,
                                         none_write_byte,
                                         none_write_short,
                                         none_write_flush,
                                         winmm_synchronize,
                                         winmm_in_open,
                                         winmm_in_abort,
                                         winmm_in_close,
                                         winmm_in_poll,
                                         winmm_has_host_error,
                                         winmm_get_host_error
                                     };

pm_fns_node pm_winmm_out_dictionary = {
                                          winmm_write_short,
                                          winmm_begin_sysex,
                                          winmm_end_sysex,
                                          winmm_write_byte,
                                          winmm_write_short,  /* short realtime message */
                                          winmm_write_flush,
                                          winmm_synchronize,
                                          winmm_out_open,
                                          winmm_out_abort,
                                          winmm_out_close,
                                          none_poll,
                                          winmm_has_host_error,
                                          winmm_get_host_error
                                      };


/* initialize winmm interface. Note that if there is something wrong
   with winmm (e.g. it is not supported or installed), it is not an
   error. We should simply return without having added any devices to
   the table. Hence, no error code is returned. Furthermore, this init
   code is called along with every other supported interface, so the
   user would have a very hard time figuring out what hardware and API
   generated the error. Finally, it would add complexity to pmwin.c to
   remember where the error code came from in order to convert to text.
 */
void pm_winmm_init( void )
{
    pm_winmm_mapper_input();
    pm_winmm_mapper_output();
    pm_winmm_general_inputs();
    pm_winmm_general_outputs();
}


/* no error codes are returned, even if errors are encountered, because
   there is probably nothing the user could do (e.g. it would be an error
   to retry.
 */
void pm_winmm_term( void )
{
    int i;
#ifdef DEBUG
    char msg[PM_HOST_ERROR_MSG_LEN];
#endif
    int doneAny = 0;
#ifdef DEBUG
    printf("pm_winmm_term called\n");
#endif
    for (i = 0; i < pm_descriptor_index; i++) {
        PmInternal * midi = descriptors[i].internalDescriptor;
        if (midi) {
            midiwinmm_type m = (midiwinmm_type) midi->descriptor;
            if (m->handle.out) {
                /* close next open device*/
#ifdef DEBUG
                if (doneAny == 0) {
                    printf("begin closing open devices...\n");
                    doneAny = 1;
                }
                /* report any host errors; this EXTEREMELY useful when
                   trying to debug client app */
                if (winmm_has_host_error(midi)) {
                    winmm_get_host_error(midi, msg, PM_HOST_ERROR_MSG_LEN);
                    printf("%s\n", msg);
                }
#endif
                /* close all open ports */
                (*midi->dictionary->close)(midi);
            }
        }
    }
    if (midi_in_caps) {
        pm_free(midi_in_caps);
        midi_in_caps = NULL;
    }
    if (midi_out_caps) {
        pm_free(midi_out_caps);
        midi_out_caps = NULL;
    }
#ifdef DEBUG
    if (doneAny) {
        printf("warning: devices were left open. They have been closed.\n");
    }
    printf("pm_winmm_term exiting\n");
#endif
    pm_descriptor_index = 0;
}
#elif __APPLE__
/* ptmacosx.c -- portable timer implementation for mac os x */

#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <CoreFoundation/CoreFoundation.h>

#import <mach/mach.h>
#import <mach/mach_error.h>
#import <mach/mach_time.h>
#import <mach/clock.h>

// (amalg) #include "porttime.h"

#define THREAD_IMPORTANCE 30
#define LONG_TIME 1000000000.0

static int time_started_flag = FALSE;
static CFAbsoluteTime startTime = 0.0;
static CFRunLoopRef timerRunLoop;

typedef struct {
    int resolution;
    PtCallback *callback;
    void *userData;
} PtThreadParams;


void Pt_CFTimerCallback(CFRunLoopTimerRef timer, void *info)
{
    PtThreadParams *params = (PtThreadParams*)info;
    (*params->callback)(Pt_Time(), params->userData);
}

static void* Pt_Thread(void *p)
{
    CFTimeInterval timerInterval;
    CFRunLoopTimerContext timerContext;
    CFRunLoopTimerRef timer;
    PtThreadParams *params = (PtThreadParams*)p;
    //CFTimeInterval timeout;

    /* raise the thread's priority */
    kern_return_t error;
    thread_extended_policy_data_t extendedPolicy;
    thread_precedence_policy_data_t precedencePolicy;

    extendedPolicy.timeshare = 0;
    error = thread_policy_set(mach_thread_self(), THREAD_EXTENDED_POLICY,
                              (thread_policy_t)&extendedPolicy,
                              THREAD_EXTENDED_POLICY_COUNT);
    if (error != KERN_SUCCESS) {
        mach_error("Couldn't set thread timeshare policy", error);
    }

    precedencePolicy.importance = THREAD_IMPORTANCE;
    error = thread_policy_set(mach_thread_self(), THREAD_PRECEDENCE_POLICY,
                              (thread_policy_t)&precedencePolicy,
                              THREAD_PRECEDENCE_POLICY_COUNT);
    if (error != KERN_SUCCESS) {
        mach_error("Couldn't set thread precedence policy", error);
    }

    /* set up the timer context */
    timerContext.version = 0;
    timerContext.info = params;
    timerContext.retain = NULL;
    timerContext.release = NULL;
    timerContext.copyDescription = NULL;

    /* create a new timer */
    timerInterval = (double)params->resolution / 1000.0;
    timer = CFRunLoopTimerCreate(NULL, startTime+timerInterval, timerInterval,
                                 0, 0, Pt_CFTimerCallback, &timerContext);

    timerRunLoop = CFRunLoopGetCurrent();
    CFRunLoopAddTimer(timerRunLoop, timer, CFSTR("PtTimeMode"));

    /* run until we're told to stop by Pt_Stop() */
    CFRunLoopRunInMode(CFSTR("PtTimeMode"), LONG_TIME, false);
    
    CFRunLoopRemoveTimer(CFRunLoopGetCurrent(), timer, CFSTR("PtTimeMode"));
    CFRelease(timer);
    free(params);

    return NULL;
}

PtError Pt_Start(int resolution, PtCallback *callback, void *userData)
{
    PtThreadParams *params = (PtThreadParams*)malloc(sizeof(PtThreadParams));
    pthread_t pthread_id;

    printf("Pt_Start() called\n");

    // /* make sure we're not already playing */
    if (time_started_flag) return ptAlreadyStarted;
    startTime = CFAbsoluteTimeGetCurrent();

    if (callback) {
    
        params->resolution = resolution;
        params->callback = callback;
        params->userData = userData;
    
        pthread_create(&pthread_id, NULL, Pt_Thread, params);
    }

    time_started_flag = TRUE;
    return ptNoError;
}


PtError Pt_Stop()
{
    printf("Pt_Stop called\n");

    CFRunLoopStop(timerRunLoop);
    time_started_flag = FALSE;
    return ptNoError;
}


int Pt_Started()
{
    return time_started_flag;
}


PtTimestamp Pt_Time()
{
    CFAbsoluteTime now = CFAbsoluteTimeGetCurrent();
    return (PtTimestamp) ((now - startTime) * 1000.0);
}


void Pt_Sleep(int32_t duration)
{
    usleep(duration * 1000);
}
/* pmmac.c -- PortMidi os-dependent code */

/* This file only needs to implement:
pm_init(), which calls various routines to register the 
available midi devices,
Pm_GetDefaultInputDeviceID(), and
Pm_GetDefaultOutputDeviceID().
It is seperate from pmmacosxcm because we might want to register
non-CoreMIDI devices.
*/

// (amalg) #include "stdlib.h"
// (amalg) #include "portmidi.h"
// (amalg) #include "pmutil.h"
// (amalg) #include "pminternal.h"
// (amalg) #include "pmmacosxcm.h"

PmDeviceID pm_default_input_device_id = -1;
PmDeviceID pm_default_output_device_id = -1;

void pm_init()
{
    PmError err = pm_macosxcm_init();
    // this is set when we return to Pm_Initialize, but we need it
    // now in order to (successfully) call Pm_CountDevices()
    pm_initialized = TRUE;
    if (!err) {
        pm_default_input_device_id = find_default_device(
                "/PortMidi/PM_RECOMMENDED_INPUT_DEVICE", TRUE, 
                pm_default_input_device_id);
        pm_default_output_device_id = find_default_device(
                "/PortMidi/PM_RECOMMENDED_OUTPUT_DEVICE", FALSE, 
                pm_default_output_device_id);
    }
}


void pm_term(void)
{
    pm_macosxcm_term();
}


PmDeviceID Pm_GetDefaultInputDeviceID()
{
    Pm_Initialize();
    return pm_default_input_device_id;
}

PmDeviceID Pm_GetDefaultOutputDeviceID() {
    Pm_Initialize();
    return pm_default_output_device_id;
}

void *pm_alloc(size_t s) { return malloc(s); }

void pm_free(void *ptr) { free(ptr); }


/*
 * Platform interface to the MacOS X CoreMIDI framework
 * 
 * Jon Parise <jparise at cmu.edu>
 * and subsequent work by Andrew Zeldis and Zico Kolter
 * and Roger B. Dannenberg
 *
 * $Id: pmmacosx.c,v 1.17 2002/01/27 02:40:40 jon Exp $
 */
 
/* Notes:
    since the input and output streams are represented by MIDIEndpointRef
    values and almost no other state, we store the MIDIEndpointRef on
    descriptors[midi->device_id].descriptor. The only other state we need
    is for errors: we need to know if there is an error and if so, what is
    the error text. We use a structure with two kinds of
    host error: "error" and "callback_error". That way, asynchronous callbacks
    do not interfere with other error information.
    
    OS X does not seem to have an error-code-to-text function, so we will
    just use text messages instead of error codes.
 */

#include <stdlib.h>

//#define CM_DEBUG 1

// (amalg) #include "portmidi.h"
// (amalg) #include "pmutil.h"
// (amalg) #include "pminternal.h"
// (amalg) #include "porttime.h"
// (amalg) #include "pmmac.h"
// (amalg) #include "pmmacosxcm.h"

#include <stdio.h>
#include <string.h>

#include <CoreServices/CoreServices.h>
#include <CoreMIDI/MIDIServices.h>
#include <CoreAudio/HostTime.h>
#include <unistd.h>

#define PACKET_BUFFER_SIZE 1024
/* maximum overall data rate (OS X limit is 15000 bytes/second) */
#define MAX_BYTES_PER_S 14000

/* Apple reports that packets are dropped when the MIDI bytes/sec
   exceeds 15000. This is computed by "tracking the number of MIDI 
   bytes scheduled into 1-second buckets over the last six seconds
   and averaging these counts." 

   This is apparently based on timestamps, not on real time, so 
   we have to avoid constructing packets that schedule high speed
   output even if the actual writes are delayed (which was my first
   solution).

   The LIMIT_RATE symbol, if defined, enables code to modify 
   timestamps as follows:
     After each packet is formed, the next allowable timestamp is
     computed as this_packet_time + this_packet_len * delay_per_byte

     This is the minimum timestamp allowed in the next packet. 

     Note that this distorts accurate timestamps somewhat.
 */
#define LIMIT_RATE 1

#define SYSEX_BUFFER_SIZE 128

#define VERBOSE_ON 1
#define VERBOSE if (VERBOSE_ON)

#define MIDI_SYSEX      0xf0
#define MIDI_EOX        0xf7
#define MIDI_STATUS_MASK 0x80

// "Ref"s are pointers on 32-bit machines and ints on 64 bit machines
// NULL_REF is our representation of either 0 or NULL
#ifdef __LP64__
#define NULL_REF 0
#else
#define NULL_REF NULL
#endif

static MIDIClientRef	client = NULL_REF; 	/* Client handle to the MIDI server */
static MIDIPortRef	portIn = NULL_REF;	/* Input port handle */
static MIDIPortRef	portOut = NULL_REF;	/* Output port handle */

extern pm_fns_node pm_macosx_in_dictionary;
extern pm_fns_node pm_macosx_out_dictionary;

typedef struct midi_macosxcm_struct {
    PmTimestamp sync_time; /* when did we last determine delta? */
    UInt64 delta;	/* difference between stream time and real time in ns */
    UInt64 last_time;	/* last output time in host units*/
    int first_message;  /* tells midi_write to sychronize timestamps */
    int sysex_mode;     /* middle of sending sysex */
    uint32_t sysex_word; /* accumulate data when receiving sysex */
    uint32_t sysex_byte_count; /* count how many received */
    char error[PM_HOST_ERROR_MSG_LEN];
    char callback_error[PM_HOST_ERROR_MSG_LEN];
    Byte packetBuffer[PACKET_BUFFER_SIZE];
    MIDIPacketList *packetList; /* a pointer to packetBuffer */
    MIDIPacket *packet;
    Byte sysex_buffer[SYSEX_BUFFER_SIZE]; /* temp storage for sysex data */
    MIDITimeStamp sysex_timestamp; /* timestamp to use with sysex data */
    /* allow for running status (is running status possible here? -rbd): -cpr */
    unsigned char last_command; 
    int32_t last_msg_length;
    /* limit midi data rate (a CoreMidi requirement): */
    UInt64 min_next_time; /* when can the next send take place? */
    int byte_count; /* how many bytes in the next packet list? */
    Float64 us_per_host_tick; /* host clock frequency, units of min_next_time */
    UInt64 host_ticks_per_byte; /* host clock units per byte at maximum rate */
} midi_macosxcm_node, *midi_macosxcm_type;

/* private function declarations */
MIDITimeStamp timestamp_pm_to_cm(PmTimestamp timestamp);
PmTimestamp timestamp_cm_to_pm(MIDITimeStamp timestamp);

char* cm_get_full_endpoint_name(MIDIEndpointRef endpoint);


static int
midi_length(int32_t msg)
{
    int status, high, low;
    static int high_lengths[] = {
        1, 1, 1, 1, 1, 1, 1, 1,         /* 0x00 through 0x70 */
        3, 3, 3, 3, 2, 2, 3, 1          /* 0x80 through 0xf0 */
    };
    static int low_lengths[] = {
        1, 2, 3, 2, 1, 1, 1, 1,         /* 0xf0 through 0xf8 */
        1, 1, 1, 1, 1, 1, 1, 1          /* 0xf9 through 0xff */
    };

    status = msg & 0xFF;
    high = status >> 4;
    low = status & 15;

    return (high != 0xF) ? high_lengths[high] : low_lengths[low];
}

static PmTimestamp midi_synchronize(PmInternal *midi)
{
    midi_macosxcm_type m = (midi_macosxcm_type) midi->descriptor;
    UInt64 pm_stream_time_2 = 
            AudioConvertHostTimeToNanos(AudioGetCurrentHostTime());
    PmTimestamp real_time;
    UInt64 pm_stream_time;
    /* if latency is zero and this is an output, there is no 
       time reference and midi_synchronize should never be called */
    assert(midi->time_proc);
    assert(!(midi->write_flag && midi->latency == 0));
    do {
         /* read real_time between two reads of stream time */
         pm_stream_time = pm_stream_time_2;
         real_time = (*midi->time_proc)(midi->time_info);
         pm_stream_time_2 = AudioConvertHostTimeToNanos(AudioGetCurrentHostTime());
         /* repeat if more than 0.5 ms has elapsed */
    } while (pm_stream_time_2 > pm_stream_time + 500000);
    m->delta = pm_stream_time - ((UInt64) real_time * (UInt64) 1000000);
    m->sync_time = real_time;
    return real_time;
}


static void
process_packet(MIDIPacket *packet, PmEvent *event, 
	       PmInternal *midi, midi_macosxcm_type m)
{
    /* handle a packet of MIDI messages from CoreMIDI */
    /* there may be multiple short messages in one packet (!) */
    unsigned int remaining_length = packet->length;
    unsigned char *cur_packet_data = packet->data;
    while (remaining_length > 0) {
        if (cur_packet_data[0] == MIDI_SYSEX ||
            /* are we in the middle of a sysex message? */
            (m->last_command == 0 &&
             !(cur_packet_data[0] & MIDI_STATUS_MASK))) {
            m->last_command = 0; /* no running status */
            unsigned int amt = pm_read_bytes(midi, cur_packet_data, 
                                             remaining_length, 
                                             event->timestamp);
            remaining_length -= amt;
            cur_packet_data += amt;
        } else if (cur_packet_data[0] == MIDI_EOX) {
            /* this should never happen, because pm_read_bytes should
             * get and read all EOX bytes*/
            midi->sysex_in_progress = FALSE;
            m->last_command = 0;
        } else if (cur_packet_data[0] & MIDI_STATUS_MASK) {
            /* compute the length of the next (short) msg in packet */
	    unsigned int cur_message_length = midi_length(cur_packet_data[0]);
            if (cur_message_length > remaining_length) {
#ifdef DEBUG
                printf("PortMidi debug msg: not enough data");
#endif
		/* since there's no more data, we're done */
		return;
	    }
	    m->last_msg_length = cur_message_length;
	    m->last_command = cur_packet_data[0];
	    switch (cur_message_length) {
	    case 1:
	        event->message = Pm_Message(cur_packet_data[0], 0, 0);
		break; 
	    case 2:
	        event->message = Pm_Message(cur_packet_data[0], 
					    cur_packet_data[1], 0);
		break;
	    case 3:
	        event->message = Pm_Message(cur_packet_data[0],
					    cur_packet_data[1], 
					    cur_packet_data[2]);
		break;
	    default:
                /* PortMIDI internal error; should never happen */
                assert(cur_message_length == 1);
	        return; /* give up on packet if continued after assert */
	    }
	    pm_read_short(midi, event);
	    remaining_length -= m->last_msg_length;
	    cur_packet_data += m->last_msg_length;
	} else if (m->last_msg_length > remaining_length + 1) {
	    /* we have running status, but not enough data */
#ifdef DEBUG
	    printf("PortMidi debug msg: not enough data in CoreMIDI packet");
#endif
	    /* since there's no more data, we're done */
	    return;
	} else { /* output message using running status */
	    switch (m->last_msg_length) {
	    case 1:
	        event->message = Pm_Message(m->last_command, 0, 0);
		break;
	    case 2:
	        event->message = Pm_Message(m->last_command, 
					    cur_packet_data[0], 0);
		break;
	    case 3:
	        event->message = Pm_Message(m->last_command, 
					    cur_packet_data[0], 
					    cur_packet_data[1]);
		break;
	    default:
	        /* last_msg_length is invalid -- internal PortMIDI error */
	        assert(m->last_msg_length == 1);
	    }
	    pm_read_short(midi, event);
	    remaining_length -= (m->last_msg_length - 1);
	    cur_packet_data += (m->last_msg_length - 1);
	}
    }
}



/* called when MIDI packets are received */
static void
readProc(const MIDIPacketList *newPackets, void *refCon, void *connRefCon)
{
    PmInternal *midi;
    midi_macosxcm_type m;
    PmEvent event;
    MIDIPacket *packet;
    unsigned int packetIndex;
    uint32_t now;
    unsigned int status;
    
#ifdef CM_DEBUG
    printf("readProc: numPackets %d: ", newPackets->numPackets);
#endif

    /* Retrieve the context for this connection */
    midi = (PmInternal *) connRefCon;
    m = (midi_macosxcm_type) midi->descriptor;
    assert(m);
    
    /* synchronize time references every 100ms */
    now = (*midi->time_proc)(midi->time_info);
    if (m->first_message || m->sync_time + 100 /*ms*/ < now) { 
        /* time to resync */
        now = midi_synchronize(midi);
        m->first_message = FALSE;
    }
    
    packet = (MIDIPacket *) &newPackets->packet[0];
    /* printf("readproc packet status %x length %d\n", packet->data[0], 
               packet->length); */
    for (packetIndex = 0; packetIndex < newPackets->numPackets; packetIndex++) {
        /* Set the timestamp and dispatch this message */
        event.timestamp = (PmTimestamp) /* explicit conversion */ (
                (AudioConvertHostTimeToNanos(packet->timeStamp) - m->delta) / 
                (UInt64) 1000000);
        status = packet->data[0];
        /* process packet as sysex data if it begins with MIDI_SYSEX, or
           MIDI_EOX or non-status byte with no running status */
#ifdef CM_DEBUG
        printf(" %d", packet->length);
#endif
        if (status == MIDI_SYSEX || status == MIDI_EOX || 
            ((!(status & MIDI_STATUS_MASK)) && !m->last_command)) {
	    /* previously was: !(status & MIDI_STATUS_MASK)) {
             * but this could mistake running status for sysex data
             */
            /* reset running status data -cpr */
	    m->last_command = 0;
	    m->last_msg_length = 0;
            /* printf("sysex packet length: %d\n", packet->length); */
            pm_read_bytes(midi, packet->data, packet->length, event.timestamp);
        } else {
            process_packet(packet, &event, midi, m);
	}
        packet = MIDIPacketNext(packet);
    }
#ifdef CM_DEBUG
    printf("\n");
#endif
}

static PmError
midi_in_open(PmInternal *midi, void *driverInfo)
{
    MIDIEndpointRef endpoint;
    midi_macosxcm_type m;
    OSStatus macHostError;
    
    /* insure that we have a time_proc for timing */
    if (midi->time_proc == NULL) {
        if (!Pt_Started()) 
            Pt_Start(1, 0, 0);
        /* time_get does not take a parameter, so coerce */
        midi->time_proc = (PmTimeProcPtr) Pt_Time;
    }
    endpoint = (MIDIEndpointRef) (long) descriptors[midi->device_id].descriptor;
    if (endpoint == NULL_REF) {
        return pmInvalidDeviceId;
    }

    m = (midi_macosxcm_type) pm_alloc(sizeof(midi_macosxcm_node)); /* create */
    midi->descriptor = m;
    if (!m) {
        return pmInsufficientMemory;
    }
    m->error[0] = 0;
    m->callback_error[0] = 0;
    m->sync_time = 0;
    m->delta = 0;
    m->last_time = 0;
    m->first_message = TRUE;
    m->sysex_mode = FALSE;
    m->sysex_word = 0;
    m->sysex_byte_count = 0;
    m->packetList = NULL;
    m->packet = NULL;
    m->last_command = 0;
    m->last_msg_length = 0;

    macHostError = MIDIPortConnectSource(portIn, endpoint, midi);
    if (macHostError != noErr) {
        pm_hosterror = macHostError;
        sprintf(pm_hosterror_text, 
                "Host error %ld: MIDIPortConnectSource() in midi_in_open()",
                (long) macHostError);
        midi->descriptor = NULL;
        pm_free(m);
        return pmHostError;
    }
    
    return pmNoError;
}

static PmError
midi_in_close(PmInternal *midi)
{
    MIDIEndpointRef endpoint;
    OSStatus macHostError;
    PmError err = pmNoError;
    
    midi_macosxcm_type m = (midi_macosxcm_type) midi->descriptor;
    
    if (!m) return pmBadPtr;

    endpoint = (MIDIEndpointRef) (long) descriptors[midi->device_id].descriptor;
    if (endpoint == NULL_REF) {
        pm_hosterror = pmBadPtr;
    }
    
    /* shut off the incoming messages before freeing data structures */
    macHostError = MIDIPortDisconnectSource(portIn, endpoint);
    if (macHostError != noErr) {
        pm_hosterror = macHostError;
        sprintf(pm_hosterror_text, 
                "Host error %ld: MIDIPortDisconnectSource() in midi_in_close()",
                (long) macHostError);
        err = pmHostError;
    }
    
    midi->descriptor = NULL;
    pm_free(midi->descriptor);
    
    return err;
}


static PmError
midi_out_open(PmInternal *midi, void *driverInfo)
{
    midi_macosxcm_type m;

    m = (midi_macosxcm_type) pm_alloc(sizeof(midi_macosxcm_node)); /* create */
    midi->descriptor = m;
    if (!m) {
        return pmInsufficientMemory;
    }
    m->error[0] = 0;
    m->callback_error[0] = 0;
    m->sync_time = 0;
    m->delta = 0;
    m->last_time = 0;
    m->first_message = TRUE;
    m->sysex_mode = FALSE;
    m->sysex_word = 0;
    m->sysex_byte_count = 0;
    m->packetList = (MIDIPacketList *) m->packetBuffer;
    m->packet = NULL;
    m->last_command = 0;
    m->last_msg_length = 0;
    m->min_next_time = 0;
    m->byte_count = 0;
    m->us_per_host_tick = 1000000.0 / AudioGetHostClockFrequency();
    m->host_ticks_per_byte = (UInt64) (1000000.0 / 
                                       (m->us_per_host_tick * MAX_BYTES_PER_S));
    return pmNoError;
}


static PmError
midi_out_close(PmInternal *midi)
{
    midi_macosxcm_type m = (midi_macosxcm_type) midi->descriptor;
    if (!m) return pmBadPtr;
    
    midi->descriptor = NULL;
    pm_free(midi->descriptor);
    
    return pmNoError;
}

static PmError
midi_abort(PmInternal *midi)
{
    PmError err = pmNoError;
    OSStatus macHostError;
    MIDIEndpointRef endpoint =
            (MIDIEndpointRef) (long) descriptors[midi->device_id].descriptor;
    macHostError = MIDIFlushOutput(endpoint);
    if (macHostError != noErr) {
        pm_hosterror = macHostError;
        sprintf(pm_hosterror_text,
                "Host error %ld: MIDIFlushOutput()", (long) macHostError);
        err = pmHostError;
    }
    return err;
}


static PmError
midi_write_flush(PmInternal *midi, PmTimestamp timestamp)
{
    OSStatus macHostError;
    midi_macosxcm_type m = (midi_macosxcm_type) midi->descriptor;
    MIDIEndpointRef endpoint = 
            (MIDIEndpointRef) (long) descriptors[midi->device_id].descriptor;
    assert(m);
    assert(endpoint);
    if (m->packet != NULL) {
        /* out of space, send the buffer and start refilling it */
        /* before we can send, maybe delay to limit data rate. OS X allows
         * 15KB/s. */
        UInt64 now = AudioGetCurrentHostTime();
        if (now < m->min_next_time) {
            usleep((useconds_t) 
                   ((m->min_next_time - now) * m->us_per_host_tick));
        }
        macHostError = MIDISend(portOut, endpoint, m->packetList);
        m->packet = NULL; /* indicate no data in packetList now */
        m->min_next_time = now + m->byte_count * m->host_ticks_per_byte;
        m->byte_count = 0;
        if (macHostError != noErr) goto send_packet_error;
    }
    return pmNoError;
    
send_packet_error:
    pm_hosterror = macHostError;
    sprintf(pm_hosterror_text, 
            "Host error %ld: MIDISend() in midi_write()",
            (long) macHostError);
    return pmHostError;

}


static PmError
send_packet(PmInternal *midi, Byte *message, unsigned int messageLength, 
            MIDITimeStamp timestamp)
{
    PmError err;
    midi_macosxcm_type m = (midi_macosxcm_type) midi->descriptor;
    assert(m);
    
    /* printf("add %d to packet %p len %d\n", message[0], m->packet, messageLength); */
    m->packet = MIDIPacketListAdd(m->packetList, sizeof(m->packetBuffer), 
                                  m->packet, timestamp, messageLength, 
                                  message);
    m->byte_count += messageLength;
    if (m->packet == NULL) {
        /* out of space, send the buffer and start refilling it */
        /* make midi->packet non-null to fool midi_write_flush into sending */
        m->packet = (MIDIPacket *) 4; 
        /* timestamp is 0 because midi_write_flush ignores timestamp since
         * timestamps are already in packets. The timestamp parameter is here
         * because other API's need it. midi_write_flush can be called 
         * from system-independent code that must be cross-API.
         */
        if ((err = midi_write_flush(midi, 0)) != pmNoError) return err;
        m->packet = MIDIPacketListInit(m->packetList);
        assert(m->packet); /* if this fails, it's a programming error */
        m->packet = MIDIPacketListAdd(m->packetList, sizeof(m->packetBuffer),
                                      m->packet, timestamp, messageLength, 
                                      message);
        assert(m->packet); /* can't run out of space on first message */           
    }
    return pmNoError;
}    


static PmError
midi_write_short(PmInternal *midi, PmEvent *event)
{
    PmTimestamp when = event->timestamp;
    PmMessage what = event->message;
    MIDITimeStamp timestamp;
    UInt64 when_ns;
    midi_macosxcm_type m = (midi_macosxcm_type) midi->descriptor;
    Byte message[4];
    unsigned int messageLength;

    if (m->packet == NULL) {
        m->packet = MIDIPacketListInit(m->packetList);
        /* this can never fail, right? failure would indicate something 
           unrecoverable */
        assert(m->packet);
    }
    
    /* compute timestamp */
    if (when == 0) when = midi->now;
    /* if latency == 0, midi->now is not valid. We will just set it to zero */
    if (midi->latency == 0) when = 0;
    when_ns = ((UInt64) (when + midi->latency) * (UInt64) 1000000) + m->delta;
    timestamp = (MIDITimeStamp) AudioConvertNanosToHostTime(when_ns);

    message[0] = Pm_MessageStatus(what);
    message[1] = Pm_MessageData1(what);
    message[2] = Pm_MessageData2(what);
    messageLength = midi_length(what);
        
    /* make sure we go foreward in time */
    if (timestamp < m->min_next_time) timestamp = m->min_next_time;

    #ifdef LIMIT_RATE
        if (timestamp < m->last_time)
            timestamp = m->last_time;
	m->last_time = timestamp + messageLength * m->host_ticks_per_byte;
    #endif

    /* Add this message to the packet list */
    return send_packet(midi, message, messageLength, timestamp);
}


static PmError 
midi_begin_sysex(PmInternal *midi, PmTimestamp when)
{
    UInt64 when_ns;
    midi_macosxcm_type m = (midi_macosxcm_type) midi->descriptor;
    assert(m);
    m->sysex_byte_count = 0;
    
    /* compute timestamp */
    if (when == 0) when = midi->now;
    /* if latency == 0, midi->now is not valid. We will just set it to zero */
    if (midi->latency == 0) when = 0;
    when_ns = ((UInt64) (when + midi->latency) * (UInt64) 1000000) + m->delta;
    m->sysex_timestamp = (MIDITimeStamp) AudioConvertNanosToHostTime(when_ns);

    if (m->packet == NULL) {
        m->packet = MIDIPacketListInit(m->packetList);
        /* this can never fail, right? failure would indicate something 
           unrecoverable */
        assert(m->packet);
    }
    return pmNoError;
}


static PmError
midi_end_sysex(PmInternal *midi, PmTimestamp when)
{
    PmError err;
    midi_macosxcm_type m = (midi_macosxcm_type) midi->descriptor;
    assert(m);
    
    /* make sure we go foreward in time */
    if (m->sysex_timestamp < m->min_next_time) 
        m->sysex_timestamp = m->min_next_time;

    #ifdef LIMIT_RATE
        if (m->sysex_timestamp < m->last_time) 
            m->sysex_timestamp = m->last_time;
        m->last_time = m->sysex_timestamp + m->sysex_byte_count *
                                            m->host_ticks_per_byte;
    #endif
    
    /* now send what's in the buffer */
    err = send_packet(midi, m->sysex_buffer, m->sysex_byte_count,
                      m->sysex_timestamp);
    m->sysex_byte_count = 0;
    if (err != pmNoError) {
        m->packet = NULL; /* flush everything in the packet list */
        return err;
    }
    return pmNoError;
}


static PmError
midi_write_byte(PmInternal *midi, unsigned char byte, PmTimestamp timestamp)
{
    midi_macosxcm_type m = (midi_macosxcm_type) midi->descriptor;
    assert(m);
    if (m->sysex_byte_count >= SYSEX_BUFFER_SIZE) {
        PmError err = midi_end_sysex(midi, timestamp);
        if (err != pmNoError) return err;
    }
    m->sysex_buffer[m->sysex_byte_count++] = byte;
    return pmNoError;
}


static PmError
midi_write_realtime(PmInternal *midi, PmEvent *event)
{
    /* to send a realtime message during a sysex message, first
       flush all pending sysex bytes into packet list */
    PmError err = midi_end_sysex(midi, 0);
    if (err != pmNoError) return err;
    /* then we can just do a normal midi_write_short */
    return midi_write_short(midi, event);
}

static unsigned int midi_has_host_error(PmInternal *midi)
{
    midi_macosxcm_type m = (midi_macosxcm_type) midi->descriptor;
    return (m->callback_error[0] != 0) || (m->error[0] != 0);
}


static void midi_get_host_error(PmInternal *midi, char *msg, unsigned int len)
{
    midi_macosxcm_type m = (midi_macosxcm_type) midi->descriptor;
    msg[0] = 0; /* initialize to empty string */
    if (m) { /* make sure there is an open device to examine */
        if (m->error[0]) {
            strncpy(msg, m->error, len);
            m->error[0] = 0; /* clear the error */
        } else if (m->callback_error[0]) {
            strncpy(msg, m->callback_error, len);
            m->callback_error[0] = 0; /* clear the error */
        }
        msg[len - 1] = 0; /* make sure string is terminated */
    }
}


MIDITimeStamp timestamp_pm_to_cm(PmTimestamp timestamp)
{
    UInt64 nanos;
    if (timestamp <= 0) {
        return (MIDITimeStamp)0;
    } else {
        nanos = (UInt64)timestamp * (UInt64)1000000;
        return (MIDITimeStamp)AudioConvertNanosToHostTime(nanos);
    }
}

PmTimestamp timestamp_cm_to_pm(MIDITimeStamp timestamp)
{
    UInt64 nanos;
    nanos = AudioConvertHostTimeToNanos(timestamp);
    return (PmTimestamp)(nanos / (UInt64)1000000);
}


//
// Code taken from http://developer.apple.com/qa/qa2004/qa1374.html
//////////////////////////////////////
// Obtain the name of an endpoint without regard for whether it has connections.
// The result should be released by the caller.
CFStringRef EndpointName(MIDIEndpointRef endpoint, bool isExternal)
{
  CFMutableStringRef result = CFStringCreateMutable(NULL, 0);
  CFStringRef str;
  
  // begin with the endpoint's name
  str = NULL;
  MIDIObjectGetStringProperty(endpoint, kMIDIPropertyName, &str);
  if (str != NULL) {
    CFStringAppend(result, str);
    CFRelease(str);
  }
  
  MIDIEntityRef entity = NULL_REF;
  MIDIEndpointGetEntity(endpoint, &entity);
  if (entity == NULL_REF)
    // probably virtual
    return result;
  
  if (CFStringGetLength(result) == 0) {
    // endpoint name has zero length -- try the entity
    str = NULL;
    MIDIObjectGetStringProperty(entity, kMIDIPropertyName, &str);
    if (str != NULL) {
      CFStringAppend(result, str);
      CFRelease(str);
    }
  }
  // now consider the device's name
  MIDIDeviceRef device = NULL_REF;
  MIDIEntityGetDevice(entity, &device);
  if (device == NULL_REF)
    return result;
  
  str = NULL;
  MIDIObjectGetStringProperty(device, kMIDIPropertyName, &str);
  if (CFStringGetLength(result) == 0) {
      CFRelease(result);
      return str;
  }
  if (str != NULL) {
    // if an external device has only one entity, throw away
    // the endpoint name and just use the device name
    if (isExternal && MIDIDeviceGetNumberOfEntities(device) < 2) {
      CFRelease(result);
      return str;
    } else {
      if (CFStringGetLength(str) == 0) {
        CFRelease(str);
        return result;
      }
      // does the entity name already start with the device name?
      // (some drivers do this though they shouldn't)
      // if so, do not prepend
        if (CFStringCompareWithOptions( result, /* endpoint name */
             str /* device name */,
             CFRangeMake(0, CFStringGetLength(str)), 0) != kCFCompareEqualTo) {
        // prepend the device name to the entity name
        if (CFStringGetLength(result) > 0)
          CFStringInsert(result, 0, CFSTR(" "));
        CFStringInsert(result, 0, str);
      }
      CFRelease(str);
    }
  }
  return result;
}


// Obtain the name of an endpoint, following connections.
// The result should be released by the caller.
static CFStringRef ConnectedEndpointName(MIDIEndpointRef endpoint)
{
  CFMutableStringRef result = CFStringCreateMutable(NULL, 0);
  CFStringRef str;
  OSStatus err;
  long i;
  
  // Does the endpoint have connections?
  CFDataRef connections = NULL;
  long nConnected = 0;
  bool anyStrings = false;
  err = MIDIObjectGetDataProperty(endpoint, kMIDIPropertyConnectionUniqueID, &connections);
  if (connections != NULL) {
    // It has connections, follow them
    // Concatenate the names of all connected devices
    nConnected = CFDataGetLength(connections) / (int32_t) sizeof(MIDIUniqueID);
    if (nConnected) {
      const SInt32 *pid = (const SInt32 *)(CFDataGetBytePtr(connections));
      for (i = 0; i < nConnected; ++i, ++pid) {
        MIDIUniqueID id = EndianS32_BtoN(*pid);
        MIDIObjectRef connObject;
        MIDIObjectType connObjectType;
        err = MIDIObjectFindByUniqueID(id, &connObject, &connObjectType);
        if (err == noErr) {
          if (connObjectType == kMIDIObjectType_ExternalSource  ||
              connObjectType == kMIDIObjectType_ExternalDestination) {
            // Connected to an external device's endpoint (10.3 and later).
            str = EndpointName((MIDIEndpointRef)(connObject), true);
          } else {
            // Connected to an external device (10.2) (or something else, catch-all)
            str = NULL;
            MIDIObjectGetStringProperty(connObject, kMIDIPropertyName, &str);
          }
          if (str != NULL) {
            if (anyStrings)
              CFStringAppend(result, CFSTR(", "));
            else anyStrings = true;
            CFStringAppend(result, str);
            CFRelease(str);
          }
        }
      }
    }
    CFRelease(connections);
  }
  if (anyStrings)
    return result; // caller should release result

  CFRelease(result);

  // Here, either the endpoint had no connections, or we failed to obtain names for any of them.
  return EndpointName(endpoint, false);
}


char* cm_get_full_endpoint_name(MIDIEndpointRef endpoint)
{
#ifdef OLDCODE
    MIDIEntityRef entity;
    MIDIDeviceRef device;

    CFStringRef endpointName = NULL;
    CFStringRef deviceName = NULL;
#endif
    CFStringRef fullName = NULL;
    CFStringEncoding defaultEncoding;
    char* newName;

    /* get the default string encoding */
    defaultEncoding = CFStringGetSystemEncoding();

    fullName = ConnectedEndpointName(endpoint);
    
#ifdef OLDCODE
    /* get the entity and device info */
    MIDIEndpointGetEntity(endpoint, &entity);
    MIDIEntityGetDevice(entity, &device);

    /* create the nicely formated name */
    MIDIObjectGetStringProperty(endpoint, kMIDIPropertyName, &endpointName);
    MIDIObjectGetStringProperty(device, kMIDIPropertyName, &deviceName);
    if (deviceName != NULL) {
        fullName = CFStringCreateWithFormat(NULL, NULL, CFSTR("%@: %@"),
                                            deviceName, endpointName);
    } else {
        fullName = endpointName;
    }
#endif    
    /* copy the string into our buffer */
    newName = (char *) malloc(CFStringGetLength(fullName) + 1);
    CFStringGetCString(fullName, newName, CFStringGetLength(fullName) + 1,
                       defaultEncoding);

    /* clean up */
#ifdef OLDCODE
    if (endpointName) CFRelease(endpointName);
    if (deviceName) CFRelease(deviceName);
#endif
    if (fullName) CFRelease(fullName);

    return newName;
}

 

pm_fns_node pm_macosx_in_dictionary = {
    none_write_short,
    none_sysex,
    none_sysex,
    none_write_byte,
    none_write_short,
    none_write_flush,
    none_synchronize,
    midi_in_open,
    midi_abort,
    midi_in_close,
    success_poll,
    midi_has_host_error,
    midi_get_host_error,
};

pm_fns_node pm_macosx_out_dictionary = {
    midi_write_short,
    midi_begin_sysex,
    midi_end_sysex,
    midi_write_byte,
    midi_write_realtime,
    midi_write_flush,
    midi_synchronize,
    midi_out_open,
    midi_abort,
    midi_out_close,
    success_poll,
    midi_has_host_error,
    midi_get_host_error,
};


PmError pm_macosxcm_init(void)
{
    ItemCount numInputs, numOutputs, numDevices;
    MIDIEndpointRef endpoint;
    int i;
    OSStatus macHostError;
    char *error_text;

    /* Determine the number of MIDI devices on the system */
    numDevices = MIDIGetNumberOfDevices();
    numInputs = MIDIGetNumberOfSources();
    numOutputs = MIDIGetNumberOfDestinations();

    /* Return prematurely if no devices exist on the system
       Note that this is not an error. There may be no devices.
       Pm_CountDevices() will return zero, which is correct and
       useful information
     */
    if (numDevices <= 0) {
        return pmNoError;
    }


    /* Initialize the client handle */
    macHostError = MIDIClientCreate(CFSTR("PortMidi"), NULL, NULL, &client);
    if (macHostError != noErr) {
        error_text = "MIDIClientCreate() in pm_macosxcm_init()";
        goto error_return;
    }

    /* Create the input port */
    macHostError = MIDIInputPortCreate(client, CFSTR("Input port"), readProc,
                                          NULL, &portIn);
    if (macHostError != noErr) {
        error_text = "MIDIInputPortCreate() in pm_macosxcm_init()";
        goto error_return;
    }
        
    /* Create the output port */
    macHostError = MIDIOutputPortCreate(client, CFSTR("Output port"), &portOut);
    if (macHostError != noErr) {
        error_text = "MIDIOutputPortCreate() in pm_macosxcm_init()";
        goto error_return;
    }

    /* Iterate over the MIDI input devices */
    for (i = 0; i < numInputs; i++) {
        endpoint = MIDIGetSource(i);
        if (endpoint == NULL_REF) {
            continue;
        }

        /* set the first input we see to the default */
        if (pm_default_input_device_id == -1)
            pm_default_input_device_id = pm_descriptor_index;
        
        /* Register this device with PortMidi */
        pm_add_device("CoreMIDI", cm_get_full_endpoint_name(endpoint),
                      TRUE, (void *) (long) endpoint, &pm_macosx_in_dictionary);
    }

    /* Iterate over the MIDI output devices */
    for (i = 0; i < numOutputs; i++) {
        endpoint = MIDIGetDestination(i);
        if (endpoint == NULL_REF) {
            continue;
        }

        /* set the first output we see to the default */
        if (pm_default_output_device_id == -1)
            pm_default_output_device_id = pm_descriptor_index;

        /* Register this device with PortMidi */
        pm_add_device("CoreMIDI", cm_get_full_endpoint_name(endpoint),
                      FALSE, (void *) (long) endpoint,
                      &pm_macosx_out_dictionary);
    }
    return pmNoError;
    
error_return:
    pm_hosterror = macHostError;
    sprintf(pm_hosterror_text, "Host error %ld: %s\n", (long) macHostError, 
            error_text);
    pm_macosxcm_term(); /* clear out any opened ports */
    return pmHostError;
}

void pm_macosxcm_term(void)
{
    if (client != NULL_REF) MIDIClientDispose(client);
    if (portIn != NULL_REF) MIDIPortDispose(portIn);
    if (portOut != NULL_REF) MIDIPortDispose(portOut);
}
/*

readbinaryplist.c -- Roger B. Dannenberg, Jun 2008
Based on ReadBinaryPList.m by Jens Ayton, 2007

Note that this code is intended to read preference files and has an upper
bound on file size (currently 100MB) and assumes in some places that 32 bit
offsets are sufficient.

Here are his comments:

Reader for binary property list files (version 00).

This has been found to work on all 566 binary plists in my ~/Library/Preferences/
and /Library/Preferences/ directories. This probably does not provide full
test coverage. It has also been found to provide different data to Apple's
implementation when presented with a key-value archive. This is because Apple's
implementation produces undocumented CFKeyArchiverUID objects. My implementation
produces dictionaries instead, matching the in-file representation used in XML
and OpenStep plists. See extract_uid().

Full disclosure: in implementing this software, I read one comment and one
struct defintion in CFLite, Apple's implementation, which is under the APSL
license. I also deduced the information about CFKeyArchiverUID from that code.
However, none of the implementation was copied.

Copyright (C) 2007 Jens Ayton

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*/

/* A note about memory management:
Strings and possibly other values are unique and because the values
associated with IDs are cached, you end up with a directed graph rather
than a tree. It is tricky to free the data because if you do a simple
depth-first search to free nodes, you will free nodes twice. I decided
to allocate memory from blocks of 1024 bytes and keep the blocks in a
list associated with but private to this module. So the user should
access this module by calling:
    bplist_read_file() or bplist_read_user_pref() or 
    bplist_read_system_pref()
which returns a value. When you are done with the value, call
    bplist_free_data()
This will of course free the value_ptr returned by bplist_read_*()

To deal with memory exhaustion (what happens when malloc returns
NULL?), use setjmp/longjmp -- a single setjmp protects the whole
parser, and allocate uses longjmp to abort. After abort, memory
is freed and NULL is returned to caller. There is not much here
in the way of error reporting.

Memory is obtained by calling allocate which either returns the
memory requested or calls longjmp, so callers don't have to check.

*/

#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <sys/stat.h>
// (amalg) #include "readbinaryplist.h"
#include <Carbon/Carbon.h>

#define NO 0
#define YES 1
#define BOOL int

#define MAXPATHLEN 256

/* there are 2 levels of error logging/printing:
 *   BPLIST_LOG and BPLIST_LOG_VERBOSE
 * either or both can be set to non-zero to turn on
 * If BPLIST_LOG_VERBOSE is true, then BPLIST_LOG 
 * is also true.
 * 
 * In the code, logging is done by calling either
 * bplist_log() or bplist_log_verbose(), which take
 * parameters like printf but might be a no-op.
 */
 
/* #define BPLIST_LOG_VERBOSE 1 */

#if BPLIST_LOG_VERBOSE
    #ifndef BPLIST_LOG
        #define BPLIST_LOG 1
    #endif
#endif

#if BPLIST_LOG
    #define bplist_log printf
#else
    #define bplist_log(...)
#endif

#if BPLIST_LOG_VERBOSE
    #define bplist_log_verbose bplist_log
#else
    #define bplist_log_verbose(...)
#endif


/********* MEMORY MANAGEMENT ********/
#define BLOCK_SIZE 1024
// memory is aligned to multiples of this; assume malloc automatically
// aligns to this number and assume this number is > sizeof(void *)
#define ALIGNMENT 8
static void *block_list = NULL;
static char *free_ptr = NULL;
static char *end_ptr = NULL;
static jmp_buf abort_parsing;

static void *allocate(size_t size)
{
    void *result;
    if (free_ptr + size > end_ptr) {
        size_t how_much = BLOCK_SIZE;
        // align everything to 8 bytes
        if (size > BLOCK_SIZE - ALIGNMENT) {
            how_much = size + ALIGNMENT;
        }
        result = malloc(how_much);
        if (result == NULL) {
            /* serious problem */
            longjmp(abort_parsing, 1);
        }
        *((void **)result) = block_list;
        block_list = result;
        free_ptr = ((char *) result) + ALIGNMENT;
        end_ptr = ((char *) result) + how_much;
    }
    // now, there is enough rooom at free_ptr
    result = free_ptr;
    free_ptr += size;
    return result;
}

void bplist_free_data()
{
    while (block_list) {
        void *next = *(void **)block_list;
        free(block_list);
        block_list = next;
    }
    free_ptr = NULL;
    end_ptr = NULL;
}

// layout of trailer -- last 32 bytes in plist data
    uint8_t unused[6];
    uint8_t offset_int_size;
    uint8_t object_ref_size;
    uint64_t object_count;
    uint64_t top_level_object;
    uint64_t offset_table_offset;


enum
{
    kHEADER_SIZE = 8,
    kTRAILER_SIZE = 32, //sizeof(bplist_trailer_node),
    kMINIMUM_SANE_SIZE = kHEADER_SIZE + kTRAILER_SIZE
};


static const char kHEADER_BYTES[kHEADER_SIZE] = "bplist00";

// map from UID key to previously parsed value
typedef struct cache_struct {
    uint64_t key;
    value_ptr value;
    struct cache_struct *next;
} cache_node, *cache_ptr;


typedef struct bplist_info
{
    uint64_t object_count;
    const uint8_t *data_bytes;
    uint64_t length;
    uint64_t offset_table_offset;
    uint8_t offset_int_size;
    uint8_t object_ref_size;
    cache_ptr cache;
} bplist_info_node, *bplist_info_ptr;


static value_ptr bplist_read_pldata(pldata_ptr data);
static value_ptr bplist_read_pref(char *filename, OSType folder_type);
static uint64_t read_sized_int(bplist_info_ptr bplist, uint64_t offset, uint8_t size);
static uint64_t read_offset(bplist_info_ptr bplist, uint64_t index);
static BOOL read_self_sized_int(bplist_info_ptr bplist, uint64_t offset, uint64_t *outValue, size_t *outSize);

static value_ptr extract_object(bplist_info_ptr bplist, uint64_t objectRef);
static value_ptr extract_simple(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_int(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_real(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_date(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_data(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_ascii_string(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_unicode_string(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_uid(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_array(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_dictionary(bplist_info_ptr bplist, uint64_t offset);


value_ptr value_create()
{
    value_ptr value = (value_ptr) allocate(sizeof(value_node));
    return value;
}


void value_set_integer(value_ptr v, int64_t i) {
    v->tag = kTAG_INT; v->integer = i;
}

void value_set_real(value_ptr v, double d) {
    v->tag = kTAG_REAL; v->real = d;
}

// d is seconds since 1 January 2001
void value_set_date(value_ptr v, double d) {
    v->tag = kTAG_DATE; v->real = d;
}

void value_set_ascii_string(value_ptr v, const uint8_t *s, size_t len) {
    v->tag = kTAG_ASCIISTRING;
    v->string = (char *) allocate(len + 1);
    memcpy(v->string, s, len);
    v->string[len] = 0;
}

void value_set_unicode_string(value_ptr v, const uint8_t *s, size_t len) {
    v->tag = kTAG_UNICODESTRING;
    v->string = (char *) allocate(len + 1);
    memcpy(v->string, s, len);
    v->string[len] = 0;
}

void value_set_uid(value_ptr v, uint64_t uid)
{
    v->tag = kTAG_UID; v->uinteger = uid;
}

// v->data points to a pldata that points to the actual bytes
// the bytes are copied, so caller must free byte source (*data)
void value_set_data(value_ptr v, const uint8_t *data, size_t len) {
    v->tag = kTAG_DATA;
    pldata_ptr pldata = (pldata_ptr) allocate(sizeof(pldata_node));
    pldata->data = (uint8_t *) allocate(len);
    memcpy(pldata->data, data, len);
    pldata->len = len;
    v->data = pldata;
    printf("value at %p gets data at %p\n", v, pldata);
}

// caller releases ownership of array to value_ptr v
void value_set_array(value_ptr v, value_ptr *array, size_t length) {
    array_ptr a = (array_ptr) allocate(sizeof(array_node));
    a->array = array;
    a->length = length;
    v->tag = kTAG_ARRAY;
    v->array = a;
}

// caller releases ownership of dict to value_ptr v
void value_set_dict(value_ptr v, dict_ptr dict) {
    v->tag = kTAG_DICTIONARY;
    v->dict = dict;
}


// look up an objectref in the cache, a ref->value_ptr mapping
value_ptr cache_lookup(cache_ptr cache, uint64_t ref)
{
    while (cache) {
        if (cache->key == ref) {
            return cache->value;
        }
        cache = cache->next;
    }
    return NULL;
}


// insert an objectref and value in the cache
void cache_insert(cache_ptr *cache, uint64_t ref, value_ptr value)
{
    cache_ptr c = (cache_ptr) allocate(sizeof(cache_node));
    c->key = ref;
    c->value = value;
    c->next = *cache;
    *cache = c;
}


// insert an objectref and value in a dictionary
void dict_insert(dict_ptr *dict, value_ptr key, value_ptr value)
{
    dict_ptr d = (dict_ptr) allocate(sizeof(dict_node));
    d->key = key;
    d->value = value;
    d->next = *dict;
    *dict = d;
}


BOOL is_binary_plist(pldata_ptr data)
{
    if (data->len < kMINIMUM_SANE_SIZE)  return NO;
    return memcmp(data->data, kHEADER_BYTES, kHEADER_SIZE) == 0;
}


value_ptr bplist_read_file(char *filename)
{
    struct stat stbuf;
    pldata_node pldata;
    FILE *file;
    size_t n;
    value_ptr value;
    int rslt = stat(filename, &stbuf);
    if (rslt) {
        #if BPLIST_LOG
            perror("in stat");
        #endif
        bplist_log("Could not stat %s, error %d\n", filename, rslt);
        return NULL;
    }
    // if file is >100MB, assume it is not a preferences file and give up
    if (stbuf.st_size > 100000000) {
        bplist_log("Large file %s encountered (%llu bytes) -- not read\n",
                   filename, stbuf.st_size);
        return NULL;
    }
    pldata.len = (size_t) stbuf.st_size;
    // note: this is supposed to be malloc, not allocate. It is separate
    // from the graph structure, large, and easy to free right after
    // parsing.
    pldata.data = (uint8_t *) malloc(pldata.len);
    if (!pldata.data) {
        bplist_log("Could not allocate %lu bytes for %s\n",
                   (unsigned long) pldata.len, filename);
        return NULL;
    }
    file = fopen(filename, "rb");
    if (!file) {
        bplist_log("Could not open %s\n", filename);
        return NULL;
    }
    n = fread(pldata.data, 1, pldata.len, file);
    if (n != pldata.len) {
        bplist_log("Error reading from %s\n", filename);
        return NULL;
    }
    value = bplist_read_pldata(&pldata);
    free(pldata.data);
    return value;
}


value_ptr bplist_read_pref(char *filename, OSType folder_type)
{
    FSRef prefdir;
    char cstr[MAXPATHLEN];

    OSErr err = FSFindFolder(kOnAppropriateDisk, folder_type,
                             FALSE, &prefdir);
    if (err) {
        bplist_log("Error finding preferences folder: %d\n", err);
        return NULL;
    }
    err = FSRefMakePath(&prefdir, (UInt8 *) cstr, (UInt32) (MAXPATHLEN - 1));
    if (err) {
        bplist_log("Error making path name for preferences folder: %d\n", err);
        return NULL;
    }
    strlcat(cstr, "/", MAXPATHLEN);
    strlcat(cstr, filename, MAXPATHLEN);
    return bplist_read_file(cstr);
}


value_ptr bplist_read_system_pref(char *filename) {
    return bplist_read_pref(filename, kSystemPreferencesFolderType);
}


value_ptr bplist_read_user_pref(char *filename) {
    return bplist_read_pref(filename, kPreferencesFolderType);
}


// data is stored with high-order bytes first.
// read from plist data in a machine-independent fashion
//
uint64_t convert_uint64(uint8_t *ptr)
{
    uint64_t rslt = 0;
    int i;
    // shift in bytes, high-order first
    for (i = 0; i < sizeof(uint64_t); i++) {
        rslt <<= 8;
        rslt += ptr[i];
    }
    return rslt;
}


value_ptr bplist_read_pldata(pldata_ptr data)
{
    value_ptr result = NULL;
    bplist_info_node bplist;
    uint8_t *ptr;
    uint64_t top_level_object;
    int i;

    if (data == NULL)  return NULL;
    if (!is_binary_plist(data)) {
        bplist_log("Bad binary plist: too short or invalid header.\n");
        return NULL;
    }
        
    // read trailer
    ptr = (uint8_t *) (data->data + data->len - kTRAILER_SIZE);
    bplist.offset_int_size = ptr[6];
    bplist.object_ref_size = ptr[7];
    bplist.object_count = convert_uint64(ptr + 8);
    top_level_object = convert_uint64(ptr + 16);
    bplist.offset_table_offset = convert_uint64(ptr + 24);
        
    // Basic sanity checks
    if (bplist.offset_int_size < 1 || bplist.offset_int_size > 8 ||
        bplist.object_ref_size < 1 || bplist.object_ref_size > 8 ||
        bplist.offset_table_offset < kHEADER_SIZE) {
        bplist_log("Bad binary plist: trailer declared insane.\n");
        return NULL;                
    }
        
    // Ensure offset table is inside file
    uint64_t offsetTableSize = bplist.offset_int_size * bplist.object_count;
    if (offsetTableSize + bplist.offset_table_offset + kTRAILER_SIZE > 
        data->len) {
        bplist_log("Bad binary plist: offset table overlaps end of container.\n");
        return NULL;
    }
        
    bplist.data_bytes = data->data;
    bplist.length = data->len;
    bplist.cache = NULL; /* dictionary is empty */

    bplist_log_verbose("Got a sane bplist with %llu items, offset_int_size: %u, object_ref_size: %u\n", 
                      bplist.object_count, bplist.offset_int_size, 
                      bplist.object_ref_size);
    /* at this point, we are ready to do some parsing which allocates
        memory for the result data structure. If memory allocation (using
        allocate fails, a longjmp will return to here and we simply give up
     */
    i = setjmp(abort_parsing);
    if (i == 0) {
        result = extract_object(&bplist, top_level_object);
    } else {
        bplist_log("allocate() failed to allocate memory. Giving up.\n");
        result = NULL;
    }
    if (!result) {
        bplist_free_data();
    }
    return result;
}


static value_ptr extract_object(bplist_info_ptr bplist, uint64_t objectRef)
{
    uint64_t offset;
    value_ptr result = NULL;
    uint8_t objectTag;
    
    if (objectRef >= bplist->object_count) {
        // Out-of-range object reference.
        bplist_log("Bad binary plist: object index is out of range.\n");
        return NULL;
    }
        
    // Use cached object if it exists
    result = cache_lookup(bplist->cache, objectRef);
    if (result != NULL)  return result;
        
    // Otherwise, find object in file.
    offset = read_offset(bplist, objectRef);
    if (offset > bplist->length) {
        // Out-of-range offset.
        bplist_log("Bad binary plist: object outside container.\n");
        return NULL;
    }
    objectTag = *(bplist->data_bytes + offset);
    switch (objectTag & 0xF0) {
    case kTAG_SIMPLE:
        result = extract_simple(bplist, offset);
        break;
                
    case kTAG_INT:
        result = extract_int(bplist, offset);
        break;
                        
    case kTAG_REAL:
        result = extract_real(bplist, offset);
        break;
                        
    case kTAG_DATE:
        result = extract_date(bplist, offset);
        break;
                        
    case kTAG_DATA:
        result = extract_data(bplist, offset);
        break;
                        
    case kTAG_ASCIISTRING:
        result = extract_ascii_string(bplist, offset);
        break;
                        
    case kTAG_UNICODESTRING:
        result = extract_unicode_string(bplist, offset);
        break;
        
    case kTAG_UID:
        result = extract_uid(bplist, offset);
        break;
        
    case kTAG_ARRAY:
        result = extract_array(bplist, offset);
        break;
        
    case kTAG_DICTIONARY:
        result = extract_dictionary(bplist, offset);
        break;
        
    default:
        // Unknown tag.
        bplist_log("Bad binary plist: unknown tag 0x%X.\n", 
                   (objectTag & 0x0F) >> 4);
        result = NULL;
    }
    
    // Cache and return result.
    if (result != NULL)  
        cache_insert(&bplist->cache, objectRef, result);
    return result;
}


static uint64_t read_sized_int(bplist_info_ptr bplist, uint64_t offset, 
                               uint8_t size)
{
    assert(bplist->data_bytes != NULL && size >= 1 && size <= 8 && 
           offset + size <= bplist->length);
        
    uint64_t result = 0;
    const uint8_t *byte = bplist->data_bytes + offset;
        
    do {
        // note that ints seem to be high-order first
        result = (result << 8) | *byte++;
    } while (--size);
        
    return result;
}


static uint64_t read_offset(bplist_info_ptr bplist, uint64_t index)
{
    assert(index < bplist->object_count);
        
    return read_sized_int(bplist, 
            bplist->offset_table_offset + bplist->offset_int_size * index, 
            bplist->offset_int_size);
}


static BOOL read_self_sized_int(bplist_info_ptr bplist, uint64_t offset, 
                             uint64_t *outValue, size_t *outSize)
{
    uint32_t size;
    int64_t value;
        
    assert(bplist->data_bytes != NULL && offset < bplist->length);
        
    size = 1 << (bplist->data_bytes[offset] & 0x0F);
    if (size > 8) {
        // Maximum allowable size in this implementation is 1<<3 = 8 bytes.
        // This also happens to be the biggest we can handle.
        return NO;
    }
        
    if (offset + 1 + size > bplist->length) {
        // Out of range.
        return NO;
    }
        
    value = read_sized_int(bplist, offset + 1, size);
    
    if (outValue != NULL) *outValue = value;
    if (outSize != NULL) *outSize = size + 1; // +1 for tag byte.
    return YES;
}


static value_ptr extract_simple(bplist_info_ptr bplist, uint64_t offset)
{
    assert(bplist->data_bytes != NULL && offset < bplist->length);
    value_ptr value = value_create();
        
    switch (bplist->data_bytes[offset]) {
    case kVALUE_NULL:
        value->tag = kVALUE_NULL;
        return value;
        
    case kVALUE_TRUE:
        value->tag = kVALUE_TRUE;
        return value;
                        
    case kVALUE_FALSE:
        value->tag = kVALUE_FALSE;
        return value;
    }
        
    // Note: kVALUE_FILLER is treated as invalid, because it, er, is.
    bplist_log("Bad binary plist: invalid atom.\n");
    free(value);
    return NULL;
}


static value_ptr extract_int(bplist_info_ptr bplist, uint64_t offset)
{
    value_ptr value = value_create();
    value->tag = kTAG_INT;

    if (!read_self_sized_int(bplist, offset, &value->uinteger, NULL)) {
        bplist_log("Bad binary plist: invalid integer object.\n");
    }
        
    /* NOTE: originally, I sign-extended here. This was the wrong thing; it
       turns out that negative ints are always stored as 64-bit, and smaller
       ints are unsigned.
    */
    return value;
}


static value_ptr extract_real(bplist_info_ptr bplist, uint64_t offset)
{
    value_ptr value = value_create();
    uint32_t size;
        
    assert(bplist->data_bytes != NULL && offset < bplist->length);
    
    size = 1 << (bplist->data_bytes[offset] & 0x0F);
        
    // FIXME: what to do if faced with other sizes for float/double?
    assert (sizeof (float) == sizeof (uint32_t) && 
            sizeof (double) == sizeof (uint64_t));
        
    if (offset + 1 + size > bplist->length) {
        bplist_log("Bad binary plist: %s object overlaps end of container.\n", 
                  "floating-point number");
        free(value);
        return NULL;
    }
        
    if (size == sizeof (float)) {
        // cast is ok because we know size is 4 bytes
        uint32_t i = (uint32_t) read_sized_int(bplist, offset + 1, size); 
        // Note that this handles byte swapping.
        value_set_real(value, *(float *)&i);
        return value;
    } else if (size == sizeof (double)) {
        uint64_t i = read_sized_int(bplist, offset + 1, size);
        // Note that this handles byte swapping.
        value_set_real(value, *(double *)&i);
        return value;
    } else {
        // Can't handle floats of other sizes.
        bplist_log("Bad binary plist: can't handle %u-byte float.\n", size);
        free(value);
        return NULL;
    }
}


static value_ptr extract_date(bplist_info_ptr bplist, uint64_t offset)
{
    value_ptr value;
    assert(bplist->data_bytes != NULL && offset < bplist->length);
        
    // Data has size code like int and real, but only 3 (meaning 8 bytes) is valid.
    if (bplist->data_bytes[offset] != kVALUE_FULLDATETAG) {
        bplist_log("Bad binary plist: invalid size for date object.\n");
        return NULL;
    }
        
    if (offset + 1 + sizeof (double) > bplist->length) {
        bplist_log("Bad binary plist: %s object overlaps end of container.\n", 
                  "date");
        return NULL;
    }
        
    // FIXME: what to do if faced with other sizes for double?
    assert (sizeof (double) == sizeof (uint64_t));
        
    uint64_t date = read_sized_int(bplist, offset + 1, sizeof(double));
    // Note that this handles byte swapping.
    value = value_create();
    value_set_date(value, *(double *)&date);
    return value;
}


uint64_t bplist_get_a_size(bplist_info_ptr bplist, 
                           uint64_t *offset_ptr, char *msg)
{
    uint64_t size = bplist->data_bytes[*offset_ptr] & 0x0F;
    (*offset_ptr)++;
    if (size == 0x0F) {
        // 0x0F means separate int size follows. 
        // Smaller values are used for short data.
        size_t extra; // the length of the data size we are about to read
        if ((bplist->data_bytes[*offset_ptr] & 0xF0) != kTAG_INT) {
            // Bad data, mistagged size int
            bplist_log("Bad binary plist: %s object size is not tagged as int.\n",
                       msg);
            return UINT64_MAX; // error
        }
                
        // read integer data as size, extra tells how many bytes to skip
        if (!read_self_sized_int(bplist, *offset_ptr, &size, &extra)) {
            bplist_log("Bad binary plist: invalid %s object size tag.\n", 
                      "data");
            return UINT64_MAX; // error
        }
        (*offset_ptr) += extra;
    }

    if (*offset_ptr + size > bplist->length) {
        bplist_log("Bad binary plist: %s object overlaps end of container.\n", 
                  "data");
        return UINT64_MAX; // error
    }
    return size;
}


static value_ptr extract_data(bplist_info_ptr bplist, uint64_t offset)
{
    uint64_t size;
    value_ptr value;
        
    assert(bplist->data_bytes != NULL && offset < bplist->length);
        
    if ((size = bplist_get_a_size(bplist, &offset, "data")) == UINT64_MAX) 
        return NULL;
        
    value = value_create();
    // cast is ok because we only allow files up to 100MB:
    value_set_data(value, bplist->data_bytes + (size_t) offset, (size_t) size);
    return value;
}


static value_ptr extract_ascii_string(bplist_info_ptr bplist, uint64_t offset)
{
    uint64_t size;
    value_ptr value; // return value
        
    assert(bplist->data_bytes != NULL && offset < bplist->length);
        
    if ((size = bplist_get_a_size(bplist, &offset, "ascii string")) ==
        UINT64_MAX) 
        return NULL;

    value = value_create();
    // cast is ok because we only allow 100MB files
    value_set_ascii_string(value, bplist->data_bytes + (size_t) offset, 
                           (size_t) size);
    return value;
}


static value_ptr extract_unicode_string(bplist_info_ptr bplist, uint64_t offset)
{
    uint64_t size;
    value_ptr value;
        
    assert(bplist->data_bytes != NULL && offset < bplist->length);
        
    if ((size = bplist_get_a_size(bplist, &offset, "unicode string")) == 
        UINT64_MAX)
        return NULL;
        
    value = value_create();
    // cast is ok because we only allow 100MB files
    value_set_unicode_string(value, bplist->data_bytes + (size_t) offset, 
                             (size_t) size);
    return value;
}


static value_ptr extract_uid(bplist_info_ptr bplist, uint64_t offset)
{
    /* UIDs are used by Cocoa's key-value coder.
       When writing other plist formats, they are expanded to dictionaries of
       the form <dict><key>CF$UID</key><integer>value</integer></dict>, so we
       do the same here on reading. This results in plists identical to what
       running plutil -convert xml1 gives us. However, this is not the same
       result as [Core]Foundation's plist parser, which extracts them as un-
       introspectable CF objects. In fact, it even seems to convert the CF$UID
       dictionaries from XML plists on the fly.
    */
        
    value_ptr value;
    uint64_t uid;
        
    if (!read_self_sized_int(bplist, offset, &uid, NULL)) {
        bplist_log("Bad binary plist: invalid UID object.\n");
        return NULL;
    }
        
    // assert(NO); // original code suggests using a string for a key
    // but our dictionaries all use big ints for keys, so I don't know
    // what to do here
    
    // In practice, I believe this code is never executed by PortMidi.
    // I changed it to do something and not raise compiler warnings, but
    // not sure what the code should do.

    value = value_create();
    value_set_uid(value, uid);
    // return [NSDictionary dictionaryWithObject:
    //         [NSNumber numberWithUnsignedLongLong:value] 
    //         forKey:"CF$UID"];
    return value;
}


static value_ptr extract_array(bplist_info_ptr bplist, uint64_t offset)
{
    uint64_t i, count;
    uint64_t size;
    uint64_t elementID;
    value_ptr element = NULL;
    value_ptr *array = NULL;
    value_ptr value = NULL;
    BOOL ok = YES;
        
    assert(bplist->data_bytes != NULL && offset < bplist->length);
        
    if ((count = bplist_get_a_size(bplist, &offset, "array")) == UINT64_MAX)
        return NULL;
        
    if (count > UINT64_MAX / bplist->object_ref_size - offset) {
        // Offset overflow.
        bplist_log("Bad binary plist: %s object overlaps end of container.\n", 
                   "array");
        return NULL;
    }
        
    size = bplist->object_ref_size * count;
    if (size + offset > bplist->length) {
        bplist_log("Bad binary plist: %s object overlaps end of container.\n", 
                   "array");
        return NULL;
    }
        
    // got count, the number of array elements

    value = value_create();
    assert(value);

    if (count == 0) {
        // count must be size_t or smaller because max file size is 100MB
        value_set_array(value, array, (size_t) count);
        return value;
    }
        
    array = allocate(sizeof(value_ptr) * (size_t) count);
        
    for (i = 0; i != count; ++i) {
        bplist_log_verbose("[%u]\n", i);
        elementID = read_sized_int(bplist, offset + i * bplist->object_ref_size, 
                                 bplist->object_ref_size);
        element = extract_object(bplist, elementID);
        if (element != NULL) {
            array[i] = element;
        } else {
            ok = NO;
            break;
        }
    }
    if (ok) { // count is smaller than size_t max because of 100MB file limit
        value_set_array(value, array, (size_t) count);
    }

    return value;
}


static value_ptr extract_dictionary(bplist_info_ptr bplist, uint64_t offset)
{
    uint64_t i, count;
    uint64_t size;
    uint64_t elementID;
    value_ptr value = NULL;
    dict_ptr dict = NULL;
    BOOL ok = YES;
        
    assert(bplist->data_bytes != NULL && offset < bplist->length);
        
        
    if ((count = bplist_get_a_size(bplist, &offset, "array")) == UINT64_MAX)
        return NULL;

    if (count > UINT64_MAX / (bplist->object_ref_size * 2) - offset) {
        // Offset overflow.
        bplist_log("Bad binary plist: %s object overlaps end of container.\n", 
                   "dictionary");
        return NULL;
    }
    
    size = bplist->object_ref_size * count * 2;
    if (size + offset > bplist->length) {
        bplist_log("Bad binary plist: %s object overlaps end of container.\n", 
                   "dictionary");
        return NULL;
    }
    
    value = value_create();
    if (count == 0) {
        value_set_dict(value, NULL);
        return value;
    }

    for (i = 0; i != count; ++i) {
        value_ptr key;
        value_ptr val;
        elementID = read_sized_int(bplist, offset + i * bplist->object_ref_size, 
                                 bplist->object_ref_size);
        key = extract_object(bplist, elementID);
        if (key != NULL) {
            bplist_log_verbose("key: %p\n", key);
        } else {
            ok = NO;
            break;
        }
                    
        elementID = read_sized_int(bplist, 
                            offset + (i + count) * bplist->object_ref_size, 
                            bplist->object_ref_size);
        val = extract_object(bplist, elementID);
        if (val != NULL) {
            dict_insert(&dict, key, val);
        } else {
            ok = NO;
            break;
        }
    }
    if (ok) {
        value_set_dict(value, dict);
    }
    
    return value;
}

/*************** functions for accessing values ****************/


char *value_get_asciistring(value_ptr v)
{
    if (v->tag != kTAG_ASCIISTRING) return NULL;
    return v->string;
}


value_ptr value_dict_lookup_using_string(value_ptr v, char *key)
{
    dict_ptr dict;
    if (v->tag != kTAG_DICTIONARY) return NULL; // not a dictionary
    dict = v->dict;
    /* search for key */
    while (dict) {
        if (dict->key && dict->key->tag == kTAG_ASCIISTRING &&
            strcmp(key, dict->key->string) == 0) { // found it
            return dict->value;
        }
        dict = dict->next;
    }
    return NULL; /* not found */
}

value_ptr value_dict_lookup_using_path(value_ptr v, char *path)
{
    char key[MAX_KEY_SIZE];
    while (*path) { /* more to the path */
        int i = 0;
        while (i < MAX_KEY_SIZE - 1) {
            key[i] = *path++;
            if (key[i] == '/') { /* end of entry in path */
                key[i + 1] = 0;
                break;
            }
            if (!key[i]) {
                path--; /* back up to end of string char */
                break;  /* this will cause outer loop to exit */
            }
            i++;
        }
        if (!v || v->tag != kTAG_DICTIONARY) return NULL;
        /* now, look up the key to get next value */
        v = value_dict_lookup_using_string(v, key);
        if (v == NULL) return NULL;
    }
    return v;
}
                

/*************** functions for debugging ***************/

void plist_print(value_ptr v)
{
    size_t i;
    int comma_needed;
    dict_ptr dict;
    if (!v) {
        printf("NULL");
        return;
    }
    switch (v->tag & 0xF0) {
    case kTAG_SIMPLE:
        switch (v->tag) {
        case kVALUE_NULL: 
            printf("NULL@%p", v); break;
        case kVALUE_FALSE: 
            printf("FALSE@%p", v); break;
        case kVALUE_TRUE:
            printf("TRUE@%p", v); break;
        default:
            printf("UNKNOWN tag=%x@%p", v->tag, v); break;
        }
        break;
    case kTAG_INT:
        printf("%lld@%p", v->integer, v); break;
    case kTAG_REAL:
        printf("%g@%p", v->real, v); break;
    case kTAG_DATE:
        printf("date:%g@%p", v->real, v); break;
    case kTAG_DATA:
        printf("data@%p->%p:[%p:", v, v->data, v->data->data);
        for (i = 0; i < v->data->len; i++) {
            printf(" %2x", v->data->data[i]);
        }
        printf("]"); break;
    case kTAG_ASCIISTRING:
        printf("%p:\"%s\"@%p", v->string, v->string, v); break;
    case kTAG_UNICODESTRING:
        printf("unicode:%p:\"%s\"@%p", v->string, v->string, v); break;
    case kTAG_UID:
        printf("UID:%llu@%p", v->uinteger, v); break;
    case kTAG_ARRAY:
        comma_needed = FALSE;
        printf("%p->%p:[%p:", v, v->array, v->array->array);
        for (i = 0; i < v->array->length; i++) {
            if (comma_needed) printf(", ");
            plist_print(v->array->array[i]);
            comma_needed = TRUE;
        }
        printf("]"); break;
    case kTAG_DICTIONARY:
        comma_needed = FALSE;
        printf("%p:[", v);
        dict = v->dict;
        while (dict) {
            if (comma_needed) printf(", ");
            printf("%p:", dict);
            plist_print(dict->key);
            printf("->");
            plist_print(dict->value);
            comma_needed = TRUE;
            dict = dict->next;
        }
        printf("]"); break;
    default:
        printf("UNKNOWN tag=%x", v->tag);
        break;
    }
}

            
/* finddefault.c -- find_default_device() implementation
   Roger Dannenberg, June 2008
*/

#include <stdlib.h>
#include <string.h>
// (amalg) #include "portmidi.h"
// (amalg) #include "pmutil.h"
// (amalg) #include "pminternal.h"
// (amalg) #include "pmmacosxcm.h"
// (amalg) #include "readbinaryplist.h"

/* Parse preference files, find default device, search devices --
   This parses the preference file(s) once for input and once for
   output, which is inefficient but much simpler to manage. Note
   that using the readbinaryplist.c module, you cannot keep two
   plist files (user and system) open at once (due to a simple
   memory management scheme).
*/
PmDeviceID find_default_device(char *path, int input, PmDeviceID id)
/* path -- the name of the preference we are searching for
   input -- true iff this is an input device
   id -- current default device id
   returns matching device id if found, otherwise id
*/
{
    static char *pref_file = "com.apple.java.util.prefs.plist";
    char *pref_str = NULL;
    // read device preferences
    value_ptr prefs = bplist_read_user_pref(pref_file);
    if (prefs) {
        value_ptr pref_val = value_dict_lookup_using_path(prefs, path);
        if (pref_val) {
            pref_str = value_get_asciistring(pref_val);
        }
    }
    if (!pref_str) {
        bplist_free_data(); /* look elsewhere */
        prefs = bplist_read_system_pref(pref_file);
        if (prefs) {
            value_ptr pref_val = value_dict_lookup_using_path(prefs, path);
            if (pref_val) {
                pref_str = value_get_asciistring(pref_val);
            }
        }
    }
    if (pref_str) { /* search devices for match */
        int i = pm_find_default_device(pref_str, input);
        if (i != pmNoDevice) {
            id = i;
	}
    }
    if (prefs) {
        bplist_free_data();
    }
    return id;
}
#endif
