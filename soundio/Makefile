ifeq ($(jack),1)
	CFLAGS += -DSOUNDIO_HAVE_JACK=1
	LDFLAGS += -lm -ljack -pthread
endif
ifeq ($(linux),1)
	CFLAGS += -DSOUNDIO_HAVE_ALSA=1
	LDFLAGS += -lm -lasound -pthread
endif
ifeq ($(macos),1)
	CFLAGS += -DSOUNDIO_HAVE_COREAUDIO=1
	LDFLAGS += -lm -framework CoreAudio -framework CoreMIDI -framework CoreFoundation
endif
ifeq ($(windows),1)
	CFLAGS += -DSOUNDIO_HAVE_WASAPI=1
	LDFLAGS += -lm -lole32 -lksuser -lwinmm -lws2_32 -mwindows -static
endif

example: example.o soundio_amalg.o
	$(CC) $^ $(LDFLAGS) -o $@
clean:
	rm -f example *.o
