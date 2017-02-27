ifeq ($(jack),1)
	CXXFLAGS += -D__UNIX_JACK__
	LDFLAGS += -lm -ljack -pthread
endif
ifeq ($(linux),1)
	CXXFLAGS += -D__LINUX_ALSA__
	LDFLAGS += -lm -lasound -pthread
endif
ifeq ($(macos),1)
	CXXFLAGS += -D__MAXOSX_CORE__
	LDFLAGS += -lm -framework CoreAudio -framework CoreMIDI -framework CoreFoundation
endif
ifeq ($(windows),1)
	CXXFLAGS += -D__WINDOWS_WASAPI__
	LDFLAGS += -lm -lole32 -lksuser -lwinmm -lws2_32 -mwindows -static
endif

CFLAGS += -g
CXXFLAGS += -g
LDFLAGS += -lstdc++

example: example.o ../RtAudio.o ../rtaudio_c.o
	$(CC) $^ $(LDFLAGS) -o $@
clean:
	rm -f example example.exe example.o ../RtAudio.o ../rtaudio_c.o
