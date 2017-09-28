ifeq ($(linux),1)
	LDFLAGS += -lasound -pthread
endif
ifeq ($(macos),1)
	LDFLAGS += -framework CoreMIDI -framework CoreFoundation
endif
ifeq ($(windows),1)
	LDFLAGS += -lm -lole32 -lksuser -lwinmm -lws2_32 -mwindows -static
endif

example: example.o ../pm_amalg.o
	$(CC) $^ $(LDFLAGS) -o $@
clean:
	rm -f example example.o ../pm_amalg.o
