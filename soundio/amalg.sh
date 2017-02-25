#!/bin/sh

AMALG_H=soundio.h
AMALG_C=soundio_amalg.c

echo "Downloading libsoundio sources..."
git clone https://github.com/andrewrk/libsoundio.git

cd libsoundio

cat soundio/endian.h soundio/soundio.h | sed 's!\(#include "endian.h"\)!// (amalg) \1!' > $AMALG_H

echo "#include \"$AMALG_H\"" > $AMALG_C
echo '#define SOUNDIO_VERSION_STRING ""' >> $AMALG_C
echo '#define SOUNDIO_VERSION_MAJOR 0' >> $AMALG_C
echo '#define SOUNDIO_VERSION_MINOR 0' >> $AMALG_C
echo '#define SOUNDIO_VERSION_PATCH 0' >> $AMALG_C

sed 's!\(#include ".*.h"\)!// (amalg) \1!' >> $AMALG_C << EOF
$(cat src/soundio_internal.h)
$(cat src/os.h)
$(cat src/atomics.h)
$(cat src/list.h)
$(cat src/ring_buffer.h)
$(cat src/util.h)
$(cat src/dummy.h)

#if SOUNDIO_HAVE_JACK
$(cat src/jack.h)
#endif
#if SOUNDIO_HAVE_ALSA
$(cat src/alsa.h)
#endif
#if SOUNDIO_HAVE_PULSEAUDIO
$(cat src/pulseaudio.h)
#endif
#if SOUNDIO_HAVE_COREAUDIO
$(cat src/coreaudio.h)
#endif
#if SOUNDIO_HAVE_WASAPI
$(cat src/wasapi.h)
#endif

$(cat src/soundio_private.h)

$(cat src/os.c)
$(cat src/ring_buffer.c)
$(cat src/util.c)
$(cat src/dummy.c)
$(cat src/soundio.c)
$(cat src/channel_layout.c)

#if SOUNDIO_HAVE_JACK
$(sed \
	-e 's!refresh_devices(!jack_refresh_devices(!' \
	-e 's!my_flush_events(!jack_my_flush_events(!' \
	src/jack.c)
#endif
#if SOUNDIO_HAVE_ALSA
$(sed \
	-e 's!refresh_devices(!alsa_refresh_devices(!' \
	-e 's!my_flush_events(!alsa_my_flush_events(!' \
	src/alsa.c)
#endif
#if SOUNDIO_HAVE_PULSEAUDIO
$(sed \
	-e 's!set_all_device_channel_layouts(!pa_set_all_device_channel_layouts(!' \
	-e 's!set_all_device_formats(!pa_set_all_device_formats(!' \
	-e 's!refresh_devices(!pa_refresh_devices(!' \
	-e 's!my_flush_events(!pa_my_flush_events(!' \
	src/pulseaudio.c)
#endif
#if SOUNDIO_HAVE_COREAUDIO
$(cat src/coreaudio.c)
#endif
#if SOUNDIO_HAVE_WASAPI
$(cat src/wasapi.c)
#endif
EOF

mv $AMALG_C $AMALG_H ..
cd ..

rm -rf libsoundio

echo "Done."

