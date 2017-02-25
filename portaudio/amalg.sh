#!/bin/sh

AMALG_H=portaudio.h
AMALG_C=pa_amalg.c

echo "Downloading PortAudio sources..."
git clone https://git.assembla.com/portaudio.git

cd portaudio

echo "Amalgamating to $AMALG_C and $AMALG_H..."

sed 's!\(#include ".*.h"\)!// (amalg) \1!' >> $AMALG_H << EOF
$(cat include/portaudio.h)

#if PA_USE_ASIO
$(cat include/pa_asio.h)
#endif

#if PA_USE_JACK
$(cat include/pa_jack.h)
#endif

#if PA_USE_ALSA
$(cat include/pa_linux_alsa.h)
#endif

#if PA_USE_COREAUDIO
$(cat include/pa_mac_core.h)
#endif

#if PA_USE_DS
$(cat include/pa_win_ds.h)
#endif

#if PA_USE_WASAPI
$(cat include/pa_win_wasapi.h)
#endif

#if PA_USE_WDMKS
$(cat include/pa_win_wdmks.h)
#endif

#if PA_USE_WMME
$(cat include/pa_win_wmme.h)
#endif

#if PA_USE_DS || PA_USE_WASAPI || PA_USE_WMME
$(cat include/pa_win_waveformat.h)
#endif
EOF

echo "#include \"$AMALG_H\"" > $AMALG_C
sed 's!\(#include ".*.h"\)!// (amalg) \1!' >> $AMALG_C << EOF

$(cat src/common/pa_types.h)

$(cat src/common/pa_allocation.h)
$(cat src/common/pa_converters.h)
$(cat src/common/pa_cpuload.h)
$(cat src/common/pa_debugprint.h)
$(cat src/common/pa_dither.h)
$(cat src/common/pa_endianness.h)
$(cat src/common/pa_gitrevision.h)
$(cat src/common/pa_hostapi.h)
$(cat src/common/pa_memorybarrier.h)
$(cat src/common/pa_process.h)
$(cat src/common/pa_ringbuffer.h)
$(cat src/common/pa_stream.h)
$(cat src/common/pa_trace.h)
$(cat src/common/pa_util.h)

$(cat src/common/*.c)

#ifdef WIN32
$(cat src/os/win/*.h)
$(cat src/os/win/*.c)
#else
$(cat src/os/unix/*.h)
$(cat src/os/unix/*.c)
#endif

#if PA_USE_ALSA
$(cat src/hostapi/alsa/*.h)
$(cat src/hostapi/alsa/*.c)
#endif

#if 0
$(cat src/hostapi/asihpi/*.h)
$(cat src/hostapi/asihpi/*.c)
#endif

#if PA_USE_ASIO
// This uses C++, not C!
$(cat src/hostapi/asio/*.h)
$(cat src/hostapi/asio/*.cpp)
#endif

#if PA_USE_COREAUDIO
$(cat src/hostapi/coreaudio/*.h)
$(cat src/hostapi/coreaudio/*.c)
#endif

#if PA_USE_DS
$(cat src/hostapi/dsound/*.h)
$(cat src/hostapi/dsound/*.c)
#endif

#if PA_USE_JACK
$(cat src/hostapi/jack/*.h)
$(cat src/hostapi/jack/*.c)
#endif

#if PA_USE_OSS
$(cat src/hostapi/oss/*.h)
$(cat src/hostapi/oss/*.c)
#endif

#if PA_USE_WASAPI
$(cat src/hostapi/wasapi/mingw-include/*.h)
$(cat src/hostapi/wasapi/*.c)
#endif

#if PA_USE_WDMKS
$(cat src/hostapi/wdmks/*.h)
$(cat src/hostapi/wdmks/*.c) 
#endif

#if PA_USE_WMME
$(cat src/hostapi/wmme/*.h)
$(cat src/hostapi/wmme/*.c)
#endif
EOF

mv $AMALG_C $AMALG_H ..
cd ..

rm -rf portaudio

echo "Done."