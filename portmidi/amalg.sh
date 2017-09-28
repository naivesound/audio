#!/bin/sh

AMALG_H=portmidi.h
AMALG_C=pm_amalg.c

echo "Downloading PortMidi sources..."
svn checkout http://svn.code.sf.net/p/portmedia/code/portmidi/trunk/ portmidi

cd portmidi

echo "Amalgamating to $AMALG_C and $AMALG_H..."

sed 's!\(#include ".*.h"\)!// (amalg) \1!' >> $AMALG_H << EOF
#include <stdlib.h>
#include <string.h>
$(cat pm_common/portmidi.h)
$(cat pm_common/pmutil.h)
$(cat pm_common/pminternal.h)
$(cat porttime/porttime.h)

#if __linux__
#define PMALSA
$(cat pm_linux/pmlinux.h)
$(cat pm_linux/pmlinuxalsa.h)
#elif _WIN32
#include <windows.h>
$(cat pm_win/pmwinmm.h)
#elif __APPLE__
$(cat pm_mac/pmmac.h)
$(cat pm_mac/pmmacosxcm.h)
$(cat pm_mac/readbinaryplist.h)
#else
#error "Unknown platform"
#endif
EOF

echo "#include \"$AMALG_H\"" > $AMALG_C
sed 's!\(#include ".*.h"\)!// (amalg) \1!' >> $AMALG_C << EOF
$(cat pm_common/pmutil.c)
$(cat pm_common/portmidi.c)
$(cat porttime/porttime.c)
#if __linux__
#include <sys/timeb.h>
#include <sys/time.h>
#include <sys/resource.h>
$(cat porttime/ptlinux.c)
$(cat pm_linux/pmlinux.c)
$(cat pm_linux/pmlinuxalsa.c)
$(cat pm_linux/finddefault.c)
#elif _WIN32
$(cat porttime/ptwinmm.c)
$(cat pm_win/pmwin.c)
$(cat pm_win/pmwinmm.c)
#elif __APPLE__
$(cat porttime/ptmacosx_cf.c)
$(cat pm_mac/pmmac.c)
$(cat pm_mac/pmmacosxcm.c)
$(cat pm_mac/readbinaryplist.c)
$(cat pm_mac/finddefault.c)
#endif
EOF

mv $AMALG_C $AMALG_H ..
cd ..

rm -rf portmidi

echo "Done."
