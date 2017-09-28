#include <math.h>
#include <stdio.h>

#include "../portmidi.h"

int main() {
    int r = Pm_Initialize();
    if (r != 0) {
        goto error;
    }
    printf("%d\n", Pm_CountDevices());
    Pt_Start(1, NULL, NULL);
    Pt_Stop();
error:
    return 0;
}
