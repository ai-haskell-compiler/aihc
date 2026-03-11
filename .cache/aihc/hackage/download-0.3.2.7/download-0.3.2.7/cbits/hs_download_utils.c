#include "hs_download_utils.h"

char * hs_download_last_error () {
    return downloadLastErrString;
}

int hs_download_last_error_code () {
    return downloadLastErrCode;
}
