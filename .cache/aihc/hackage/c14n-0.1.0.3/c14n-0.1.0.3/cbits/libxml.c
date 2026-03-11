#include <libxml/globals.h>

void freeXml(void* p) {
    xmlFree(p);
}