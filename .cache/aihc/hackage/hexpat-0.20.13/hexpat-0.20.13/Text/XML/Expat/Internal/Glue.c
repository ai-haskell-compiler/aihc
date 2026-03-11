#include <expat.h>
#include <unistd.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    XML_Parser parser;
    XML_Char* (*decoder)(const XML_Char*);
    int locations;
} MyParser;

typedef struct {
    size_t offset;
    size_t capacity;
    uint8_t* block;
    MyParser* mp;
} Block;

/*!
 * Allocate the specified amount of room in the buffer.
 */
static void* alloc(Block* blk, size_t room)
{
    size_t minCapacity = blk->offset + room;
    size_t start = blk->offset;
    {
        int changed = 0;
        size_t capacity = blk->capacity;
        while (capacity < minCapacity) {
            capacity = capacity * 2;
            changed = 1;
        }
        if (changed) {
            blk->capacity = capacity;
            blk->block = realloc(blk->block, capacity);
        }
    }
    blk->offset += room;
    return blk->block + start;
}

static void pushType(Block* blk, uint32_t type)
{
    *(uint32_t*)alloc(blk, 4) = type;
    if (blk->mp->locations) {
        int64_t* loc = alloc(blk, 32);
        loc[0] = (int64_t)XML_GetCurrentLineNumber(blk->mp->parser);
        loc[1] = (int64_t)XML_GetCurrentColumnNumber(blk->mp->parser);
        loc[2] = (int64_t)XML_GetCurrentByteIndex(blk->mp->parser);
        loc[3] = (int64_t)XML_GetCurrentByteCount(blk->mp->parser);
    }
}

#define ROUND_UP_32(x) (((x) + 3) & ~3)

static void startElement(
    void *userData,
    const XML_Char *name,
    const XML_Char **atts)
{
    Block* blk = userData;
    size_t nameLen = strlen(name) + 1;
    size_t nAtts, i;
    for (nAtts = 0; atts[nAtts] != NULL; nAtts += 2) ;
    pushType(blk, 1);
    *(uint32_t*)alloc(blk, 4) = nAtts;
    memcpy(alloc(blk, nameLen), name, nameLen);
    for (i = 0; i < nAtts; i++) {
        size_t attLen = strlen(atts[i]) + 1;
        memcpy(alloc(blk, attLen), atts[i], attLen);
    }
    blk->offset = ROUND_UP_32(blk->offset);
}

static void endElement(void *userData, const XML_Char *name)
{
    Block* blk = userData;
    size_t nameLen = strlen(name) + 1;
    pushType(blk, 2);
    memcpy(alloc(blk, nameLen), name, nameLen);
    blk->offset = ROUND_UP_32(blk->offset);
}

static void characterData(
    void *userData,
    const XML_Char *s,
    int len)
{
    Block* blk = userData;
    pushType(blk, 3);
    *(uint32_t*)alloc(blk, 4) = len;
    memcpy(alloc(blk, (size_t)len), s, (size_t)len);
    blk->offset = ROUND_UP_32(blk->offset);
}

static void xmlDeclHandler(void           *userData,
                            const XML_Char *version,
                            const XML_Char *encoding,
                            int             standalone)
{
    Block* blk = userData;
    int verLen = strlen(version) + 1;
    pushType(blk, 4);
    memcpy(alloc(blk, verLen), version, verLen);
    if (encoding != NULL) {
        int encLen = strlen(encoding)+1;
        uint8_t* pEnc = alloc(blk, encLen+1);
        pEnc[0] = 1;
        memcpy(pEnc+1, encoding, encLen);
    }
    else
        *(uint8_t*)alloc(blk, 1) = 0;
    *(int8_t*)alloc(blk, 1) = (int8_t)standalone;
    blk->offset = ROUND_UP_32(blk->offset);
}

static void startCData(void* userData)
{
    Block* blk = userData;
    pushType(blk, 5);
}

static void endCData(void* userData)
{
    Block* blk = userData;
    pushType(blk, 6);
}

static void processingInstruction(void *userData, const XML_Char *target, const XML_Char *data)
{
    Block* blk = userData;
    int targetLen = strlen(target) + 1;
    int dataLen = strlen(data) + 1;
    pushType(blk, 7);
    memcpy(alloc(blk, targetLen), target, targetLen);
    memcpy(alloc(blk, dataLen), data, dataLen);
    blk->offset = ROUND_UP_32(blk->offset);
}

static void comment(void* userData, const XML_Char *text)
{
    Block* blk = userData;
    int textLen = strlen(text) + 1;
    pushType(blk, 8);
    memcpy(alloc(blk, textLen), text, textLen);
    blk->offset = ROUND_UP_32(blk->offset);
}

MyParser* hexpatNewParser(const XML_Char* encoding, int locations)
{
    MyParser* mp = malloc(sizeof(MyParser));
    XML_Parser p = XML_ParserCreate(encoding);
    XML_SetStartElementHandler(p, startElement);
    XML_SetEndElementHandler(p, endElement);
    XML_SetCharacterDataHandler(p, characterData);
    XML_SetXmlDeclHandler(p, xmlDeclHandler);
    XML_SetCdataSectionHandler(p, startCData, endCData);
    XML_SetProcessingInstructionHandler(p, processingInstruction);
    XML_SetCommentHandler(p, comment);
    mp->parser = p;
    mp->locations = locations;
    return mp;
}

void hexpatFreeParser(MyParser* mp)
{
    XML_ParserFree(mp->parser);
    free(mp);
}

static int externalEntityRef(XML_Parser parser,
                                const XML_Char *context,
                                const XML_Char *base,
                                const XML_Char *systemId,
                                const XML_Char *publicId)
{
    if (systemId == NULL && publicId == NULL) {
        XML_Parser eep = XML_ExternalEntityParserCreate(parser, context, NULL);
        enum XML_Status ret = XML_Parse(eep, "", 0, XML_TRUE);
        if (ret == XML_STATUS_OK) {
            XML_ParserFree(eep);
        }
        else {
            XML_ParserFree(eep);
            XML_StopParser(parser, 0);
        }
    }
    else
        XML_StopParser(parser, 0);
}

static void skippedEntity(void *userData,
                           const XML_Char *entityName,
                           int is_parameter_entity)
{
    Block* blk = userData;
    if (is_parameter_entity)
        XML_StopParser(blk->mp->parser, 0);
    else {
        XML_Char* out = blk->mp->decoder(entityName);
        if (out != NULL) {
            characterData(blk, out, strlen(out));
            free(out);
        }
        else
            XML_StopParser(blk->mp->parser, 0);
    }
}

enum XML_Status hexpatParse(
    MyParser* mp,
    const char* s,
    int len,
    int isFinal,
    uint8_t** buffer,
    int* length)
{
    enum XML_Status ret;
    Block blk;
    blk.offset = 0;
    blk.capacity = 256;
    blk.block = malloc(blk.capacity);
    blk.mp = mp;
    XML_SetUserData(mp->parser, &blk);
    ret = XML_Parse(mp->parser, s, len, isFinal);
    *(uint32_t*)alloc(&blk, 4) = 0;
    *buffer = blk.block;
    *length = (int)blk.offset;
    return ret;
}

void hexpatSetEntityHandler(
    MyParser* mp,
    XML_Char* (*decoder)(const XML_Char*))
{
    mp->decoder = decoder;
    XML_UseForeignDTD(mp->parser, XML_TRUE);
    XML_SetExternalEntityRefHandler(mp->parser, externalEntityRef);
    XML_SetSkippedEntityHandler(mp->parser, skippedEntity);
}

XML_Parser hexpatGetParser(MyParser* mp)
{
    return mp->parser;
}
