#ifdef __cplusplus
extern "C" {
#endif

#ifndef GD_EXTRAS_H
#define G_DEXTRAS_H 1

#include <gd.h>

BGD_DECLARE(void) gdImagePtrDestroyIfNotNull (gdImagePtr *im);

BGD_DECLARE(void) gdImageCopyRotated90 (gdImagePtr dst,
					gdImagePtr src,
					int dstX, int dstY,
					int srcX, int srcY,
					int srcW, int srcH, int quadrants);



#endif

#ifdef __cplusplus
}
#endif
