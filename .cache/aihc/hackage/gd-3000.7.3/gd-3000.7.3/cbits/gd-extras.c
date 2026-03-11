#include <gd.h>
#include "gd-extras.h"

BGD_DECLARE(void) gdImagePtrDestroyIfNotNull (gdImagePtr *imp)
{
  gdImagePtr im = *imp;
  if (im) {
    gdImageDestroy(im);
  }
}

BGD_DECLARE(void) gdImageCopyRotated90 (gdImagePtr dst,
					gdImagePtr src,
					int dstX, int dstY,
					int srcX, int srcY,
					int srcW, int srcH, int quadrants)
{
  int dx, dy;  
  /* make sure it's 0-3 */
  int q = (4 + quadrants % 4) % 4;
  int even = q % 2 == 0;
  int cos_q = even ? 1 - q :     0;
  int sin_q = even ?     0 : 2 - q;

  /* hack for figuring out destination width */
  int dstW = even ? srcW : srcH;
  int dstH = even ? srcH : srcW;

  int cmap[gdMaxColors];
  int i;
  for (i = 0; (i < gdMaxColors); i++)
    {
      cmap[i] = (-1);
    }

  for (dy = 0; (dy < dstH); dy++)
    {
      for (dx = 0; (dx < dstW); dx++)
	{
	  int sx = srcX + cos_q * dx - sin_q * dy;
	  int sy = srcY + sin_q * dx + cos_q * dy;
	  if ((sx >= 0) && (sx < srcW) &&
	      (sy >= 0) && (sy < srcH))
	    {
	      int c = gdImageGetPixel (src, sx, sy);

	      if (!src->trueColor)
		{
		  /* Use a table to avoid an expensive
		     lookup on every single pixel */
		  if (cmap[c] == -1)
		    {
		      cmap[c] = gdImageColorResolveAlpha (dst,
							  gdImageRed (src, c),
							  gdImageGreen (src,
									c),
							  gdImageBlue (src,
								       c),
							  gdImageAlpha (src,
									c));
		    }
		  gdImageSetPixel (dst, dstX + dx, dstY + dy, cmap[c]);
		}
	      else
		{
		  gdImageSetPixel (dst,
				   dstX + dx, dstY + dy,
				   gdImageColorResolveAlpha (dst,
							     gdImageRed (src,
									 c),
							     gdImageGreen
							     (src, c),
							     gdImageBlue (src,
									  c),
							     gdImageAlpha
							     (src, c)));
		}

	    }
	}
    }

}
