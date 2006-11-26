#include <gd.h>

int
main(int argc, char **argv)
{
  gdImagePtr im, im2, im3;
  int black, white, trans;
  FILE *out;
  /* Create the image */
  im = gdImageCreate(100, 100);
  /* Allocate background */
  white = gdImageColorAllocate(im, 255, 255, 255);
  /* Allocate drawing color */
  black = gdImageColorAllocate(im, 0, 0, 0);
  /* Allocate transparent color for animation compression */
  trans = gdImageColorAllocate(im, 1, 1, 1);
  /* Draw rectangle */
  gdImageRectangle(im, 0, 0, 10, 10, black);
  /* Open output file in binary mode */
  out = fopen("anim.gif", "wb");
  /* Write GIF header.  Use global color map.  Loop a few times */
  gdImageGifAnimBegin(im, out, 1, 3);
  /* Write the first frame.  No local color map.  Delay = 1s */
  gdImageGifAnimAdd(im, out, 0, 0, 0, 100, 1, NULL);
  /* construct the second frame */
  im2 = gdImageCreate(100, 100);
  /* Allocate background to make it white */
  (void)gdImageColorAllocate(im2, 255, 255, 255);
  /* Make sure the palette is identical */
  gdImagePaletteCopy (im2, im);
  /* Draw something */
  gdImageRectangle(im2, 0, 0, 15, 15, black);
  /* Allow animation compression with transparent pixels */
  gdImageColorTransparent (im2, trans);
  /* Add the second frame */
  gdImageGifAnimAdd(im2, out, 0, 0, 0, 100, 1, im);
  /* construct the second frame */
  im3 = gdImageCreate(100, 100);
  /* Allocate background to make it white */
  (void)gdImageColorAllocate(im3, 255, 255, 255);
  /* Make sure the palette is identical */
  gdImagePaletteCopy (im3, im);
  /* Draw something */
  gdImageRectangle(im3, 0, 0, 15, 20, black);
  /* Allow animation compression with transparent pixels */
  gdImageColorTransparent (im3, trans);
  /* Add the third frame, compressing against the second one */
  gdImageGifAnimAdd(im3, out, 0, 0, 0, 100, 1, im2);
  /* Write the end marker */
  /* gdImageGifAnimEnd(out); is the same as the following: */
  putc (';', out);
  /* Close file */
  fclose(out);
  /* Destroy images */
  gdImageDestroy(im);
  gdImageDestroy(im2);
  gdImageDestroy(im3);
  return 0;
}
