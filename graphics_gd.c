/*
 *  graphics_gd.c
 *
 *   Copyright (c) 2006-2008 Takeshi Abe. All rights reserved.
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 *    3. Neither the name of the authors nor the names of its contributors
 *       may be used to endorse or promote products derived from this
 *       software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id$
 */

#include "graphics_gd.h"
#include "graphics_gd_io.h"

ScmClass *GraphicsGdImageClass;
ScmClass *GraphicsGdFontClass;

static void
graphicsGdImageCleanUp(ScmObj obj)
{
  if (!graphicsGdImageDestroyedP(obj)) {
	gdImage *p = GRAPHICS_GD_IMAGE_UNBOX(obj);
	gdImageDestroy(p);
  }
}

static void
graphicsGdFontCleanUp(ScmObj obj)
{
  /* to be continued */
}

static ScmObj sym_destroyed;

int
graphicsGdImageDestroyedP(ScmObj obj)
{
  SCM_ASSERT(SCM_FOREIGN_POINTER_P(obj));
  return !SCM_FALSEP(Scm_ForeignPointerAttrGet(SCM_FOREIGN_POINTER(obj),
											   sym_destroyed, SCM_FALSE));
}

void
graphicsGdImageMarkDestroyed(ScmObj obj)
{
  SCM_ASSERT(SCM_FOREIGN_POINTER_P(obj));
  Scm_ForeignPointerAttrSet(SCM_FOREIGN_POINTER(obj),
							sym_destroyed, SCM_TRUE);
}

void
graphicsGdRaiseCondition(const char *msg, const char *arg)
{
  Scm_RaiseCondition(SCM_SYMBOL_VALUE("graphics.gd", "<gd-error>"),
					 SCM_RAISE_CONDITION_MESSAGE,
					 msg, arg);
}

#define F_OPEN_AND_LOCK(f, fl, fd, path, flag, type) do {			\
	int r;															\
	if ( (f = fopen(path, flag)) == NULL) {							\
	  graphicsGdRaiseCondition("could not open file: %s", path);	\
	  return -1;													\
	}																\
	fl.l_type = type;												\
	fl.l_whence = SEEK_SET;											\
	fl.l_start = fl.l_len = 0;										\
	fd = fileno(f);													\
	SCM_SYSCALL(r, fcntl(fd, F_SETLKW, &fl));						\
	if (r == -1) {													\
	  graphicsGdRaiseCondition("could not lock file: %s", path);	\
	  return -2;													\
	}																\
  } while (0)

#define F_CLOSE_AND_RETURN(f, path) do {							\
	if (fclose(f) == 0) {											\
	  return 0;														\
	} else {														\
	  graphicsGdRaiseCondition("could not close file: %s", path);	\
	  return -3;													\
	}																\
  } while (0)

#define CALL_WITH_F4(path, flag, type, exp) do {	\
	FILE *f;										\
	struct flock fl;								\
	int fd;											\
	F_OPEN_AND_LOCK(f, fl, fd, path, flag, type);	\
	(exp);											\
	F_CLOSE_AND_RETURN(f, path);					\
  } while (0)

#define CALL_WITH_F5(path, flag, type, exp, val) do {	\
	FILE *f;											\
	struct flock fl;									\
	int fd;												\
	F_OPEN_AND_LOCK(f, fl, fd, path, flag, type);		\
	(val) = (exp);										\
	F_CLOSE_AND_RETURN(f, path);						\
  } while (0)

#define PROPER_GRAPHICS_GD_IMAGE_CREATE_FROM(fmt) int					\
  graphicsGdImageCreateFrom ## fmt(gdImage **dst, const char *path)		\
  {																		\
	CALL_WITH_F5(path, "rb", F_RDLCK, gdImageCreateFrom ## fmt(f), *dst); \
  }

#define IMPROPER_GRAPHICS_GD_IMAGE_CREATE_FROM(fmt) int				\
  graphicsGdImageCreateFrom ## fmt(gdImage **dst, const char *path)	\
  {																	\
	graphicsGdRaiseCondition("unsupported format: %s", #fmt);		\
	return -1;														\
  }

#ifdef GD_PNG
PROPER_GRAPHICS_GD_IMAGE_CREATE_FROM(Png)
#else
IMPROPER_GRAPHICS_GD_IMAGE_CREATE_FROM(Png)
#endif /* GD_PNG */

#ifdef GD_GIF
PROPER_GRAPHICS_GD_IMAGE_CREATE_FROM(Gif)
#else
IMPROPER_GRAPHICS_GD_IMAGE_CREATE_FROM(Gif)
#endif /* GD_GIF */

#ifdef GD_JPEG
PROPER_GRAPHICS_GD_IMAGE_CREATE_FROM(Jpeg)
#else
IMPROPER_GRAPHICS_GD_IMAGE_CREATE_FROM(Jpeg)
#endif /* GD_JPEG */

PROPER_GRAPHICS_GD_IMAGE_CREATE_FROM(WBMP)
PROPER_GRAPHICS_GD_IMAGE_CREATE_FROM(Gd)
PROPER_GRAPHICS_GD_IMAGE_CREATE_FROM(Gd2)

int
graphicsGdImageCreateFromGd2Part(gdImage **dst, const char *path, int srcx, int srcy, int w, int h)
{
  CALL_WITH_F5(path, "rb", F_RDLCK, gdImageCreateFromGd2Part(f, srcx, srcy, w, h), *dst);
}

PROPER_GRAPHICS_GD_IMAGE_CREATE_FROM(Xbm)

int
graphicsGdImageCreateFromXpm(gdImage **dst, const char *path)
{
#ifdef GD_XPM
  size_t len = strlen(path);
  char *temp = calloc(len + 1, sizeof(char)); /* with terminating `\0' */
  if (temp == NULL) {
	graphicsGdRaiseCondition("calloc failed: %s", "graphicsGdImageCreateFromXpm");
	return -1;
  }
  strncpy(temp, path, len + 1);
  *dst = gdImageCreateFromXpm(temp);
  free(temp);
  return 0;
#else
  graphicsGdRaiseCondition("unsupported format: %s", "Xpm");
  return -2;
#endif
}

#undef PROPER_GRAPHICS_GD_IMAGE_CREATE_FROM
#undef IMPROPER_GRAPHICS_GD_IMAGE_CREATE_FROM

/* for Gauche 0.8.7 */
#ifndef SCM_TYPE_ERROR
#define SCM_TYPE_ERROR(arg, expected) Scm_Error("%s expected for %s, but got %S", expected, #arg, arg)
#endif

#define CALL_WITH_ICTX(port, exp, alt, val, func) do {				\
	gdIOCtx *ctx;													\
	if (!SCM_IPORTP(port)) {										\
	  SCM_TYPE_ERROR(SCM_OBJ(port), "input port");					\
	  return -1;													\
	}																\
	if ( (ctx = graphicsGdGetIOCtxFromPort(port)) == NULL) {		\
	  graphicsGdRaiseCondition("could not get gdIOCtx: %s", #func);	\
	  (val) = (alt);												\
	  return -2;													\
	}																\
	(val) = (exp);													\
	return 0;														\
  } while (0)

#define PROPER_GRAPHICS_GD_IMAGE_CREATE_PORT(fmt) int					\
  graphicsGdImageCreate ## fmt ## Port(gdImage **dst, ScmPort *port)	\
  {																		\
	CALL_WITH_ICTX(port, gdImageCreateFrom ## fmt ## Ctx(ctx), (gdImage *)NULL, *dst, graphicsGdImageCreate ## fmt ## Port); \
  }

#define IMPROPER_GRAPHICS_GD_IMAGE_CREATE_PORT(fmt) int					\
  graphicsGdImageCreate ## fmt ## Port(gdImage **dst, ScmPort *port)	\
  {																		\
	graphicsGdRaiseCondition("unsupported format: %s", #fmt);			\
	*dst = (gdImage *)NULL;												\
	return -1;															\
  }

#ifdef GD_PNG
PROPER_GRAPHICS_GD_IMAGE_CREATE_PORT(Png)
#else
IMPROPER_GRAPHICS_GD_IMAGE_CREATE_PORT(Png)
#endif /* GD_PNG */

#ifdef GD_GIF
PROPER_GRAPHICS_GD_IMAGE_CREATE_PORT(Gif)
#else
IMPROPER_GRAPHICS_GD_IMAGE_CREATE_PORT(Gif)
#endif /* GD_GIF */

#ifdef GD_JPEG
PROPER_GRAPHICS_GD_IMAGE_CREATE_PORT(Jpeg)
#else
IMPROPER_GRAPHICS_GD_IMAGE_CREATE_PORT(Jpeg)
#endif /* GD_JPEG */

PROPER_GRAPHICS_GD_IMAGE_CREATE_PORT(WBMP)
PROPER_GRAPHICS_GD_IMAGE_CREATE_PORT(Gd)
PROPER_GRAPHICS_GD_IMAGE_CREATE_PORT(Gd2)

int
graphicsGdImageCreateGd2PartPort(gdImage **dst, ScmPort *port, int srcx, int srcy, int w, int h)
{
  CALL_WITH_ICTX(port, gdImageCreateFromGd2PartCtx(ctx, srcx, srcy, w, h), (gdImage *)NULL, *dst, graphicsGdImageCreateGd2PartPort);
}

#undef PROPER_GRAPHICS_GD_IMAGE_CREATE_PORT
#undef IMPROPER_GRAPHICS_GD_IMAGE_CREATE_PORT

#define CALL_WITH_OCTX(port, exp, func) do {						\
	gdIOCtx *ctx;													\
	if (!SCM_OPORTP(port)) {										\
	  SCM_TYPE_ERROR(SCM_OBJ(port), "output port");					\
	  return;														\
	}																\
	if ( (ctx = graphicsGdGetIOCtxFromPort(port)) == NULL) {		\
	  graphicsGdRaiseCondition("could not get gdIOCtx: %s", #func);	\
	  return;														\
	}																\
	(exp);															\
	(ctx->gd_free)(ctx);											\
  } while (0)

#define PROPER_GRAPHICS_GD_IMAGE_WRITE_AS(fmt, args) void				\
  graphicsGdImageWriteAs ## fmt(gdImage *im, ScmPort *port)				\
  {																		\
	CALL_WITH_OCTX(port, gdImage ## fmt ## Ctx args, graphicsGdImageWriteAs ## fmt); \
  }

#define IMPROPER_GRAPHICS_GD_IMAGE_WRITE_AS(fmt) void			\
  graphicsGdImageWriteAs ## fmt(gdImage *im, ScmPort *port)		\
  {																\
	graphicsGdRaiseCondition("unsupported format: %s", #fmt);	\
  }

void
graphicsGdImageWriteAsJpeg(gdImage *im, ScmPort *port, int quality)
{
#ifdef GD_JPEG
  CALL_WITH_OCTX(port, gdImageJpegCtx(im, ctx, quality), graphicsGdImageWriteAsJpeg);
#else
  graphicsGdRaiseCondition("unsupported format: %s", "Jpeg");
#endif /* GD_JPEG */
}

void
graphicsGdImageWriteAsWBMP(gdImage *im, int fg, ScmPort *port)
{
  CALL_WITH_OCTX(port, gdImageWBMPCtx(im, fg, ctx), graphicsGdImageWriteAsWBMP);
}

#ifdef GD_PNG
PROPER_GRAPHICS_GD_IMAGE_WRITE_AS(Png, (im, ctx))
#else
IMPROPER_GRAPHICS_GD_IMAGE_WRITE_AS(Png)
#endif /* GD_PNG */

#ifdef GD_GIF
PROPER_GRAPHICS_GD_IMAGE_WRITE_AS(Gif, (im, ctx))
#else
IMPROPER_GRAPHICS_GD_IMAGE_WRITE_AS(Gif)
#endif /* GD_GIF */

#undef PROPER_GRAPHICS_GD_IMAGE_WRITE_AS
#undef IMPROPER_GRAPHICS_GD_IMAGE_WRITE_AS

#define PROPER_GRAPHICS_GD_IMAGE_SAVE_AS(fmt, args) int				\
  graphicsGdImageSaveAs ## fmt(gdImage *im, const char *path)		\
  {																	\
	CALL_WITH_F4(path, "wb", F_WRLCK, gdImage ## fmt args);			\
  }

#define IMPROPER_GRAPHICS_GD_IMAGE_SAVE_AS(fmt) int				\
  graphicsGdImageSaveAs ## fmt(gdImage *im, const char *path)	\
  {																\
	graphicsGdRaiseCondition("unsupported format: %s", #fmt);	\
	return -1;													\
  }

int
graphicsGdImageSaveAsJpeg(gdImage *im, const char *path, int quality)
{
#ifdef GD_JPEG
  CALL_WITH_F4(path, "wb", F_WRLCK, gdImageJpeg(im, f, quality));
#else
  graphicsGdRaiseCondition("unsupported format: %s", "Jpeg");
  return -1;
#endif /* GD_JPEG */
}

int
graphicsGdImageSaveAsWBMP(gdImage* im, int fg, const char *path)
{
  CALL_WITH_F4(path, "wb", F_WRLCK, gdImageWBMP(im, fg, f));
}

#ifdef GD_PNG
PROPER_GRAPHICS_GD_IMAGE_SAVE_AS(Png, (im, f))
#else
IMPROPER_GRAPHICS_GD_IMAGE_SAVE_AS(Png)
#endif /* GD_PNG */

#ifdef GD_GIF
PROPER_GRAPHICS_GD_IMAGE_SAVE_AS(Gif, (im, f))
#else
IMPROPER_GRAPHICS_GD_IMAGE_SAVE_AS(Gif)
#endif /* GD_GIF */

PROPER_GRAPHICS_GD_IMAGE_SAVE_AS(Gd, (im, f))

int
graphicsGdImageSaveAsGd2(gdImage* im, const char *path, int chunkSize, int fmt)
{
  CALL_WITH_F4(path, "wb", F_WRLCK, gdImageGd2(im, f, chunkSize, fmt));
}

#undef PROPER_GRAPHICS_GD_IMAGE_SAVE_AS
#undef IMPROPER_GRAPHICS_GD_IMAGE_SAVE_AS

void
graphicsGdImageGifAnimBeginPort(gdImage *im, ScmPort *oport, int GlobalCM, int loops)
{
  CALL_WITH_OCTX(oport, gdImageGifAnimBeginCtx(im, ctx, GlobalCM, loops), graphicsGdImageGifAnimBeginPort);
}

void
graphicsGdImageGifAnimAddPort(gdImage *im, ScmPort *oport, int localCM, int LeftOfs, int TopOfs, int Delay, int Disposal, gdImage *previm)
{
  CALL_WITH_OCTX(oport, gdImageGifAnimAddCtx(im, ctx, localCM, LeftOfs, TopOfs, Delay, Disposal, previm), graphicsGdImageGifAnimAddPort);
}

void
graphicsGdImageGifAnimEndPort(ScmPort *oport)
{
  SCM_PUTC(';', oport);
}

#define CHECK_LIST_AND_LENGTH(list, length) do {						\
	if (!SCM_LISTP(list) || !SCM_PROPER_LIST_P(list)) {					\
	  SCM_TYPE_ERROR(list, "proper list");								\
	  return;															\
	}																	\
	if (length < 0) {													\
	  Scm_Error("non negative integer required, but got %d", length);	\
	  return;															\
	}																	\
  } while (0)

#define DEFINE_GRAPHICS_GD_IMAGE_POLYGON(name) void						\
  graphicsGdImage ## name(gdImage *im, ScmObj points, int pointsTotal, int color) \
  {																		\
	ScmObj head, pair;													\
	gdPoint *p, *q;														\
	int i = 0;															\
	CHECK_LIST_AND_LENGTH(points, pointsTotal);							\
	p = q = calloc(pointsTotal, sizeof(gdPoint));						\
	if (p == NULL) {													\
	  graphicsGdRaiseCondition("calloc failed: %s", "graphicsGdImage" #name); \
	  return;															\
	}																	\
	SCM_FOR_EACH (head, points) {										\
	  if (i++ == pointsTotal) break;									\
	  pair = SCM_CAR(head);												\
	  if (!SCM_PAIRP(pair)) {											\
		SCM_TYPE_ERROR(pair, "pair");									\
		free(p);														\
		return;															\
	  }																	\
	  q->x = SCM_INT_VALUE(SCM_CAR(pair));								\
	  q->y = SCM_INT_VALUE(SCM_CDR(pair));								\
	  q++;																\
	}																	\
	gdImage ## name(im, p, pointsTotal, color);							\
	free(p);															\
  }

DEFINE_GRAPHICS_GD_IMAGE_POLYGON(Polygon)
#ifdef WITH_GD_IMAGE_OPEN_POLYGON
DEFINE_GRAPHICS_GD_IMAGE_POLYGON(OpenPolygon)
#else
void
graphicsGdImageOpenPolygon(gdImage *im, ScmObj points, int pointsTotal, int color)
{
  graphicsGdRaiseCondition("gdImageOpenPolygon is unavailable (possibly because of version %s)", GDLIB_VERSION);
  return;
}
#endif /* WITH_GD_IMAGE_OPEN_POLYGON */
DEFINE_GRAPHICS_GD_IMAGE_POLYGON(FilledPolygon)

#undef DEFINE_GRAPHICS_GD_IMAGE_POLYGON

void
graphicsGdImageSetStyle(gdImage *im, ScmObj style, int styleLength)
{
  ScmObj head;
  int *p, *q, i = 0;
  CHECK_LIST_AND_LENGTH(style, styleLength);
  p = q = calloc(styleLength, sizeof(int));
  if (p == NULL) {
	graphicsGdRaiseCondition("calloc failed: %s", "graphicsGdImageSetStyle");
	return;
  }
  SCM_FOR_EACH (head, style) {
	if (i++ == styleLength) break;
	*q++ = SCM_INT_VALUE(SCM_CAR(head));
  }
  gdImageSetStyle(im, p, styleLength);
  free(p);
}

#define PROPER_GRAPHICS_GD_FONT_GET(size) int	\
  graphicsGdFontGet ## size(gdFont **dst)		\
  {												\
	*dst = gdFontGet ## size();					\
	return 0;									\
  }

#define IMPROPER_GRAPHICS_GD_FONT_GET(size) int					\
  graphicsGdFontGet ## size(gdFont **dst)						\
  {																\
	graphicsGdRaiseCondition("could not get gdFont%s", #size);	\
	*dst = (gdFont *)NULL;										\
	return -1;													\
  }

#ifdef HAVE_GDFONTG_H
PROPER_GRAPHICS_GD_FONT_GET(Giant)
#else
IMPROPER_GRAPHICS_GD_FONT_GET(Giant)
#endif /* HAVE_GDFONTG_H */

#ifdef HAVE_GDFONTL_H
PROPER_GRAPHICS_GD_FONT_GET(Large)
#else
IMPROPER_GRAPHICS_GD_FONT_GET(Large)
#endif /* HAVE_GDFONTL_H */

#ifdef HAVE_GDFONTMB_H
PROPER_GRAPHICS_GD_FONT_GET(MediumBold)
#else
IMPROPER_GRAPHICS_GD_FONT_GET(MediumBold)
#endif /* HAVE_GDFONTMB_H */

#ifdef HAVE_GDFONTS_H
PROPER_GRAPHICS_GD_FONT_GET(Small)
#else
IMPROPER_GRAPHICS_GD_FONT_GET(Small)
#endif /* HAVE_GDFONTS_H */

#ifdef HAVE_GDFONTT_H
PROPER_GRAPHICS_GD_FONT_GET(Tiny)
#else
IMPROPER_GRAPHICS_GD_FONT_GET(Tiny)
#endif /* HAVE_GDFONTT_H */

#undef PROPER_GRAPHICS_GD_FONT_GET
#undef IMPROPER_GRAPHICS_GD_FONT_GET

#define WITH_UCHARP_FROM_CONST_CHARP(dst, src, exp, func) do {			\
	size_t len = strlen(src);											\
	unsigned char *dst = calloc(len + 1, sizeof(unsigned char)); /* with the terminating `\0' */ \
	if (dst == NULL) {													\
	  graphicsGdRaiseCondition("calloc failed: %s", #func);				\
	  return;															\
	}																	\
	strncpy(dst, src, len + 1);											\
	(exp);																\
	free(dst);															\
  } while (0)

void
graphicsGdImageString(gdImagePtr im, gdFontPtr f, int x, int y, const char *s, int color)
{
  WITH_UCHARP_FROM_CONST_CHARP(dst, s, gdImageString(im, f, x, y, dst, color), graphicsGdImageString);
}

void
graphicsGdImageStringUp(gdImagePtr im, gdFontPtr f, int x, int y, const char *s, int color)
{
  WITH_UCHARP_FROM_CONST_CHARP(dst, s, gdImageStringUp(im, f, x, y, dst, color), graphicsGdImageStringUp);
}

int
graphicsGdImageStringFT(gdImage *im,
						ScmPair **brect0, ScmPair **brect1, ScmPair **brect2, ScmPair **brect3,
						int fg, ScmString *fontlist, double ptsize, double angle, int x, int y, ScmString *str)
{
  int brect[8];
  char *s = Scm_GetString(str);
  char *fl = Scm_GetString(fontlist);
  char *e = gdImageStringFT(im, brect, fg, fl, ptsize, angle, x, y, s);
  if (e == NULL) {
	*brect0 = SCM_NEW(ScmPair);
	SCM_SET_CAR(*brect0, Scm_MakeInteger(brect[0]));
	SCM_SET_CDR(*brect0, Scm_MakeInteger(brect[1]));
	*brect1 = SCM_NEW(ScmPair);
	SCM_SET_CAR(*brect1, Scm_MakeInteger(brect[2]));
	SCM_SET_CDR(*brect1, Scm_MakeInteger(brect[3]));
	*brect2 = SCM_NEW(ScmPair);
	SCM_SET_CAR(*brect2, Scm_MakeInteger(brect[4]));
	SCM_SET_CDR(*brect2, Scm_MakeInteger(brect[5]));
	*brect3 = SCM_NEW(ScmPair);
	SCM_SET_CAR(*brect3, Scm_MakeInteger(brect[6]));
	SCM_SET_CDR(*brect3, Scm_MakeInteger(brect[7]));
	return 0;
  } else {
	graphicsGdRaiseCondition("gdImageStringFT failed: %s", e);
	return -1;
  }
}

ScmObj
graphicsGdGetFeatures(void)
{
  ScmObj r = (ScmObj)SCM_NIL;
#ifdef GD_XPM
  r = Scm_Cons(SCM_INTERN("xpm"), r);
#endif /* GD_XPM */
#ifdef GD_PNG
  r = Scm_Cons(SCM_INTERN("png"), r);
#endif /* GD_PNG */
#ifdef GD_JPEG
  r = Scm_Cons(SCM_INTERN("jpeg"), r);
#endif /* GD_JPEG */
#ifdef GD_GIF
  r = Scm_Cons(SCM_INTERN("gif"), r);
#endif /* GD_GIF */
#ifdef GD_FREETYPE
  r = Scm_Cons(SCM_INTERN("freetype"), r);
#endif /* GD_FREETYPE */
#ifdef GD_FONTCONFIG
  r = Scm_Cons(SCM_INTERN("fontconfig"), r);
#endif /* GD_FONTCONFIG */
  return r;
}

const char *
graphicsGdGetVersion(void)
{
  return GDLIB_VERSION;
}

/*
 * Module initialization function.
 */
extern void Scm_Init_graphics_gdlib(ScmModule*);

ScmObj
Scm_Init_graphics_gd(void)
{
  const char *module_name = "graphics.gd";
  ScmModule *mod;

  SCM_INIT_EXTENSION(graphics_gd);

  mod = SCM_MODULE(SCM_FIND_MODULE(module_name, TRUE));

  GraphicsGdImageClass =
	Scm_MakeForeignPointerClass(mod, "<gd-image>",
								NULL, graphicsGdImageCleanUp, SCM_FOREIGN_POINTER_KEEP_IDENTITY|SCM_FOREIGN_POINTER_MAP_NULL);
  GraphicsGdFontClass =
	Scm_MakeForeignPointerClass(mod, "<gd-font>",
								NULL, graphicsGdFontCleanUp, SCM_FOREIGN_POINTER_KEEP_IDENTITY|SCM_FOREIGN_POINTER_MAP_NULL);

  sym_destroyed = SCM_INTERN("destroyed?");

  Scm_Init_graphics_gdlib(mod);

  /* the following feature identifiers are available *after* loading. */
#ifdef GD_XPM
  Scm_AddFeature("gauche.ext.graphics.gd.xpm", module_name);
#endif /* GD_XPM */
#ifdef GD_PNG
  Scm_AddFeature("gauche.ext.graphics.gd.png", module_name);
#endif /* GD_PNG */
#ifdef GD_JPEG
  Scm_AddFeature("gauche.ext.graphics.gd.jpeg", module_name);
#endif /* GD_JPEG */
#ifdef GD_GIF
  Scm_AddFeature("gauche.ext.graphics.gd.gif", module_name);
#endif /* GD_GIF */
#ifdef GD_FREETYPE
  Scm_AddFeature("gauche.ext.graphics.gd.freetype", module_name);
#endif /* GD_FREETYPE */
#ifdef GD_FONTCONFIG
  Scm_AddFeature("gauche.ext.graphics.gd.fontconfig", module_name);
#endif /* GD_FONTCONFIG */
}
