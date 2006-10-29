/*
 *  graphics_gd.c
 *
 *   Copyright (c) 2006 Takeshi Abe. All rights reserved.
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

#define PROPER_GRAPHICS_GD_IMAGE_CREATE_FROM(fmt) int					\
  graphicsGdImageCreateFrom ## fmt(gdImage **dst, const char *path)		\
  {																		\
	FILE *f;															\
	struct flock fl;													\
	int fd;																\
	gdImage *im;														\
	if ( (f = fopen(path, "rb")) == NULL) {								\
	  graphicsGdRaiseCondition("could not open file: %s", path);		\
	  return -1;														\
	}																	\
	fl.l_type = F_RDLCK;												\
	fl.l_whence = SEEK_SET;												\
	fl.l_start = fl.l_len = 0;											\
	fd = fileno(f);														\
	if (fcntl(fd, F_SETLKW, &fl) != 0) {								\
	  graphicsGdRaiseCondition("could not lock file: %s", path);		\
	  return -2;														\
	}																	\
	*dst = gdImageCreateFrom ## fmt(f);									\
	if (fclose(f) == 0) {												\
	  return 0;															\
	} else {															\
	  graphicsGdRaiseCondition("could not close file: %s", path);		\
	  return -3;														\
	}																	\
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
graphicsGdImageCreateFromXpm(gdImage **dst, const char *path)
{
#ifdef GD_XPM
  size_t len = strlen(path);
  char *temp = calloc(len + 1, sizeof(char)); /* with terminating `\0' */
  if (temp == NULL) {
	graphicsGdRaiseCondition("calloc failed: %s", "graphicsGdImageCreateFromXpm()");
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

#define PROPER_GRAPHICS_GD_IMAGE_SAVE_AS(fmt, args) int					\
  graphicsGdImageSaveAs ## fmt(gdImage *im, const char *path)			\
  {																		\
	FILE *f;															\
	struct flock fl;													\
	int fd;																\
	if ( (f = fopen(path, "wb")) == NULL) {								\
	  graphicsGdRaiseCondition("could not open file: %s", path);		\
	  return -1;														\
	}																	\
	fl.l_type = F_WRLCK;												\
	fl.l_whence = SEEK_SET;												\
	fl.l_start = fl.l_len = 0;											\
	fd = fileno(f);														\
	if (fcntl(fd, F_SETLKW, &fl) != 0) {								\
	  graphicsGdRaiseCondition("could not lock file: %s", path);		\
	  return -2;														\
	}																	\
	gdImage ## fmt args;												\
	if (fclose(f) == 0) {												\
	  return 0;															\
	} else {															\
	  graphicsGdRaiseCondition("could not close file: %s", path);		\
	  return -3;														\
	}																	\
  }

#define IMPROPER_GRAPHICS_GD_IMAGE_SAVE_AS(fmt) int				\
  graphicsGdImageSaveAs ## fmt(gdImage *im, const char *path)	\
  {																\
	graphicsGdRaiseCondition("unsupported format: %s", #fmt);	\
	return -1;													\
  }

#ifdef GD_JPEG
PROPER_GRAPHICS_GD_IMAGE_SAVE_AS(Jpeg, (im, f, -1))
#else
IMPROPER_GRAPHICS_GD_IMAGE_SAVE_AS(Jpeg)
#endif /* GD_JPEG */

PROPER_GRAPHICS_GD_IMAGE_SAVE_AS(WBMP, (im, -1, f))

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
PROPER_GRAPHICS_GD_IMAGE_SAVE_AS(Gd2, (im, f, 0, GD2_FMT_COMPRESSED))

#undef PROPER_GRAPHICS_GD_IMAGE_SAVE_AS
#undef IMPROPER_GRAPHICS_GD_IMAGE_SAVE_AS

#define DEFINE_GRAPHICS_GD_IMAGE_POLYGON(name) void \
  graphicsGdImage ## name(gdImage *im, ScmObj points, int pointsTotal, int color) \
  {																		\
	ScmObj head, pair;													\
gdPoint *p, *q;														\
	int i = 0;															\
	SCM_ASSERT(SCM_LISTP(points) && SCM_PROPER_LIST_P(points) && pointsTotal >= 0);	\
	p = q = calloc(pointsTotal, sizeof(gdPoint));						\
	if (p == NULL) {													\
	  graphicsGdRaiseCondition("calloc failed: %s", "graphicsGdImage" #name "()"); \
	  return;															\
	}																	\
	SCM_FOR_EACH (head, points) {										\
	  if (i++ == pointsTotal) break;									\
	  pair = SCM_CAR(head);												\
	  SCM_ASSERT(SCM_PAIRP(pair));										\
	  q->x = SCM_INT_VALUE(SCM_CAR(pair));								\
	  q->y = SCM_INT_VALUE(SCM_CDR(pair));								\
	  q++;																\
	}																	\
	gdImage ## name(im, p, pointsTotal, color);							\
	free(p);															\
  }

DEFINE_GRAPHICS_GD_IMAGE_POLYGON(Polygon)
DEFINE_GRAPHICS_GD_IMAGE_POLYGON(OpenPolygon)
DEFINE_GRAPHICS_GD_IMAGE_POLYGON(FilledPolygon)

#undef DEFINE_GRAPHICS_GD_IMAGE_POLYGON

void
graphicsGdImageSetStyle(gdImage *im, ScmObj style, int styleLength)
{
  ScmObj head;
  int *p, *q, i = 0;
  SCM_ASSERT(SCM_LISTP(style) && SCM_PROPER_LIST_P(style) && styleLength >= 0);
  p = q = calloc(styleLength, sizeof(int));
  if (p == NULL) {
	graphicsGdRaiseCondition("calloc failed: %s", "graphicsGdImageSetStyle()");
	return;
  }
  SCM_FOR_EACH (head, style) {
	if (i++ == styleLength) break;
	*q++ = SCM_INT_VALUE(SCM_CAR(head));
  }
  gdImageSetStyle(im, p, styleLength);
  free(p);
}

#define PROPER_GRAPHICS_GD_FONT_GET(size) int \
  graphicsGdFontGet ## size(gdFont **dst)		   \
  {												   \
	*dst = gdFontGet ## size();					   \
	return 0;									   \
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

int
graphicsGdImageStringFT(gdImage *im,
						ScmPair **brect0, ScmPair **brect1, ScmPair **brect2, ScmPair **brect3,
						int fg, char *fontlist, double ptsize, double angle, int x, int y, ScmString *str)
{
  int brect[8];
  char *s = Scm_GetString(str);
  char *e = gdImageStringFT(im, brect, fg, fontlist, ptsize, angle, x, y, s);
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

ScmObj
Scm_Init_graphics_gd(void)
{
  ScmModule *mod;

  SCM_INIT_EXTENSION(graphics_gd);

  mod = SCM_MODULE(SCM_FIND_MODULE("graphics.gd", TRUE));

  GraphicsGdImageClass =
	Scm_MakeForeignPointerClass(mod, "<gd-image>",
								NULL, graphicsGdImageCleanUp, SCM_FOREIGN_POINTER_KEEP_IDENTITY|SCM_FOREIGN_POINTER_MAP_NULL);
  GraphicsGdFontClass =
	Scm_MakeForeignPointerClass(mod, "<gd-font>",
								NULL, graphicsGdFontCleanUp, SCM_FOREIGN_POINTER_KEEP_IDENTITY|SCM_FOREIGN_POINTER_MAP_NULL);

  sym_destroyed = SCM_INTERN("destroyed?");

  Scm_Init_graphics_gdlib(mod);
}
