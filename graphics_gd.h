/*
 *  graphics_gd.h
 *
 *   Copyright (c) 2006,2007 Takeshi Abe. All rights reserved.
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

#ifndef GAUCHE_GRAPHICS_GD_H
#define GAUCHE_GRAPHICS_GD_H

#include <gauche.h>
#include <gauche/extend.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

#include <gd.h>
#ifdef HAVE_GDFONTG_H
#include <gdfontg.h>
#endif /* HAVE_GDFONTG_H */
#ifdef HAVE_GDFONTL_H
#include <gdfontl.h>
#endif /* HAVE_GDFONTL_H */
#ifdef HAVE_GDFONTMB_H
#include <gdfontmb.h>
#endif /* HAVE_GDFONTMB_H */
#ifdef HAVE_GDFONTS_H
#include <gdfonts.h>
#endif /* HAVE_GDFONTS_H */
#ifdef HAVE_GDFONTT_H
#include <gdfontt.h>
#endif /* HAVE_GDFONTT_H */

SCM_DECL_BEGIN

extern ScmClass *GraphicsGdImageClass;
#define GRAPHICS_GD_IMAGE_P(obj)     SCM_XTYPEP(obj, GraphicsGdImageClass)
#define GRAPHICS_GD_IMAGE_UNBOX(obj) SCM_FOREIGN_POINTER_REF(gdImage *, obj)
#define GRAPHICS_GD_IMAGE_BOX(ctx)   Scm_MakeForeignPointer(GraphicsGdImageClass, ctx)

extern ScmClass *GraphicsGdFontClass;
#define GRAPHICS_GD_FONT_P(obj)     SCM_XTYPEP(obj, GraphicsGdFontClass)
#define GRAPHICS_GD_FONT_UNBOX(obj) SCM_FOREIGN_POINTER_REF(gdFont *, obj)
#define GRAPHICS_GD_FONT_BOX(ctx)   Scm_MakeForeignPointer(GraphicsGdFontClass, ctx)

int graphicsGdImageDestroyedP(ScmObj obj);
void graphicsGdImageMarkDestroyed(ScmObj obj);

int graphicsGdImageCreateFromPng(gdImage **dst, const char *path);
int graphicsGdImageCreateFromGif(gdImage **dst, const char *path);
int graphicsGdImageCreateFromJpeg(gdImage **dst, const char *path);
int graphicsGdImageCreateFromWBMP(gdImage **dst, const char *path);
int graphicsGdImageCreateFromGd(gdImage **dst, const char *path);
int graphicsGdImageCreateFromGd2(gdImage **dst, const char *path);
int graphicsGdImageCreateFromGd2Part(gdImage **dst, const char *path, int srcx, int srcy, int w, int h);
int graphicsGdImageCreateFromXbm(gdImage **dst, const char *path);
int graphicsGdImageCreateFromXpm(gdImage **dst, const char *path);
int graphicsGdImageCreatePngPort(gdImage **dst, ScmPort *port);
int graphicsGdImageCreateGifPort(gdImage **dst, ScmPort *port);
int graphicsGdImageCreateJpegPort(gdImage **dst, ScmPort *port);
int graphicsGdImageCreateWBMPPort(gdImage **dst, ScmPort *port);
int graphicsGdImageCreateGdPort(gdImage **dst, ScmPort *port);
int graphicsGdImageCreateGd2Port(gdImage **dst, ScmPort *port);
int graphicsGdImageCreateGd2PartPort(gdImage **dst, ScmPort *port, int srcx, int srcy, int w, int h);
void graphicsGdImageWriteAsJpeg(gdImage *im, ScmPort *port, int quality);
void graphicsGdImageWriteAsWBMP(gdImage *im, int fg, ScmPort *port);
void graphicsGdImageWriteAsPng(gdImage *im, ScmPort *port);
void graphicsGdImageWriteAsGif(gdImage *im, ScmPort *port);
int graphicsGdImageSaveAsJpeg(gdImage *im, const char *path, int quality);
int graphicsGdImageSaveAsWBMP(gdImage* im, int fg, const char *path);
int graphicsGdImageSaveAsPng(gdImage *im, const char *path);
int graphicsGdImageSaveAsGif(gdImage *im, const char *path);
int graphicsGdImageSaveAsGd(gdImage *im, const char *path);
int graphicsGdImageSaveAsGd2(gdImage* im, const char *path, int chunkSize, int fmt);
void graphicsGdImageGifAnimBeginPort(gdImage *im, ScmPort *oport, int GlobalCM, int loops);
void graphicsGdImageGifAnimAddPort(gdImage *im, ScmPort *oport, int localCM, int LeftOfs, int TopOfs, int Delay, int Disposal, gdImage *previm);
void graphicsGdImageGifAnimEndPort(ScmPort *oport);
void graphicsGdImagePolygon(gdImage *im, ScmObj points, int pointsTotal, int color);
void graphicsGdImageOpenPolygon(gdImage *im, ScmObj points, int pointsTotal, int color);
void graphicsGdImageFilledPolygon(gdImage *im, ScmObj points, int pointsTotal, int color);
void graphicsGdImageSetStyle(gdImage *im, ScmObj style, int styleLength);
int graphicsGdFontGetGiant(gdFont **dst);
int graphicsGdFontGetLarge(gdFont **dst);
int graphicsGdFontGetMediumBold(gdFont **dst);
int graphicsGdFontGetSmall(gdFont **dst);
int graphicsGdFontGetTiny(gdFont **dst);
void graphicsGdImageString(gdImagePtr im, gdFontPtr f, int x, int y, const char *s, int color);
void graphicsGdImageStringUp(gdImagePtr im, gdFontPtr f, int x, int y, const char *s, int color);
int graphicsGdImageStringFT(gdImage *im,
							ScmPair **brect0, ScmPair **brect1, ScmPair **brect2, ScmPair **brect3,
							int fg, ScmString *fontlist, double ptsize, double angle, int x, int y, ScmString *str);

ScmObj graphicsGdGetFeatures(void);
const char *graphicsGdGetVersion(void);

SCM_DECL_END

#endif  /* GAUCHE_GRAPHICS_GD_H */
