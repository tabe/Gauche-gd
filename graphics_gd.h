/*
 *  graphics_gd.h
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

#ifndef GAUCHE_GRAPHICS_GD_H
#define GAUCHE_GRAPHICS_GD_H

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

#include <gd.h>

#include <gauche.h>
#include <gauche/extend.h>

SCM_DECL_BEGIN

extern ScmClass *GraphicsGdImageClass;
#define GRAPHICS_GD_IMAGE_P(obj)     SCM_XTYPEP(obj, GraphicsGdImageClass)
#define GRAPHICS_GD_IMAGE_UNBOX(obj) SCM_FOREIGN_POINTER_REF(gdImage *, obj)
#define GRAPHICS_GD_IMAGE_BOX(ctx)   Scm_MakeForeignPointer(GraphicsGdImageClass, ctx)

extern ScmClass *GraphicsGdFontClass;
#define GRAPHICS_GD_FONT_P(obj)     SCM_XTYPEP(obj, GraphicsGdFontClass)
#define GRAPHICS_GD_FONT_UNBOX(obj) SCM_FOREIGN_POINTER_REF(gdFont *, obj)
#define GRAPHICS_GD_FONT_BOX(ctx)   Scm_MakeForeignPointer(GraphicsGdFontClass, ctx)

SCM_DECL_END

#endif  /* GAUCHE_GRAPHICS_GD_H */
