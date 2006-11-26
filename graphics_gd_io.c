/*
 *  graphics_gd_io.c
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

#include "graphics_gd_io.h"

static int
graphicsGdIOCtxGetC(struct gdIOCtx *ctx)
{
  graphicsGdIOCtx *gCtx;
  gCtx = (graphicsGdIOCtx *)ctx;
  return Scm_Getc(gCtx->port);
}

static int
graphicsGdIOCtxGetBuf(struct gdIOCtx *ctx, void *buf, int size)
{
  graphicsGdIOCtx *gCtx;
  gCtx = (graphicsGdIOCtx *)ctx;
  return Scm_Getz((char *)buf, size, gCtx->port);
}

static void
graphicsGdIOCtxPutC(struct gdIOCtx *ctx, int a)
{
  graphicsGdIOCtx *gCtx;
  gCtx = (graphicsGdIOCtx *)ctx;
  SCM_PUTB((ScmByte)a, gCtx->port);
}

static int
graphicsGdIOCtxPutBuf(struct gdIOCtx *ctx, const void *buf, int size)
{
  graphicsGdIOCtx *gCtx;
  gCtx = (graphicsGdIOCtx *)ctx;
  Scm_Putz((const char *)buf, size, gCtx->port);
  return size;
}

static int
graphicsGdIOCtxSeek(struct gdIOCtx *ctx, const int pos)
{
  graphicsGdIOCtx *gCtx;
  ScmObj r;
  gCtx = (graphicsGdIOCtx *)ctx;
  r = Scm_PortSeek(gCtx->port, Scm_OffsetToInteger((off_t)pos), SEEK_SET);
  return !SCM_FALSEP(r);
}

static long
graphicsGdIOCtxTell(struct gdIOCtx *ctx)
{
  graphicsGdIOCtx *gCtx;
  ScmObj r;
  gCtx = (graphicsGdIOCtx *)ctx;
  r = Scm_PortSeek(gCtx->port, Scm_OffsetToInteger((off_t)0), SEEK_CUR);
  if (SCM_FALSEP(r)) return -1;
  return (long)Scm_GetInteger(r);
}

static void
graphicsGdIOCtxFree(struct gdIOCtx *ctx)
{
  free(ctx);
}

gdIOCtx *
graphicsGdGetIOCtxFromPort(ScmPort *port)
{
  graphicsGdIOCtx *ctx;
  SCM_ASSERT(SCM_PORTP(port));
  if ( (ctx = (graphicsGdIOCtx *)malloc(sizeof(graphicsGdIOCtx))) == NULL) {
	return NULL;
  }
  ctx->ctx.getC    = graphicsGdIOCtxGetC;
  ctx->ctx.getBuf  = graphicsGdIOCtxGetBuf;
  ctx->ctx.putC    = graphicsGdIOCtxPutC;
  ctx->ctx.putBuf  = graphicsGdIOCtxPutBuf;
  ctx->ctx.seek    = graphicsGdIOCtxSeek;
  ctx->ctx.tell    = graphicsGdIOCtxTell;
  ctx->ctx.gd_free = graphicsGdIOCtxFree;
  ctx->port        = port;
  return (gdIOCtx *)ctx;
}
