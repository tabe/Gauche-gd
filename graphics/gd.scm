;;;
;;;  gd.scm
;;;
;;;   Copyright (c) 2006,2007 Takeshi Abe. All rights reserved.
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;    1. Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;
;;;    2. Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;
;;;    3. Neither the name of the authors nor the names of its contributors
;;;       may be used to endorse or promote products derived from this
;;;       software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
;;;  $Id$

(define-module graphics.gd
  (use gauche.charconv)
  (use gauche.parameter)
  (use srfi-42)
  (export <gd-image>
		  <gd-font>
		  ;;; C Layer API
		  gdMaxColors gdAlphaMax gdAlphaOpaque gdAlphaTransparent gdRedMax gdGreenMax gdBlueMax
		  gd-true-color-get-alpha gd-true-color-get-red gd-true-color-get-green gd-true-color-get-blue
		  gd-alpha-blend
		  gdStyled gdBrushed gdStyledBrushed gdTiled gdTransparent gdAntiAliased
		  gd-image-create gd-image-create-palette gd-image-create-true-color
		  gd-image-create-from-png gd-image-create-from-gif gd-image-create-from-jpeg
		  gd-image-create-from-xbm gd-image-create-from-xpm gd-image-create-from-wbmp
		  gd-image-create-from-gd gd-image-create-from-gd2 gd-image-create-from-gd2-part
		  gd-image-destroy
		  gd-image-set-pixel gd-image-get-pixel gd-image-get-true-color-pixel
		  gd-image-line gd-image-rectangle gd-image-filled-rectangle
		  gd-image-set-clip gd-image-get-clip
		  gd-image-bounds-safe
		  gd-image-char gd-image-char-up
		  gd-image-string gd-image-string-up
		  gd-font-cache-setup gd-font-cache-shutdown gd-free-font-cache
		  gd-image-string-ft
		  gdFTEX_LINESPACE gdFTEX_CHARMAP gdFTEX_RESOLUTION gdFTEX_DISABLE_KERNING
		  gdFTEX_XSHOW gdFTEX_FONTPATHNAME gdFTEX_FONTCONFIG gdFTEX_RETURNFONTPATHNAME
		  gd-ft-use-font-config
		  gdFTEX_Unicode gdFTEX_Shift_JIS gdFTEX_Big5
		  gd-image-polygon gd-image-open-polygon gd-image-filled-polygon
		  gd-image-color-allocate gd-image-color-allocate-alpha
		  gd-image-color-closest gd-image-color-closest-alpha gd-image-color-closest-hwb
		  gd-image-color-exact gd-image-color-exact-alpha
		  gd-image-color-resolve gd-image-color-resolve-alpha
		  gd-true-color gd-true-color-alpha
		  gd-image-color-deallocate
		  gd-image-create-palette-from-true-color gd-image-true-color-to-palette
		  gd-image-color-transparent gd-image-palette-copy
		  gdArc gdPie gdChord gdNoFill gdEdged
		  gd-image-filled-arc gd-image-arc gd-image-filled-ellipse gd-image-fill-to-border gd-image-fill
		  gd-image-copy gd-image-copy-merge gd-image-copy-merge-gray
		  gd-image-copy-resized gd-image-copy-resampled gd-image-copy-rotated
		  gd-image-set-brush gd-image-set-tile
		  gd-image-set-anti-aliased gd-image-set-anti-aliased-dont-blend
		  gd-image-set-thickness gd-image-interlace gd-image-alpha-blending gd-image-save-alpha
		  gd-image-true-color gd-image-sx gd-image-sy
		  gd-image-colors-total gd-image-red gd-image-green gd-image-blue gd-image-alpha
		  gd-image-get-transparent gd-image-get-interlaced
		  gd-image-palette-pixel gd-image-true-color-pixel
		  GD2_CHUNKSIZE GD2_CHUNKSIZE_MIN GD2_CHUNKSIZE_MAX
		  GD2_VERS GD2_ID
		  GD2_FMT_RAW GD2_FMT_COMPRESSED
		  gd-image-compare
		  GD_CMP_IMAGE GD_CMP_NUM_COLORS GD_CMP_COLOR GD_CMP_SIZE_X GD_CMP_SIZE_Y
		  GD_CMP_TRANSPARENT GD_CMP_BACKGROUND GD_CMP_INTERLACE GD_CMP_TRUECOLOR
		  GD_RESOLUTION
		  gd-image-square-to-circle gd-image-sharpen ;; gdfx.h
		  gd-font-get-giant gd-font-get-large gd-font-get-medium-bold gd-font-get-small gd-font-get-tiny ;; gdfont*.h

		  ;;; Simple API
		  <gd-error>
		  destroy!
		  set-pixel! get-pixel
		  current-gd-image-format with-gd-image-format
		  read-gd-image
		  write-object
		  write-as
		  save-as
		  line! rectangle!
		  set-clip! get-clip bounds-safe?
		  current-ft-font current-ft-fg current-ft-pt current-ft-angle with-ft-font/fg/pt/angle
		  char! string! polygon!
		  color-allocate! color-closest color-exact! color-resolve! color-deallocate!
		  true-color->palette true-color->palette!
		  color-transparent! palette-copy!
		  gif-anim-begin gif-anim-add gif-anim-end gif-anim-with
		  arc! ellipse! fill! copy!
		  set-brush! set-tile! set-anti-aliased! set-style! set-thickness! interlace! alpha-blending! save-alpha!
		  get-width get-height
		  colors-total
		  get-red get-green get-blue get-alpha get-transparent get-interlaced
		  true-color? palette?
		  object-equal?
		  *gd-features*
		  *gd-version*
          pixel-fold pixel-for-each
		  ))
(select-module graphics.gd)

(dynamic-load "graphics_gd")

(define-condition-type <gd-error> <error> #f
  (error-code))

(define-constant gdFTEX_LINESPACE            1)
(define-constant gdFTEX_CHARMAP              2)
(define-constant gdFTEX_RESOLUTION           4)
(define-constant gdFTEX_DISABLE_KERNING      8)
(define-constant gdFTEX_XSHOW               16)
(define-constant gdFTEX_FONTPATHNAME        32)
(define-constant gdFTEX_FONTCONFIG          64)
(define-constant gdFTEX_RETURNFONTPATHNAME 128)

(define-constant gdFTEX_Unicode   0)
(define-constant gdFTEX_Shift_JIS 1)
(define-constant gdFTEX_Big5      2)

(define current-gd-image-format (make-parameter 'gif))
(define (with-gd-image-format fmt thunk)
  (parameterize ((current-gd-image-format fmt))
	(thunk)))

(define-method read-gd-image ((port <port>))
  (read-gd-image port (current-gd-image-format)))
(define-method read-gd-image ((port <port>) (fmt <symbol>) . rest)
  (case fmt
	((gif) (gd-image-create-gif-port port))
	((jpg jpeg jpe) (gd-image-create-jpeg-port port))
	((png) (gd-image-create-png-port port))
	((wbmp) (gd-image-create-wbmp-port port))
	((gd) (gd-image-create-gd-port port))
	((gd2)
	 (let-keywords* rest ((x #f)
						  (y #f)
						  (w #f)
						  (h #f))
	   (if (and x y w h)
		   (gd-image-create-gd2-part-port port x y w h)
		   (gd-image-create-gd2-port port))))
	(else (error "unknown format:" fmt))))

(define-method write-object ((im <gd-image>) port)
  (write-as im (current-gd-image-format) port))

(define-method write-as ((im <gd-image>) (fmt <symbol>) port . rest)
  (case fmt
	((gif)
	 (gd-image-write-as-gif im port))
	((jpg jpeg jpe)
	 (let-keywords* rest ((quality -1))
	   (gd-image-write-as-jpeg im port quality)))
	((png)
	 (gd-image-write-as-png im port))
	((wbmp)
	 (let-keywords* rest ((foreground -1))
	   (gd-image-write-as-wbmp im foreground port)))
	(else (error "unknown format:" fmt))))

(define-method save-as ((im <gd-image>) (path <string>))
  (let ((s (string-split path #\.)))
	(if (= 1 (length s))
		(gd-image-save-as-gd2 im path)
		(save-as im path (string->symbol (car (last-pair s)))))))
(define-method save-as ((im <gd-image>) (path <string>) (fmt <symbol>) . rest)
  (case fmt
	((gif)
	 (gd-image-save-as-gif im path))
	((jpg jpeg jpe)
	 (let-keywords* rest ((quality -1))
	   (gd-image-save-as-jpeg im path quality)))
	((png)
	 (gd-image-save-as-png im path))
	((wbmp)
	 (let-keywords* rest ((foreground -1))
	   (gd-image-save-as-wbmp im foreground path)))
	((gd)
	 (gd-image-save-as-gd im path))
	((gd2)
	 (let-keywords* rest ((chunk-size 0)
						  (compress #t))
	   (gd-image-save-as-gd2 im path chunk-size (if compress GD2_FMT_COMPRESSED GD2_FMT_RAW))))
	(else
	 (error "unkown format:" fmt))))

(define-method destroy! ((im <gd-image>))
  (gd-image-destroy im))

(define-method set-pixel! ((im <gd-image>) (x <integer>) (y <integer>) (color <integer>))
  (gd-image-set-pixel im x y color))
(define-method get-pixel ((im <gd-image>) (x <integer>) (y <integer>) . rest)
  (let-keywords* rest ((true-color #f))
	(if true-color
		(gd-image-get-true-color-pixel im x y)
		(gd-image-get-pixel im x y))))

(define-method line! ((im <gd-image>) (x1 <integer>) (y1 <integer>) (x2 <integer>) (y2 <integer>) (color <integer>))
  (gd-image-line im x1 y1 x2 y2 color))
(define-method rectangle! ((im <gd-image>) (x1 <integer>) (y1 <integer>) (x2 <integer>) (y2 <integer>) (color <integer>) . rest)
  (let-keywords* rest ((filled #f))
	((if filled gd-image-filled-rectangle gd-image-rectangle) im x1 y1 x2 y2 color)))

(define-method set-clip! ((im <gd-image>) (x1 <integer>) (y1 <integer>) (x2 <integer>) (y2 <integer>))
  (gd-image-set-clip im x1 y1 x2 y2))
(define-method get-clip ((im <gd-image>))
  (gd-image-get-clip im))
(define-method bounds-safe? ((im <gd-image>) (x <integer>) (y <integer>))
  (= 1 (gd-image-bounds-safe im x y)))

(define-method char! ((im <gd-image>) (f <gd-font>) (x <integer>) (y <integer>) (c <char>) (color <integer>) . rest)
  (let-keywords* rest ((direction 'right))
	(case direction
	  ((right) (gd-image-char im f x y (char->integer c) color))
	  ((up)    (gd-image-char-up im f x y (char->integer c) color))
	  (else    (error "unknown direction: " direction)))))

(define current-ft-font  (make-parameter #f))
(define current-ft-fg    (make-parameter #f))
(define current-ft-pt    (make-parameter 12))
(define current-ft-angle (make-parameter 0))
(define (with-ft-font/fg/pt/angle font fg pt angle thunk)
  (parameterize ((current-ft-font  font)
				 (current-ft-fg    fg)
				 (current-ft-pt    pt)
				 (current-ft-angle angle))
	(thunk)))

(define-method string! ((im <gd-image>) (f <gd-font>) (x <integer>) (y <integer>) (str <string>) (color <integer>) . rest)
  (let-keywords* rest ((direction 'right))
	(case direction
	  ((right) (gd-image-string im f x y str color))
	  ((up)    (gd-image-string-up im f x y str color))
	  (else    (error "unknown direction: " direction)))))
(define-method string! ((im <gd-image>) (fg <integer>) (fontlist <string>) (ptsize <real>) (angle <real>) (x <integer>) (y <integer>) (str <string>))
  (gd-image-string-ft im fg fontlist ptsize angle x y (ces-convert str (gauche-character-encoding) 'UTF8)))
(define-method string! ((im <gd-image>) (x <integer>) (y <integer>) (str <string>) . rest)
  (let-keywords* rest ((font  (current-ft-font))
					   (fg    (current-ft-fg))
					   (pt    (current-ft-pt))
					   (angle (current-ft-angle)))
	(gd-image-string-ft im fg font pt angle x y (ces-convert str (gauche-character-encoding) 'UTF8))))

(define-method polygon! ((im <gd-image>) (points <list>) (pointsTotal <integer>) (color <integer>) . rest)
  (let-keywords* rest ((option #f))
	(case option
	  ((filled) (gd-image-filled-polygon im points pointsTotal color))
	  ((open)   (gd-image-open-polygon   im points pointsTotal color))
	  (else     (gd-image-polygon        im points pointsTotal color)))))
(define-method polygon! ((im <gd-image>) (points <list>) (color <integer>) . rest)
  (apply polygon! im points (length points) color rest))

(define-method color-allocate! ((im <gd-image>) (r <integer>) (g <integer>) (b <integer>))
  (gd-image-color-allocate im r g b))
(define-method color-allocate! ((im <gd-image>) (r <integer>) (g <integer>) (b <integer>) (a <integer>))
  (gd-image-color-allocate-alpha im r g b a))

(define-method color-closest ((im <gd-image>) (r <integer>) (g <integer>) (b <integer>) (a <integer>))
  (let ((result (gd-image-color-closest-alpha im r g b a)))
	(and (<= 0 result) result)))
(define-method color-closest ((im <gd-image>) (r <integer>) (g <integer>) (b <integer>) . rest)
  (let-keywords* rest ((hwb #f))
	(let ((result ((if hwb
					   gd-image-color-closest-hwb
					   gd-image-color-closest) im r g b)))
	  (and (<= 0 result) result))))

(define-method color-exact! ((im <gd-image>) (r <integer>) (g <integer>) (b <integer>))
  (let ((result (gd-image-color-exact im r g b)))
	(and (<= 0 result) result)))
(define-method color-exact! ((im <gd-image>) (r <integer>) (g <integer>) (b <integer>) (a <integer>))
  (let ((result (gd-image-color-exact-alpha im r g b a)))
	(and (<= 0 result) result)))

(define-method color-resolve! ((im <gd-image>) (r <integer>) (g <integer>) (b <integer>))
  (gd-image-color-resolve im r g b))
(define-method color-resolve! ((im <gd-image>) (r <integer>) (g <integer>) (b <integer>) (a <integer>))
  (gd-image-color-resolve-alpha im r g b a))

(define-method color-deallocate! ((im <gd-image>) (color <integer>))
  (gd-image-color-deallocate im color))

(define-method true-color->palette ((im <gd-image>) (ditherFlag <integer>) (colorsWanted <integer>))
  (gd-image-create-palette-from-true-color im ditherFlag colorsWanted))
(define-method true-color->palette! ((im <gd-image>) (ditherFlag <integer>) (colorsWanted <integer>))
  (gd-image-true-color-to-palette im ditherFlag colorsWanted)
  im)

(define-method gif-anim-begin ((im <gd-image>) (oport <port>) (GlobalCM <integer>) (loops <integer>))
  (gd-image-gif-anim-begin-port im oport GlobalCM loops))
(define-method gif-anim-add ((im <gd-image>) (oport <port>) (localCM <integer>) (LeftOfs <integer>) (TopOfs <integer>) (Delay <integer>) (Disposal <integer>) (previm <gd-image>))
  (gd-image-gif-anim-add-port im oport localCM LeftOfs TopOfs Delay Disposal previm))
(define-method gif-anim-add ((im <gd-image>) (oport <port>) (localCM <integer>) (LeftOfs <integer>) (TopOfs <integer>) (Delay <integer>) (Disposal <integer>))
  (gd-image-gif-anim-add-port im oport localCM LeftOfs TopOfs Delay Disposal #f))
(define-method gif-anim-end ((oport <port>))
  (gd-image-gif-anim-end-port oport))
(define (gif-anim-with im oport thunk . rest)
  (let-keywords* rest ((global-cm -1)
					   (loop -1))
	(gd-image-gif-anim-begin-port im oport global-cm loop)
	(thunk)
	(gd-image-gif-anim-end-port oport)))

(define-method color-transparent! ((im <gd-image>) (color <integer>))
  (gd-image-color-transparent im color))
(define-method palette-copy! ((dst <gd-image>) (src <gd-image>))
  (gd-image-palette-copy dst src))

(define-method arc! ((im <gd-image>) (cx <integer>) (cy <integer>) (w <integer>) (h <integer>) (s <integer>) (e <integer>) (color <integer>) . rest)
  (let-keywords* rest ((filled #f))
	(if filled
		(gd-image-filled-arc im cx cy w h s e color filled)
		(gd-image-arc im cx cy w h s e color))))
(define-method ellipse! ((im <gd-image>) (cx <integer>) (cy <integer>) (w <integer>) (h <integer>) (color <integer>) . rest)
  (let-keywords* rest ((filled #f))
	(if filled
		(gd-image-filled-ellipse im cx cy w h color)
		(gd-image-arc im cx cy w h 0 360 color))))
(define-method fill! ((im <gd-image>) (x <integer>) (y <integer>) (color <integer>) . rest)
  (let-keywords* rest ((border #f))
	(if border
		(gd-image-fill-to-border im x y border color)
		(gd-image-fill im x y color))))

(define-method copy! ((dst <gd-image>) (src <gd-image>) (dstX <integer>) (dstY <integer>) (srcX <integer>) (srcY <integer>) (w <integer>) (h <integer>))
  (gd-image-copy dst src dstX dstY srcX srcY w h))
(define-method copy! ((dst <gd-image>) (src <gd-image>) (dstX <integer>) (dstY <integer>) (srcX <integer>) (srcY <integer>) (w <integer>) (h <integer>) (pct <integer>) . rest)
  (let-keywords* rest ((gray #f))
	((if gray gd-image-copy-merge-gray gd-image-copy-merge) dst src dstX dstY srcX srcY w h pct)))
(define-method copy! ((dst <gd-image>) (src <gd-image>) (dstX <integer>) (dstY <integer>) (srcX <integer>) (srcY <integer>) (dstW <integer>) (dstH <integer>) (srcW <integer>) (srcH <integer>) . rest)
  (let-keywords* rest ((resampled #f))
	((if resampled gd-image-copy-resampled gd-image-copy-resized) dst src dstX dstY srcX srcY dstW dstH srcW srcH)))
(define-method copy! ((dst <gd-image>) (src <gd-image>) (dstX <real>) (dstY <real>) (srcX <integer>) (srcY <integer>) (srcW <integer>) (srcH <integer>) (angle <integer>))
  (gd-image-copy-rotated dst src dstX dstY srcX srcY srcW srcH angle))

(define-method set-brush! ((im <gd-image>) (brush <gd-image>))
  (gd-image-set-brush im brush))
(define-method set-tile! ((im <gd-image>) (tile <gd-image>))
  (gd-image-set-tile im tile))

(define-method set-anti-aliased! ((im <gd-image>) (c <integer>))
  (gd-image-set-anti-aliased im c))
(define-method set-anti-aliased! ((im <gd-image>) (c <integer>) (dont-blend <integer>))
  (gd-image-set-anti-aliased-dont-blend im c dont-blend))

(define-method set-style! ((im <gd-image>) (style <list>) (styleLength <integer>))
  (gd-image-set-style im style styleLength))
(define-method set-style! ((im <gd-image>) (style <list>))
  (let ((len (length style)))
	(gd-image-set-style im style len)))

(define-method set-thickness! ((im <gd-image>) (thickness <integer>))
  (gd-image-set-thickness im thickness))
(define-method interlace! ((im <gd-image>) (interlaceArg <integer>))
  (gd-image-interlace im interlaceArg))
(define-method alpha-blending! ((im <gd-image>) (alphaBlendingArg <integer>))
  (gd-image-alpha-blending im alphaBlendingArg))
(define-method save-alpha! ((im <gd-image>) (saveAlphaFlag <integer>))
  (gd-image-save-alpha im saveAlphaFlag))

(define-method get-width ((im <gd-image>))
  (gd-image-sx im))
(define-method get-height ((im <gd-image>))
  (gd-image-sy im))

(define-method colors-total ((im <gd-image>))
  (gd-image-colors-total im))

(define-method get-red ((im <gd-image>) (c <integer>))
  (gd-image-red im c))
(define-method get-green ((im <gd-image>) (c <integer>))
  (gd-image-green im c))
(define-method get-blue ((im <gd-image>) (c <integer>))
  (gd-image-blue im c))
(define-method get-alpha ((im <gd-image>) (c <integer>))
  (gd-image-alpha im c))

(define-method get-transparent ((im <gd-image>))
  (gd-image-get-transparent im))
(define-method get-interlaced ((im <gd-image>))
  (gd-image-get-interlaced im))

(define-method true-color? ((im <gd-image>))
  (not (= 0 (gd-image-true-color im))))
(define-method palette? ((im <gd-image>))
  (= 0 (gd-image-true-color im)))

(define-method object-equal? ((im1 <gd-image>) (im2 <gd-image>))
  (= 0 (gd-image-compare im1 im2)))

(define-constant *gd-features* (gd-get-features))
(define-constant *gd-version*  (gd-get-version))

(define-method pixel-for-each ((im <gd-image>) proc)
  (do-ec (: x (gd-image-sx im)) (: y (gd-image-sy im))
         (proc x y (gd-image-get-pixel im x y))))

(define-method pixel-fold ((im <gd-image>) proc knil)
  (fold-ec knil
           (: x (gd-image-sx im)) (: y (gd-image-sy im))
           (list x y (gd-image-get-pixel im x y))
           (lambda (temp seed)
             (set-cdr! (last-pair temp) (list seed))
             (apply proc temp))))

(provide "graphics/gd")
