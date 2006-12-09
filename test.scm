;;; -*- coding: euc-jp -*-
;;;
;;; test graphics.gd
;;;

(use gauche.test)
(use gauche.version)

(test-start "graphics.gd")
(use graphics.gd)
(test-module 'graphics.gd)

(test-section "constant")
(define-syntax test*-constant
  (syntax-rules ()
	((_ (name value) ...)
	 (begin
	   (test* (symbol->string 'name) name value)
	   ...))))
(test*-constant
 (gdMaxColors        256)
 (gdAlphaMax         127)
 (gdAlphaOpaque      0)
 (gdAlphaTransparent 127)
 (gdRedMax           255)
 (gdGreenMax         255)
 (gdBlueMax          255)
 (gdStyled           -2)
 (gdBrushed          -3)
 (gdStyledBrushed    -4)
 (gdTiled            -5)
 (gdTransparent      -6)
 (gdAntiAliased      -7)
 (gdFTEX_LINESPACE   1)
 (gdFTEX_CHARMAP     2)
 (gdFTEX_RESOLUTION  4)
 (gdFTEX_DISABLE_KERNING 8)
 (gdFTEX_XSHOW       16)
 (gdFTEX_FONTPATHNAME 32)
 (gdFTEX_FONTCONFIG  64)
 (gdFTEX_RETURNFONTPATHNAME 128)
 (gdFTEX_Unicode     0)
 (gdFTEX_Shift_JIS   1)
 (gdFTEX_Big5        2)
 (gdArc              0)
 (gdPie              0)
 (gdChord            1)
 (gdNoFill           2)
 (gdEdged            4)
 (GD2_CHUNKSIZE      128)
 (GD2_CHUNKSIZE_MIN  64)
 (GD2_CHUNKSIZE_MAX  4096)
 (GD2_VERS           2)
 (GD2_ID             "gd2")
 (GD2_FMT_RAW        1)
 (GD2_FMT_COMPRESSED 2)
 (GD_CMP_IMAGE       1)
 (GD_CMP_NUM_COLORS  2)
 (GD_CMP_COLOR       4)
 (GD_CMP_SIZE_X      8)
 (GD_CMP_SIZE_Y      16)
 (GD_CMP_TRANSPARENT 32)
 (GD_CMP_BACKGROUND  64)
 (GD_CMP_INTERLACE   128)
 (GD_CMP_TRUECOLOR   256)
 (GD_RESOLUTION      96)
 )

(test-section "parameter")
(use gauche.parameter)
(test* "current-gd-image-format" #t (and (is-a? current-gd-image-format <parameter>)
										 (eq? 'gif (current-gd-image-format))))
(test* "current-ft-font" #t (and (is-a? current-ft-font <parameter>) (not (current-ft-font))))
(test* "current-ft-fg" #t (and (is-a? current-ft-fg <parameter>) (not (current-ft-fg))))
(test* "current-ft-pt" 12 (and (is-a? current-ft-pt <parameter>) (current-ft-pt)))
(test* "current-ft-angle" 0 (and (is-a? current-ft-angle <parameter>) (current-ft-angle)))

(test-section "color")
(define color #x789abcde)
(test* "gd-true-color-get-alpha" #x78 (gd-true-color-get-alpha color))
(test* "gd-true-color-get-red"   #x9a (gd-true-color-get-red   color))
(test* "gd-true-color-get-green" #xbc (gd-true-color-get-green color))
(test* "gd-true-color-get-blue"  #xde (gd-true-color-get-blue  color))
(test* "gd-alpha-blend"
	   (if (version<=? "2.0.29" *gd-version*) 33440629 33309300)
	   (gd-alpha-blend #x11ee3355 #x09ff4477))

(test-section "gdImage")
(define im (gd-image-create 240 240))
(test* "gd-image-create" #t (is-a? im <gd-image>))
(define im (gd-image-create-palette 240 240))
(test* "gd-image-create-palette" #t (is-a? im <gd-image>))
(gd-image-destroy im)
(gd-image-destroy im) ;; expected to be repeated safely
(define im (gd-image-create-true-color 240 240))
(test* "gd-image-create-true-color" #t (is-a? im <gd-image>))

(define im (gd-image-create-from-gif "test/screen.gif"))
(test* "gd-image-create-from-gif" #t (is-a? im <gd-image>))
(test* "get-width"  776 (get-width im))
(test* "get-height" 592 (get-height im))
(test* "gd-image-get-pixel" 255 (gd-image-get-pixel im 300 400))
(test* "gd-image-get-true-color-pixel" 16711676 (gd-image-get-true-color-pixel im 300 400))
(define-method resize ((im <gd-image>) (sx <integer>) (sy <integer>))
  (let ((dst (gd-image-create-true-color sx sy)))
	(gd-image-copy-resampled dst im 0 0 0 0 sx sy (gd-image-sx im) (gd-image-sy im))
	dst))
(define im-small (resize im 97 74))
(save-as im-small (if (memq 'png *gd-features*)
					  "test/screen-thumb.png"
					  "test/screen-thumb.gif"))
(destroy! im-small)
(define im-empty (gd-image-create-from-jpeg "test/empty.jpg")) ;; may put a message
(test* "gd-image-create-from-jpeg" #f im-empty)

(define im0 (gd-image-create 320 320))
(define white  (gd-image-color-allocate im0 #xff #xff #xff))
(define green  (gd-image-color-allocate im0 #x00 #xff #x00))
(define color0 (gd-image-color-resolve im0 #x56 #x78 #x9a))
(define color1 (gd-image-color-resolve im0 #x78 #x9a #x56))
(define color2 (gd-image-color-resolve im0 #x78 #xbc #xde))
(gd-image-polygon im0
				  '((150 . 20)
					(170 . 45)
					(200 . 70)
					(160 . 130)
					(160 . 90)
					(130 . 75))
				  6
				  green)
(gd-image-line im0 0 0 100 200 color0)
(gd-image-rectangle im0 100 200 140 100 color1)
(gd-image-filled-rectangle im0 110 190 130 110 color2)
(gd-image-set-clip im0 10 20 450 300)
(receive (x1 y1 x2 y2)
	(gd-image-get-clip im0)
  (test* "gd-image-get-clip" '(10 20 319 300) (list x1 y1 x2 y2)))
(test* "gd-image-bounds-safe" 1 (gd-image-bounds-safe im0 200 300))
(test* "gd-font-cache-setup" 0 (gd-font-cache-setup))
(gd-font-cache-shutdown)
(gd-free-font-cache)
(test* "gd-ft-use-font-config"
	   (if (memq 'fontconfig *gd-features*) 1 0)
	   (gd-ft-use-font-config 1))
(gd-image-interlace im0 1)
(gd-image-arc im0 160 120 40 40 0 270 green)
(save-as im0 (if (memq 'png *gd-features*)
				 "test/im0.png"
				 "test/im0.gif"))
(define im-lambda (gd-image-create-from-xbm "test/lambda.xbm"))
(test* "gd-image-create-from-xbm" #t (begin0
									   (is-a? im-lambda <gd-image>)
									   (save-as im-lambda "test/lambda.gif")))
(save-as im-lambda "test/lambda.wbmp" 'wbmp :foreground 0) ; test the WBMP foreground option

(test-section "gdIOCtx compatible port")
(test* "type error (input port)" *test-error* (read-gd-image #f))
(test* "type error (output port)" *test-error* (write-as im0 'gif #f))

(test-section "read & write")
(current-gd-image-format (if (memq 'png *gd-features*) 'png 'gif))
(define im1
  (call-with-input-file (format #f "test/im0.~a" (current-gd-image-format))
	(lambda (iport)
	  (read-gd-image iport))))
(test* "read-gd-image" #t (is-a? im1 <gd-image>))
(define im2 (gd-image-square-to-circle im1 300))
(test* "gd-image-square-to-circle" #t (is-a? im2 <gd-image>))
(call-with-output-file (format #f "test/im1.~a" (current-gd-image-format))
  (lambda (oport)
	(write im2 oport))) ; it also test `write-as'
(use file.util)
(when (memq 'jpeg *gd-features*)
  (test* "the JPEG quality option"
		 #t
		 (let ((f0 "test/screen-high.jpg")
			   (f1 "test/screen-low.jpg"))
		   (call-with-output-file f0
			 (cut write-as im 'jpeg <> :quality 95))
		   (call-with-output-file f1
			 (cut write-as im 'jpeg <> :quality 0))
		   (< (file-size f1) (file-size f0))))
  )
(let ((f0 "test/screen-compressed0.gd2")
	  (f1 "test/screen-compressed1.gd2")
	  (f2 "test/screen-raw.gd2"))
  (test* "the GD2 compression option"
		 #t
		 (begin
		   (save-as im f0 'gd2) ; default to compress
		   (save-as im f1 'gd2 :compress #t :chunk-size 15)
		   (save-as im f2 'gd2 :compress #f)
		   (apply < (map file-size (list f0 f1 f2)))))
  (test* "read a GD2 image partly (gd-image-create-from-gd2-part)"
		 '(20 . 30)
		 (let ((im (gd-image-create-from-gd2-part f0 77 49 20 30)))
		   (cons (get-width im)
				 (get-height im))))
  (test* "read a GD2 image partly (read-gd-image)"
		 '(40 . 50)
		 (let ((im (call-with-input-file f2
 					 (cut read-gd-image <> 'gd2 :x 100 :y 120 :w 40 :h 50))))
		   (cons (get-width im)
				 (get-height im))))
  )

(test-section "gdFont")
(define *font-giant* (gd-font-get-giant))
(test* "gd-font-get-giant" #t (or (is-a? *font-giant* <gd-font>) (not *font-giant*)))
(define *font-large* (gd-font-get-large))
(test* "gd-font-get-large" #t (or (is-a? *font-large* <gd-font>) (not *font-large*)))
(define *font-medium-bold* (gd-font-get-medium-bold))
(test* "gd-font-get-medium-bold" #t (or (is-a? *font-medium-bold* <gd-font>) (not *font-medium-bold*)))
(define *font-small* (gd-font-get-small))
(test* "gd-font-get-small" #t (or (is-a? *font-small* <gd-font>) (not *font-small*)))
(define *font-tiny* (gd-font-get-tiny))
(test* "gd-font-get-tiny" #t (or (is-a? *font-tiny* <gd-font>) (not *font-tiny*)))

(test-section "gd-image-char / gd-image-char-up")
(let* ((im (gd-image-create 40 100))
	   (white (gd-image-color-allocate im #xff #xff #xff))
	   (black (gd-image-color-allocate im 0 0 0)))
  (define (A/a font y)
	(when font
	  (gd-image-char im font  0 y 65 black)
	  (gd-image-char im font 20 y 97 black)))
  (gd-image-filled-rectangle im 0 0 40 40 white)
  (A/a *font-giant* 0)
  (A/a *font-large* 20)
  (A/a *font-medium-bold* 40)
  (A/a *font-small* 60)
  (A/a *font-tiny* 80)
  (save-as im "test/a.gif"))

(test-section "gd-image-string / gd-image-string-up")
(let* ((im (gd-image-create 500 100))
	   (white (gd-image-color-allocate im #xff #xff #xff))
	   (black (gd-image-color-allocate im 0 0 0))
	   (red   (gd-image-color-allocate im #xff 0 0)))
  (gd-image-filled-rectangle im 0 0 500 100 white)
  (when *font-medium-bold*
	(gd-image-string im *font-medium-bold* 15 0 "Faith goes out through the window when beauty comes in at the door." black))
  (when *font-small*
	(gd-image-string-up im *font-small* 0 95 "George A. Moore" red))
  (save-as im "test/string.gif"))

(test-section "gd-image-string-ft")
(use srfi-1)          ;; for first, second, ...
(use gauche.sequence) ;; for for-each-with-index
(for-each-with-index
 (lambda (i ft)
   (let* ((im (gd-image-create 320 320))
		  (white (gd-image-color-allocate im #xff #xff #xff))
		  (black (gd-image-color-allocate im 0 0 0)))
	 (gd-image-filled-rectangle im 0 0 320 320 white)
	 (let ((path (car ft))
		   (pairs (cddr ft)))
	   (when (file-exists? path)
		 (receive (lower-left lower-right upper-right upper-left)
			 (gd-image-string-ft #f black path 40.0 0.0 30 100 "Gauche-gd")
		   (test* "gd-image-string-ft(lower-left) with #f"  (first  pairs) lower-left)
		   (test* "gd-image-string-ft(lower-right) with #f" (second pairs) lower-right)
		   (test* "gd-image-string-ft(upper-right) with #f" (third  pairs) upper-right)
		   (test* "gd-image-string-ft(upper-left) with #f"  (fourth pairs) upper-left)
		   )
		 (receive (lower-left lower-right upper-right upper-left)
			 (gd-image-string-ft im black path 40.0 0.0 30 100 "Gauche-gd")
		   (test* "gd-image-string-ft(lower-left)"  (first  pairs) lower-left)
		   (test* "gd-image-string-ft(lower-right)" (second pairs) lower-right)
		   (test* "gd-image-string-ft(upper-right)" (third  pairs) upper-right)
		   (test* "gd-image-string-ft(upper-left)"  (fourth pairs) upper-left)
		   )
		 (with-ft-font/fg/pt/angle
		  path black 48 0
		  (lambda ()
			(string! im 30 200 (cadr ft))
			))
		 (save-as im (format #f "test/ft~d.gif" i))))))
 '(("/usr/share/fonts/truetype/kochi/kochi-gothic.ttf"
	"東風"
	( 29 . 108)
	(273 . 108)
	(273 .  59)
	( 29 .  59))
   ("/usr/share/fonts/truetype/sazanami/sazanami-gothic.ttf"
	"さざなみ"
	( 29 . 108)
	(273 . 108)
	(273 .  58)
	( 29 .  58))))

(test-section "GIF Animation")
; See the explanation on gdImageGifAnimBegin* in http://www.boutell.com/gd/manual2.0.33.html
(define im (gd-image-create 100 100))
(define white (gd-image-color-allocate im 255 255 255))
(define black (gd-image-color-allocate im 0 0 0))
(define trans (gd-image-color-allocate im 1 1 1))
(gd-image-rectangle im 0 0 10 10 black)
(call-with-output-file "test/anim.gif"
  (lambda (oport)
	(gif-anim-with
	 im oport
	 (lambda ()
	   (gif-anim-add im oport 0 0 0 100 1)
	   (let ((im2 (gd-image-create 100 100)))
		 (gd-image-color-allocate im2 255 255 255)
		 (gd-image-palette-copy im2 im)
		 (gd-image-rectangle im2 0 0 15 15 black)
		 (gd-image-color-transparent im2 trans)
		 (gif-anim-add im2 oport 0 0 0 100 1 im)
		 (let ((im3 (gd-image-create 100 100)))
		   (gd-image-color-allocate im3 255 255 255)
		   (gd-image-palette-copy im3 im)
		   (gd-image-rectangle im3 0 0 15 20 black)
		   (gd-image-color-transparent im3 trans)
		   (gif-anim-add im3 oport 0 0 0 100 1 im2))))
	 :global-cm 1
	 :loop 3
	 )))
(test* "gif-anim-with" #t (is-a? (call-with-input-file "test/anim.gif" (cut read-gd-image <> 'gif)) <gd-image>))

(test-section "GD Features")
(format #t "*gd-features*: ~s\n" *gd-features*)
(test-section "GD Version")
(format #t "*gd-version*: ~s\n" *gd-version*)

(test-end)

;; Local variables:
;; mode: scheme
;; end:
