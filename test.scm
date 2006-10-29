;;; -*- coding: euc-jp -*-
;;;
;;; test graphics.gd
;;;

(use gauche.test)

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

(test-section "color")
(define color #x789abcde)
(test* "gd-true-color-get-alpha" #x78 (gd-true-color-get-alpha color))
(test* "gd-true-color-get-red"   #x9a (gd-true-color-get-red   color))
(test* "gd-true-color-get-green" #xbc (gd-true-color-get-green color))
(test* "gd-true-color-get-blue"  #xde (gd-true-color-get-blue  color))
(test* "gd-alpha-blend" 33440629 (gd-alpha-blend #x11ee3355 #x09ff4477))

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
(define im1 (gd-image-square-to-circle im0 300))
(save-as im1 (if (memq 'png *gd-features*)
				 "test/im1.png"
				 "test/im1.gif"))

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
(let* ((im (gd-image-create 320 320))
	   (white (gd-image-color-allocate im #xff #xff #xff))
	   (black (gd-image-color-allocate im 0 0 0)))
  (gd-image-filled-rectangle im 0 0 320 320 white)
  (let ((fontpath "/usr/share/fonts/truetype/kochi/kochi-gothic.ttf"))
	(when (file-exists? fontpath)
	  (receive (lower-left lower-right upper-right upper-left)
		  (gd-image-string-ft im black fontpath 40.0 0.0 100 100 (symbol->string (gauche-character-encoding)))
		(test* "gd-image-string-ft(lower-left)"  (cons  99 107) lower-left)
		(test* "gd-image-string-ft(lower-right)" (cons 262 107) lower-right)
		(test* "gd-image-string-ft(upper-right)" (cons 262  60) upper-right)
		(test* "gd-image-string-ft(upper-left)"  (cons  99  60) upper-left))))
  (save-as im "test/ft.gif"))

(test-section "GD Features")
(format #t "*gd-features*: ~s\n" *gd-features*)

(test-end)

;; Local variables:
;; mode: scheme
;; end:
