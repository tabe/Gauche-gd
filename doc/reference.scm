#!/usr/bin/env gosh
;; -*- coding: euc-jp -*-

(use text.html-lite)
(use text.tree)

(define *version* "0.1.0")
(define *last-update* "Wed Oct 25 2006")

(define-syntax def
  (syntax-rules (en ja procedure method)
	((_ lang)
	 '())
	((_ en ((type name ...) (p ...) (q ...)) rest ...)
	 (def en ((type name ...) (p ...)) rest ...))
	((_ ja ((type name ...) (p ...) (q ...)) rest ...)
	 (def ja ((type name ...) (q ...)) rest ...))
	((_ lang ((procedure (name arg ...) ...) (p ...)) rest ...)
	 (list*
	  (html:h3 (html:span :class "type" "procedure") ": "
			   (html:span :class "procedure" (html-escape-string (symbol->string 'name))) " "
			   (cons (html:span :class "argument" 'arg) " ") ...)
	  ...
	  (html:p (html-escape-string p))
	  ...
	  (html:hr)
	  (def lang rest ...)))
	((_ lang ((method (name arg ...) ...) (p ...)) rest ...)
	 (list*
	  (html:h3 (html:span :class "type" "procedure") ": "
			   (html:span :class "procedure" (html-escape-string (symbol->string 'name))) " "
			   (cons (html:span :class "argument" (html-escape-string (x->string 'arg))) " ") ...)
	  ...
	  (html:p (html-escape-string p))
	  ...
	  (html:hr)
	  (def lang rest ...)))
	((_ lang ((type name ...) (p ...)) rest ...)
	 (list*
	  (html:h3 (html:span :class "type" 'type) ": "
			   (html:span :class 'type (html-escape-string (symbol->string 'name))))
	  ...
	  (html:p (html-escape-string p))
	  ...
	  (html:hr)
	  (def lang rest ...)))))

(define-macro (c-layer-api lang)
  `(def ,lang

	   ((class <gd-image>)
		("A fundamental class. Its instance has a foreign pointer to GD's \"gdImage\". It is expected as the first argument of most of procedures whose names starting with prefix `gd-image-'.")
		("���ܤȤʤ�����Υ��饹�Ǥ����ºݤˤ� GD �� \"gdImage\" �ؤ� foreign pointer �Ǥ���\"gd-image-\" (C �Ǥ� \"gdImage\")�ǻϤޤ�ۤȤ�ɤμ�³������1�����Ȥ����Ѥ����ޤ���"))

	   ((class <gd-font>)
		("Another fundamental class. Its instance has a foreign pointer to GD's \"gdFont\"."
		 "(Under developing)")
		("���ܤȤʤ�ե���ȤΥ��饹�Ǥ����ºݤˤ� GD �� \"gdFont\" �ؤ� foreign pointer �Ǥ���"
		 "(��ȯ��)"))

	   ((variable gdMaxColors gdAlphaMax gdAlphaOpaque gdAlphaTransparent gdRedMax gdGreenMax gdBlueMax
				  gdFTEX_LINESPACE gdFTEX_CHARMAP gdFTEX_RESOLUTION gdFTEX_DISABLE_KERNING
				  gdFTEX_XSHOW gdFTEX_FONTPATHNAME gdFTEX_FONTCONFIG gdFTEX_RETURNFONTPATHNAME
				  gdFTEX_Unicode gdFTEX_Shift_JIS gdFTEX_Big5
				  gdArc gdPie gdChord gdNoFill gdEdged
				  GD2_CHUNKSIZE GD2_CHUNKSIZE_MIN GD2_CHUNKSIZE_MAX
				  GD2_VERS
				  GD2_FMT_RAW GD2_FMT_COMPRESSED
				  GD_CMP_IMAGE GD_CMP_NUM_COLORS GD_CMP_COLOR GD_CMP_SIZE_X GD_CMP_SIZE_Y
				  GD_CMP_TRANSPARENT GD_CMP_BACKGROUND GD_CMP_INTERLACE GD_CMP_TRUECOLOR
				  GD_RESOLUTION)
		("Each of these is an integer corresponding to its C-alternative.")
		("���줾�� C ��Ʊ̾�������Ʊ�����ͤ��ͤ˻����ޤ���"))

	   ((variable GD2_ID)
		("It has a string value same with its C-equivalent.")
		("C ��Ʊ̾�������Ʊ��ʸ������ͤ˻����ޤ���"))

	   ((procedure (gd-true-color-get-alpha c)
				   (gd-true-color-get-red c)
				   (gd-true-color-get-green c)
				   (gd-true-color-get-blue c))
		("Each of these is equivalent to gdTrueColorGetAlpha, gdTrueColorGetRed, gdTrueColorGetGreen or gdTrueColorGetBlue respectively.")
		("���줾�� gdTrueColorGetAlpha, gdTrueColorGetRed, gdTrueColorGetGreen, gdTrueColorGetBlue ���б����ޤ���"))

	   ((procedure (gd-alpha-blend dest src))
		("This is equivalent to gdAlphaBlend.")
		("gdAlphaBlend ���б����ޤ���"))

	   ((procedure (gd-image-create sx sy)
				   (gd-image-create-palette sx sy)
				   (gd-image-create-true-color sx sy))
		("Each of these is a constructor of <gd-image> which is equivalent to gdImageCreate, gdImageCreatePalette or gdImageCreateTrueColor respectively.")
		("���줾�� gdImageCreate, gdImageCreatePalette, gdImageCreateTrueColor ���б�����<gd-image> ���֥������Ȥ��֤��ޤ���"))

	   ((procedure (gd-image-create-from-png path)
				   (gd-image-create-from-gif path)
				   (gd-image-create-from-jpeg path)
				   (gd-image-create-from-wbmp path)
				   (gd-image-create-from-gd path)
				   (gd-image-create-from-gd2 path))
		("Like C's gdImageCreateFrom* family, one of these creates a <gd-image> object from a source file in its particular image format. Unlike its C's equivalents, it treats the string-value single argument as a path of source. The file handle has been closed before successful return. In case of failure #f is returned.")
		("C �� gdImageCreateFrom* �ؿ���Ʊ�ͤˡ������μ�³���Ϥ��줾������β����ե����ޥåȤ˱����ƥե����뤫�� <gd-image> ���֥������Ȥ�������ޤ������� C ��Ʊ���δؿ��Ȱۤʤꡢ�����˥������ե�����Υѥ���ʸ����ǻ��ꤷ�ޤ�����³��������������ä����ˤϥ������ե�����Υϥ�ɥ���Ĥ����Ƥ��ޤ������Ԥ������ˤ� #f ���֤��ޤ���"))

	   ((procedure (gd-image-create-from-xpm path))
		("The gdImageCreateFromXpm equivalent, which returns a <gd-image> object in case of success, otherwise #f.")
		("C �� gdImageCreateFromXpm ���б������³���Ǥ��������������ˤ� <gd-image> ���֥������Ȥ򡢼��Ԥ������ˤ� #f ���֤��ޤ���"))

	   ((procedure (gd-image-destroy im))
		("Call gdImageDestroy() explicitly. Different from the C version it is idempotent, meaning that it can be repeated safely."
		 "(Because of gc, it is little necessary to use this procedure.)")
		("<gd-image> ���֥������Ȥλؤ��Ƥ��� \"gdImage\" �꥽�������Ф�������Ū�� gdImageDestroy ��ƤӤޤ��������֤��ƤФ�Ƥ�����Ǥ���"
		 "(����˴ؤ������̤����¤Τʤ��¤ꡢ�������ä��꥽�����β����� gc ��Ǥ���Ƥ����ƹ����ޤ���)"))

       ((procedure (gd-image-set-pixel im x y color)
				   (gd-image-get-pixel im x y)
				   (gd-image-get-true-color-pixel im x y))
        ("Each of these is equivalent to gdImageSetPixel, gdImageGetPixel or gdImageGetTrueColorPixel respectively.")
        ("���줾�� gdImageSetPixel, gdImageGetPixel, gdImageGetTrueColorPixel ���б����ޤ���"))

       ((procedure (gd-image-line im x1 y1 x2 y2 color))
        ("The gdImageLine alternative.")
        ("�ؿ� gdImageLine ���б������³���Ǥ���"))

       ((procedure (gd-image-rectangle im x1 y1 x2 y2 color)
				   (gd-image-filled-rectangle im x1 y1 x2 y2 color))
        ("Each of these is equivalent to gdImageRectangle or gdImageFilledRectangle respectively.")
        ("���줾�� gdImageRectangle, gdImageFilledRectangle ���б������³���Ǥ���"))

       ((procedure (gd-image-set-clip im x1 y1 x2 y2)
				   (gd-image-get-clip im))
        ("As well as gdImageSetClip the former one clips a rectangle from the image for subsequent drawing. The latter returns the current clip with four integer-values.")
        ("gdImageSetClip ��Ʊ�ͤ� gd-image-set-clip �ϰʸ�������Ŭ�Ѥ���륯��å��ΰ�����ꤷ�ޤ���gd-image-get-clip �ϸ��ߤΥ���å��ΰ�κ�ɸ��ɽ��4�Ĥο��ͤ��֤��ޤ���"))

	   ((procedure (gd-image-bounds-safe im x y))
		("Given the coordinates of a point, return either 1 (if it is in the current clip) or 0 (otherwise). You should use it in order to check whether the coordinates is suitable or not for arguments of another procedure, e.g. gd-image-fill.")
		("����å��ΰ���κ�ɸ����������줿����1�򡢤���ʤ���0���֤��ޤ���gd-image-fill �ʤɤμ�³���ΰ�����Ŭ������ɸ���ɤ������ǧ���뤿��ˤ��δؿ������Ѥ���٤��Ǥ���"))

	   ((procedure (gd-image-polygon im points pointsTotal color)
				   (gd-image-filled-polygon im points pointsTotal color)
				   (gd-image-open-polygon im points pointsTotal color))
		("The second argument of these procedure, which expresses a sequence of points, must satisfy the following conditions:"
		 "- of type proper <list>, and"
		 "- its elements are of type <pair> and consist of integers which correspond to the coordinates of a point."
		 "Furthermore the third one is expected to be a non-negative integer.")
		("�����μ�³���Ϥ��줾�� gdImagePolygon, gdImageFilledPolygon, gdImageOpenPolygon ���б����ޤ�������������������ɸ���ɽ����2�����Ȥ��ƽ۴Ĥ�̵���ꥹ�Ȥ���ꤷ�ޤ������Υꥹ�Ȥγ����ǤϺ�ɸ��ɽ���ڥ��Ǥʤ���Фʤ�ޤ��󡣤������3���������������Ǥ��뤳�Ȥ������ޤ���"))

	   ((procedure (gd-image-color-allocate im r g b)
				   (gd-image-color-allocate-alpha im r g b a)
				   (gd-image-color-closest im r g b)
				   (gd-image-color-closest-alpha im r g b a)
				   (gd-image-color-closest-hwb im r g b)
				   (gd-image-color-exact im r g b)
				   (gd-image-color-exact-alpha im r g b a)
				   (gd-image-color-resolve im r g b)
				   (gd-image-color-resolve-alpha im r g b a))
		("Each of these corresponds to gdImageColorAllocate, gdImageColorAllocateAlpha, gdImageColorClosest, gdImageColorClosestAlpha, gdImageColorClosestHWB, gdImageColorExact, gdImageColorExactAlpha, gdImageColorResolve or gdImageColorResolveAlpha respectively.")
		("���줾�� C �δؿ� gdImageColorAllocate, gdImageColorAllocateAlpha, gdImageColorClosest, gdImageColorClosestAlpha, gdImageColorClosestHWB, gdImageColorExact, gdImageColorExactAlpha, gdImageColorResolve, gdImageColorResolveAlpha ���б����ޤ���"))

	   ((procedure (gd-true-color r g b)
				   (gd-true-color-alpha r g b a))
		("Same as gdTrueColor or gdTrueColorAlpha resp. for each.")
		("���줾�� C �δؿ� gdTrueColor, gdTrueColorAlpha ���б����ޤ���"))

	   ((procedure (gd-image-color-deallocate im color))
		("Similar to gdImageColorDeallocate it reduces a color in the palette.")
		("gdImageColorDeallocate ��Ʊ�ͤ˥ѥ�åȤο�������˴��������ѤǤ���褦�ˤ��ޤ���"))

	   ((procedure (gd-image-create-palette-from-true-color im ditherFlag colorsWanted)
				   (gd-image-true-color-to-palette im ditherFlag colorsWanted))
		("Both convert a true color image into the palette one though the latter destructively returns into the result argument `im'.")
		("������� true color ���᡼����ѥ�åȥ��᡼�����Ѵ����ޤ�����������Ԥ�Ϳ����줿���᡼�����˲�Ū���ѹ������ΰ����˷�̤��֤��ޤ���"))

	   ((procedure (gd-image-color-transparent im color))
		("The gdImageColorTransparent equivalent.")
		("�ؿ� gdImageColorTransparent ���б����ޤ���"))

	   ((procedure (gd-image-palette-copy dst src))
		("The gdImagePaletteCopy alternative.")
		("�ؿ� gdImagePaletteCopy ���б����ޤ���"))

	   ((procedure (gd-image-filled-arc im cx cy w h s e color style)
				   (gd-image-arc im cx cy w h s e color))
		("Like gdImageFilledArc or gdImageArc respectively.")
		("���줾�� gdImageFilledArc, gdImageArc ���б����ޤ���"))

	   ((procedure (gd-image-filled-ellipse im cx cy w h color))
		("The gdImageFilledEllipse equivalent.")
		("�ؿ� gdImageFilledEllipse ���б����ޤ���"))

	   ((procedure (gd-image-fill-to-border im x y border color)
				   (gd-image-fill im x y color))
		("Each of these is equivalent to gdImageFillToBorder or gdImageFill respectively.")
		("���줾�� gdImageFillToBorder, gdImageFill ���б����ޤ���"))

	   ((procedure (gd-image-copy dst src dstX dstY srcX srcY w h)
				   (gd-image-copy-merge dst src dstX dstY srcX srcY w h pct)
				   (gd-image-copy-merge-gray dst src dstX dstY srcX srcY w h pct)
				   (gd-image-copy-resized dst src dstX dstY srcX srcY dstW dstH srcW srdH)
				   (gd-image-copy-resampled dst src dstX dstY srcX srcY dstW dstH srcW srdH)
				   (gd-image-copy-rotated dst src dstX dstY srcX srcY srcWidth srdHeight angle))
		("Note that some of these will preserve the palette of the given destination image as well as their C-equivalents.")
		("�����μ�³������ˤ��б����� gdImageCopy* �ؿ���Ʊ���褦��Ϳ����줿���ԡ���Υѥ�åȤ��ѹ����ʤ���Τ⤢�뤳�Ȥ���դ��Ƥ���������"))

	   ((procedure (gd-image-set-brush im brush)
				   (gd-image-set-tile im tile)
				   (gd-image-set-anti-aliased im c)
				   (gd-image-set-anti-aliased-dont-blend im c dont_blend)
				   (gd-image-set-thickness im thickness)
				   (gd-image-interlace im interlaceArg)
				   (gd-image-alpha-blending im blending)
				   (gd-image-save-alpha im saveFlag))
		("The following functions are called respectively: gdImageSetBrush, gdImageSetTile, gdImageSetAntiAliased, gdImageSetAntiAliasedDontBlend, gdImageSetThickness, gdImageInterlace, gdImageAlphaBlending and gdImageSaveAlpha.")
		("���줾�� gdImageSetBrush, gdImageSetTile, gdImageSetAntiAliased, gdImageSetAntiAliasedDontBlend, gdImageSetThickness, gdImageInterlace, gdImageAlphaBlending, gdImageSaveAlpha ���б����ޤ���"))

		((procedure (gd-image-set-style im style styleLength))
		 ("")
		 (""))

 	   ((procedure (gd-image-true-color im))
		("It returns 0 for a palette image, otherwise non-0.")
		("�ѥ�åȥ��᡼����Ϳ����줿����0�򡢤���ʤ���0�ʳ��ο��ͤ��֤��ޤ���"))

	   ((procedure (gd-image-sx im)
				   (gd-image-sy im))
		("Each returns the sx(`width') or sy(`height') of the image respectively.")
		("���줾��Ϳ����줿���᡼���β��⤷���ϽĤ������֤��ޤ���"))

	   ((procedure (gd-image-colors-total im))
		("Given a palette image it returns the number of currently allocated colors in the palette.")
		("�ѥ�åȥ��᡼����Ϳ����줿��硢�ѥ�åȤο������֤��ޤ���"))

	   ((procedure (gd-image-red im c)
				   (gd-image-green im c)
				   (gd-image-blue im c)
				   (gd-image-alpha im c))
		("Each of these corresponds to gdImageRed, gdImageGreen, gdImageBlue or gdImageAlpha respectively.")
		("���줾�� gdImageRed, gdImageGreen, gdImageBlue, gdImageAlpha ���б����ޤ���"))

	   ((procedure (gd-image-get-transparent im)
				   (gd-image-get-interlaced im))
		("Each of these is equivalent to gdImageGetTransparent or gdImageGetInterlaced respectively.")
		("���줾�� gdImageGetTransparent, gdImageGetInterlaced ���б����ޤ���"))

	   ((procedure (gd-image-palette-pixel im x y)
				   (gd-image-true-color-pixel im x y))
		("These inherits the bounds-unsafe nature from their C-alternatives.")
		("�����μ�³���ϥޥ��� gdImagePalettePixel �� gdImageTrueColorPixel ��ľ�ܸƤӽФ����ᡢ������Ŭ�ڤ��ɤ��������˳�ǧ����ɬ�פ�����Ǥ��礦��"))

	   ((procedure (gd-image-compare im1 im2))
		("Return 0 If two images are same wrt displayed components, otherwise non-0.")
		("Ϳ����줿2�ĤΥ��᡼�������褵��빽�����Ǥ˴ؤ������������0�򡢤���ʤ���0�ʳ��ο��ͤ��֤��ޤ���"))

	   ((procedure (gd-image-square-to-circle im radius)
				   (gd-image-sharpen im pct))
		("Like gdImageSquareToCircle or gdImageSharpen respectively.")
		("���줾�� gdImageSquareToCircle, gdImageSharpen ���б����ޤ���"))
	   ))

(define-macro (simple-api lang)
  `(def ,lang
		((constant *gd-features*)
		 ("A list of symbols which mean available features of GD."
		  "Possible symbols: fontconfig freetype gif jpeg png xpm.")
		 ("ͭ���� GD �ε�ǽ��ɽ������ܥ�Υꥹ�ȡ�"
		  "�ޤޤ���ǽ���Τ��륷��ܥ�ϰʲ����̤�: fontconfig freetype gif jpeg png xpm��"))
		((method (save-as (im <gd-image>) (path <string>) &optional (fmt <symbol>)))
		 ("It provides the (currently only) way to output a image. It tries to create a file of path `path' even if exists and return 0 in case of success. Unless the optional `fmt' is given it choices the output image format by the extension (such as \"gif\", \"jpe\", \"jpeg\", \"jpg\", \"png\", \"gd\" and \"gd2\") of the path."
		  "Available formats (if supported): gif, jpeg, png, gd and gd2.")
		 ("���᡼������Ϥ���(�������Ǥ�ͣ���)�᥽�åɤǤ���`path' �Ȥ���Ϳ����줿�ե������(��¸�Ǥ��äƤ�)�������������ޤ���������������0���֤��ޤ������ץ���ʥ�ʰ��� `fmt' ������Ū�˥��᡼���ե����ޥåȤ���ꤷ�ʤ���� `path' �γ�ĥ�Ҥˤ�ä����򤵤�ޤ���Ƚ�̤�����ĥ�Ҥ� \"gif\", \"jpg\", \"jpeg\", \"jpe\", \"png\", \"gd\", \"gd2\" �Ǥ���"
		  "(���ݡ��Ȥ��Ƥ����)���ѤǤ���ե����ޥåȤ� gif, jpeg, png, gd, gd2 �Ǥ���"))
		))

(define (document-tree lang)
  (let ((title (if (eq? 'ja lang) "Gauche-gd ��ե���󥹥ޥ˥奢��" "Gauche-gd Reference Manual")))
	(html:html
	 (html:head
	  (if (eq? 'ja lang) (html:meta :http-equiv "Content-Type" :content "text/html; charset=EUC-JP") '())
	  (html:title title))
	 (html:body
	  (html:h1 title)
	  (html:style
	   :type "text/css"
	   "<!-- \n"
	   "h2 { background-color:#dddddd; }\n"
	   "address { text-align: right; }\n"
	   ".type { font-size: medium; text-decoration: underline; }\n"
	   ".procedure { font-size: medium; font-weight: normal; }\n"
	   ".method { font-size: medium; font-weight: normal; }\n"
	   ".argument { font-size: small; font-style: oblique; font-weight: normal; }\n"
	   ".constant { font-size: medium; font-weight: normal; }\n"
	   ".variable { font-size: medium; font-weight: normal; }\n"
	   "#last_update { text-align: right; font-size: small; }\n"
	   " -->")
	  (html:p "For version " *version*)
	  (html:p :id "last_update" "last update: " *last-update*)
	  (html:h2 "C Layer API")
	  (if (eq? 'en lang)
		  (c-layer-api en)
		  (c-layer-api ja))
	  (html:h2 "Simple API")
	  (if (eq? 'en lang)
		  (html:p "(Under developing. See graphics/gd.scm and example/*.scm if interested.)")
		  (html:p "(��ȯ�档��̣�Τ������� graphics/gd.scm �� example/*.scm �򸫤Ƥ���������)"))
	  (if (eq? 'en lang)
		  (simple-api en)
		  (simple-api ja))
	  (html:address "(c) 2006 Takeshi Abe")
	  ))))

(define (main args)
  (define (usage)
	(format (current-error-port) "usage: gosh reference.scm (en|ja)\n")
	(exit 1))
  (when (< (length args) 2)
	(usage))
  (write-tree (document-tree (string->symbol (cadr args))))
  0)
