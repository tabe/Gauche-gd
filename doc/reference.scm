#!/usr/bin/env gosh
;; -*- coding: euc-jp -*-

(use text.html-lite)
(use text.tree)

(define *version* "0.3.1")
(define *last-update* "Thu Mar 24 2011")

(define-syntax def
  (syntax-rules (en ja procedure method)
	((_ en)
     '())
    ((_ ja)
	 '())
	((_ en (synopsis x y z ...) rest ...)
     (cons
      (def (synopsis x z ...))
      (def en rest ...)))
	((_ ja (synopsis x y z ...) rest ...)
     (cons
      (def (synopsis y z ...))
      (def ja rest ...)))
	((_ ((procedure (name arg ...) ...) (p ...) z ...))
     (list
      (html:h3 (html:span :class "type" "procedure") ": "
               (html:span :class "procedure" (html-escape-string (symbol->string 'name))) " "
               (cons (html:span :class "argument" 'arg) " ") ...)
      ...
      (map
       (lambda (x)
         (if (string? x)
             (html:p (html-escape-string x))
             (html:pre (html-escape-string (list-ref '(z ...) x)))))
       (list p ...))
      (html:hr)))
	((_ ((method (name arg ...) ...) (p ...) z ...))
	 (list
	  (html:h3 (html:span :class "type" "method") ": "
			   (html:span :class "method" (html-escape-string (symbol->string 'name))) " "
			   (cons (html:span :class "argument" (html-escape-string (x->string 'arg))) " ") ...)
	  ...
      (map
       (lambda (x)
         (if (string? x)
             (html:p (html-escape-string x))
             (html:pre (html-escape-string (list-ref '(z ...) x)))))
       (list p ...))
	  (html:hr)))
	((_ ((type name ...) (p ...) z ...))
	 (list
	  (html:h3 (html:span :class "type" 'type) ": "
			   (html:span :class 'type (html-escape-string (symbol->string 'name))))
	  ...
      (map
       (lambda (x)
         (if (string? x)
             (html:p (html-escape-string x))
             (html:pre (html-escape-string (list-ref '(z ...) x)))))
       (list p ...))
	  (html:hr)))))

(define-macro (c-layer-api lang)
  `(def ,lang

	   ((class <gd-image>)
		("A fundamental class. Its instance has a foreign pointer to GD's \"gdImage\". It is expected as the first argument of most of procedures whose names starting with prefix `gd-image-'.")
		("���ܤȤʤ륤�᡼���Υ��饹�Ǥ����ºݤˤ� GD �� \"gdImage\" �ؤ� foreign pointer �Ǥ���\"gd-image-\" (C �Ǥ� \"gdImage\")�ǻϤޤ�ۤȤ�ɤμ�³������1�����Ȥ����Ѥ����ޤ���"))

	   ((class <gd-font>)
		("Another fundamental class. Its instance has a foreign pointer to GD's \"gdFont\". You can get its instance by any of \"gd-font-get-*\".")
		("���ܤȤʤ�ե���ȤΥ��饹�Ǥ����ºݤˤ� GD �� \"gdFont\" �ؤ� foreign pointer �Ǥ�����³�� \"gd-font-get-*\" �Τ����줫�ǥ��󥹥��󥹤����뤳�Ȥ��Ǥ��ޤ���"))

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
		("Each of these is equivalent to gdTrueColorGetAlpha, gdTrueColorGetRed, gdTrueColorGetGreen, or gdTrueColorGetBlue respectively.")
		("���줾�� gdTrueColorGetAlpha, gdTrueColorGetRed, gdTrueColorGetGreen, gdTrueColorGetBlue ���б����ޤ���"))

	   ((procedure (gd-alpha-blend dest src))
		("This is equivalent to gdAlphaBlend.")
		("gdAlphaBlend ���б����ޤ���"))

	   ((procedure (gd-image-create sx sy)
				   (gd-image-create-palette sx sy)
				   (gd-image-create-true-color sx sy))
		("Each of these is a constructor of <gd-image> which is equivalent to gdImageCreate, gdImageCreatePalette, or gdImageCreateTrueColor respectively. If failed these return #f.")
		("���줾�� gdImageCreate, gdImageCreatePalette, gdImageCreateTrueColor ���б�����<gd-image> ���֥������Ȥ��֤��ޤ������Ԥ������� #f ���֤��ޤ���"))

	   ((procedure (gd-image-create-from-png path)
				   (gd-image-create-from-gif path)
				   (gd-image-create-from-jpeg path)
				   (gd-image-create-from-wbmp path)
				   (gd-image-create-from-xbm path)
				   (gd-image-create-from-gd path)
				   (gd-image-create-from-gd2 path)
				   (gd-image-create-from-gd2-part path))
		("Like C's gdImageCreateFrom* family, one of these creates a <gd-image> object from a source file in its particular image format. Unlike its C's equivalents, it treats the string-value single argument as a path of source. The file handle has been closed before successful return. In case of failure #f is returned.")
		("C �� gdImageCreateFrom* �ؿ���Ʊ�ͤˡ������μ�³���Ϥ��줾������Υ��᡼���ե����ޥåȤ˱����ƥե����뤫�� <gd-image> ���֥������Ȥ�������ޤ������� C ��Ʊ���δؿ��Ȱۤʤꡢ�����˥������ե�����Υѥ���ʸ����ǻ��ꤷ�ޤ�����³��������������ä����ˤϥ������ե�����Υϥ�ɥ���Ĥ����Ƥ��ޤ������Ԥ������ˤ� #f ���֤��ޤ���"))

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
        ("Each of these is equivalent to gdImageSetPixel, gdImageGetPixel, or gdImageGetTrueColorPixel respectively.")
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

	   ((procedure (gd-image-char im f x y c color)
				   (gd-image-char-up im f x y c color))
		("Put a single byte character on the given image with the font provided by a gd-font-get-*. If you would like to print a string containing multibyte characters, try procedure \"gd-image-string-ft\" or method \"string!\".")
		("���᡼����1�Х���ʸ������Ϥ��ޤ�����2�����Υե���Ȥϼ�³�� gd-font-get-* ����������Ƥ����������ޥ���Х���ʸ����ޤ�ʸ�������Ϥ�����ˤϼ�³�� \"gd-image-string-ft\" �ޤ��ϥ᥽�å� \"string!\" ��ȤäƤ���������"))

	   ((procedure (gd-image-string im f x y s color)
				   (gd-image-string-up im f x y s color))
		("Put a string consising of single byte characters, with the font provided by a gd-font-get-*. If you would like to print a string containing multibyte characters, try gd-image-string-ft.")
		("���᡼����1�Х���ʸ������ʤ�ʸ�������Ϥ��ޤ�����2�����Υե���Ȥϼ�³�� gd-font-get-* ����������Ƥ����������ޥ���Х���ʸ����ޤ�ʸ�������Ϥ�����ˤϼ�³�� \"gd-image-string-ft\" �ޤ��ϥ᥽�å� \"string!\" ��ȤäƤ���������"))

	   ((procedure (gd-image-string-ft im fg fontlist ptsize angle x y str))
		("Print a string with a FreeType font specified by the path `fontlist'. Unlike the original version, it return *four* pairs of integers which represent the coordinates of the points surrounding the bounding rectangle, and coming lower-left, lower-right, upper-right, and upper-left in that order. "
		 "It is also possible to obtain these values efficiently, i.e. without printing `str', by giving #f to `im'."
		 "If your gosh is configured with option \"--enable-multibyte=utf-8\", then congratulations! and multibyte characters will be available in `str' (with an appropriate font, of course). Otherwise you had better use method \"string!\".")
		("`fontlist' �ǥѥ�����ꤹ�뤳�Ȥ� FreeType �ե���Ȥ��Ѥ���ʸ�����񤭽Ф��ޤ������ꥸ�ʥ�� C �δؿ��Ȱۤʤꡢ���μ�³����*4��*�Υڥ����֤������줾�줬������ζ����Ϥ��ɸ(��˺��������������塢����)��ɽ���ޤ���"
		 "`im' �� #f ���Ϥ����Ȥ����褻���ˤ��ζ���κ�ɸ���ΨŪ�˵��뤳�Ȥ��Ǥ��ޤ���"
		 "gosh �� \"--enable-multibyte=utf-8\" �Ȥ������ץ�����դ��ǥӥ�ɤ���Ƥ���� `str' �ǥޥ���Х���ʸ�������ѤǤ��ޤ��������Ǥʤ����ϥ᥽�å� \"string!\" �λ��Ѥ�ͤ��Ƥ���������"))

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
		("Each of these corresponds to gdImageColorAllocate, gdImageColorAllocateAlpha, gdImageColorClosest, gdImageColorClosestAlpha, gdImageColorClosestHWB, gdImageColorExact, gdImageColorExactAlpha, gdImageColorResolve, or gdImageColorResolveAlpha respectively.")
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
		("Both convert a true color image into the palette one while the latter destructively returns into the result argument `im'. If the former fails it returns #f")
		("������� true color ���᡼����ѥ�åȥ��᡼�����Ѵ����ޤ�����������Ԥ�Ϳ����줿���᡼�����˲�Ū���ѹ������ΰ����˷�̤��֤��ޤ����ޤ����Ԥ����Ԥ������ #f ���֤�ޤ���"))

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
		("The following functions are called respectively: gdImageSetBrush, gdImageSetTile, gdImageSetAntiAliased, gdImageSetAntiAliasedDontBlend, gdImageSetThickness, gdImageInterlace, gdImageAlphaBlending, and gdImageSaveAlpha.")
		("���줾�� gdImageSetBrush, gdImageSetTile, gdImageSetAntiAliased, gdImageSetAntiAliasedDontBlend, gdImageSetThickness, gdImageInterlace, gdImageAlphaBlending, gdImageSaveAlpha ���б����ޤ���"))

		((procedure (gd-image-set-style im style styleLength))
		 ("Given style as a list, call gdImageSetStyle.")
		 ("style ��ꥹ�ȤȤ���Ϳ���뤳�Ȥ� gdImageSetStyle ��ƤӤޤ���"))

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
		("Each of these corresponds to gdImageRed, gdImageGreen, gdImageBlue, or gdImageAlpha respectively.")
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
		("Like gdImageSquareToCircle or gdImageSharpen respectively. The former returns #f in case of failure.")
		("���줾�� gdImageSquareToCircle, gdImageSharpen ���б����ޤ������Ԥϼ��Ԥ������ #f ���֤��ޤ���"))

	   ((procedure (gd-font-get-giant)
				   (gd-font-get-large)
				   (gd-font-get-medium-bold)
				   (gd-font-get-small)
				   (gd-font-get-tiny))
		("One of these procedures give you the font of size Giant, Large, MediumBold, Small, or Tiny respectively, which is for gd-image-char, gd-image-string etc. When the specified size is unavailable it return #f.")
		("�����μ�³���Ϥ��줾�쥵������ Giant, Large, MediumBold, Small, Tiny �Υե���Ȥ��֤��ޤ������Υե���Ȥ� gd-image-char �� gd-image-string ���ΰ��������Ѥ���ޤ����ե���Ȥ������ʤ����� #f ���֤��ޤ���"))
	   ))

(define-macro (simple-api lang)
  `(def ,lang
		((constant *gd-version*)
		 ("The version of GD library. Detected at the compile time of the package.")
		 ("GD �ΥС������Gauche-gd �Υ���ѥ�����˼���������ΤǤ���"))

		((constant *gd-features*)
		 ("A list of symbols which mean available features of GD."
		  "Possible symbols in the list: fontconfig freetype gif jpeg png xpm."
          "You can also use feature identifiers of form 'gauche.ext.graphics.gd.*' with cond-expand."
          "Note that they are available after loading the module.")
		 ("ͭ���� GD �ε�ǽ��ɽ������ܥ�Υꥹ�ȡ�"
		  "���Υꥹ�Ȥ˴ޤޤ���ǽ���Τ��륷��ܥ�ϰʲ����̤�: fontconfig freetype gif jpeg png xpm��"
          "�ޤ� 'gauche.ext.graphics.gd.*' �Ȥ������� feature indentifiers �� cond-expand �ȤȤ�˻Ȥ����Ȥ�Ǥ��ޤ���"
          "������ identifiers �ϥ⥸�塼��Υ��ɸ�����ѤǤ��ޤ���"))

		((parameter current-gd-image-format)
		 ("It is expected to keep a symbol which decides the image format taken with \"read-gd-image\" and/or \"write\" unless specified.")
		 ("GD �Υ��᡼������������Υե����ޥåȤ˰�¸���ʤ�������ݲ�����Ƥ��ޤ����������Ϥ�Ԥ��ݤ�����Υե����ޥåȤ�����ˤʤäƤ��뤳�Ȥ��褯����ޤ���\"read-gd-image\" �� \"write\" �� ���Υѥ�᡼�����ͤ򻲾Ȥ���Τǡ��������ä��������ѤǤ��ޤ���"))

		((procedure (with-gd-image-format fmt thunk))
		 ("Call `thunk' with the parameterized \"current-gd-image-format\" of value `fmt'.")
		 ("�嵭�� \"current-gd-image-format\" ���ͤ���Ū�� `fmt' �ˤ��� `thunk' ��ƤӤޤ������� \"current-gd-image-format\" ���ͤ���������ޤ���`thunk' ������ͤ����μ�³��������ͤǤ���"))

		((method (read-gd-image (port <port>) &optional (fmt <symbol>) &keyword x y w h))
		 ("Read a image with or without format `fmt' (such as \"gif\", \"jpe\", \"jpeg\", \"jpg\", \"png\", \"wbmp\", \"gd\", and \"gd2\"). See also \"current-gd-image-format\", or #f if an error occurs."
		  "The keywords `x', `y', `w' and `h' are only for the GD2 format and corresponding to the 2nd, 3rd, 4th and 5th argument of \"gdImageCreateFromGd2PartCtx\" respectively.")
		 ("�ݡ��� `port' ���饤�᡼�������Ϥ��ޤ������顼�ξ��� #f ���֤��ޤ����ե����ޥå� `fmt' �Ȥ��Ƽ�������ͤ� \"gif\", \"jpe\", \"jpeg\", \"jpg\", \"png\", \"wbmp\", \"gd\", �ڤ� \"gd2\" �Ǥ���\"current-gd-image-format\" �⻲�Ȥ��Ƥ���������"
		  "������� `x'��`y'��`w' �ڤ� `h' �� GD2 �ե����ޥåȤ��Ф��ƤΤ�Ŭ�Ѥ��졢���줾�� \"gdImageCreateFromGd2PartCtx\" �� 2���ܡ�3���ܡ�4���ܵڤ�5���ܤΰ������б����ޤ���"))

		((method (write-as (im <gd-image>) (fmt <symbol>) port &keyword quality foreground))
		 ("Put a image `im' with format `fmt' (such as \"gif\", \"jpe\", \"jpeg\", \"jpg\", \"png\", and \"wbmp\")."
		  "If the JPEG format specified, you can suggest the `quality' of the resulting image. The keyword is just ignored otherwise."
		  "If the WBMP format specified, you can set the color index of pixels which are considered as `foreground'. The keyword is just ignored otherwise.")
		 ("�ݡ��Ȥإ��᡼�� `im' ����Ϥ��ޤ����ե����ޥå� `fmt' �˼�������ͤ� \"gif\", \"jpe\", \"jpeg\", \"jpg\", \"png\", �ڤ� \"wbmp\" �Ǥ���"
		  "JPEG �ե����ޥåȤ����ꤵ�줿���ˤϡ�������� `quality' ��Ϳ�������ͤˤ�äƽ��Ϥμ�(�ȡ����Ū�˥�����)��Ĵ��Ǥ��ޤ������Υ�����ɤ�Ϳ�����ʤ����ˤ� GD �ˤ�äƥǥե���Ȥ��ͤ����Ѥ���ޤ�������ʳ��Υե����ޥåȤˤĤ��Ƥϡ�ñ��̵�뤵��ޤ���"
		  "WBMP �ե����ޥåȤ����ꤵ�줿���ˤϡ�������� `foreground' �ˤ�äƥӥåȤ����åȤ���륫�顼����ǥå�����Ϳ���뤳�Ȥ��Ǥ��ޤ������Υ���ǥå���������ʤ��ԥ������\"�ط�\"�Ȥ��ư����ޤ������Υ�����ɤ�Ϳ�����ʤ����ˤ� GD �ϥǥե���Ȥο����񤤤򤷤ޤ���"))

		((method (display (im <gd-image>) &optional port)
				 (write (im <gd-image>) &optional port))
		 ("They are the same abbreviations of \"write-as\". See also \"current-gd-image-format\".")
		 ("\"write-as\" �ξ�ά���ǡ��������Ʊ���褦�˿��񤤤ޤ���\"current-gd-image-format\" �⻲�Ȥ��Ƥ���������"))

		((method (save-as (im <gd-image>) (path <string>))
				 (save-as (im <gd-image>) (path <string>) (fmt <symbol>) &keyword quality foreground chunk-size compress))
		 ("It provides another way to output a image. It tries to create a file of path `path' even if exists and return 0 in case of success. In the former case it choices the output image format by the extension (such as \"gif\", \"jpe\", \"jpeg\", \"jpg\", \"png\", \"wbmp\", \"gd\", and \"gd2\") of the path. Explicit `fmt' is used in the latter."
		  "Available formats (if supported): GIF, JPEG, PNG, WBMP, GD, AND GD2."
		  "If the GD2 format specified, you can set the options of the resulting image by the keywords `chunk-size' and `compress'. If a true value following `compress' (default), then the image will be compressed with the given `chunk-size' (or, a default one without `chunk-size').  Otherwise it will be uncompressed."
		  "See \"write-as\" on the keyword `quality' for JPEG and `foreground' for WBMP.")
		 ("���᡼������Ϥ���᥽�åɤ�1�ĤǤ���`path' �Ȥ���Ϳ����줿�ե������(��¸�Ǥ��äƤ�)�������������ޤ���������������0���֤��ޤ������Ԥη��ǸƤФ줿��硢���ϥ��᡼���ե����ޥåȤ� `path' �γ�ĥ�Ҥˤ�ä����򤵤�ޤ���Ƚ�̤�����ĥ�Ҥ� \"gif\", \"jpg\", \"jpeg\", \"jpe\", \"png\", \"wbmp\", \"gd\", \"gd2\" �Ǥ�����Ԥη�������Ū�˥ե����ޥå� `fmt' ��Ϳ�����ޤ���"
		  "(���ݡ��Ȥ��Ƥ����)���ѤǤ���ե����ޥåȤ� GIF, JPEG, PNG, WBMP, GD, GD2 �Ǥ���"
		  "GD2 �ե����ޥåȤ����ꤵ�줿��硢������� `chunk-size' �ڤ� `compress' �ˤ�äƥ��ץ��������Ǥ��ޤ���`compress' ���Ф��ƿ����ͤ�Ϳ����줿�Ȥ�(�ǥե����)�ˤϡ��񤭽Ф���륤�᡼���� `chunk-size' ���ͤ�Ȥäư��̤���ޤ���`chunk-size' ��Ϳ�����ʤ���硢GD �ϥǥե���ȤΥ����������򤷤ޤ���`compress' �����ʤ鰵�̤���ޤ���"
		  "JPEG �ե����ޥåȤΤ���Υ������ `quality' �� WBMP �ե����ޥåȤΤ���Υ������ `foreground' �ˤĤ��Ƥϡ�\"write-as\" �򻲾Ȥ��Ƥ���������"))

		((method (char! (im <gd-image>) (f <gd-font>) (x <integer>) (y <integer>) (c <integer>) (color <integer>) &keyword direction))
		 ("Put a character on the given `im'. If symbol 'up follows keyword `direction', \"gd-image-char-up\" is called instead of \"gd-image-char\".")
		 ("���᡼����ʸ����񤭽Ф��ޤ���������� `direction' ��³���ƥ���ܥ� 'up �����ꤵ�줿��硢\"gd-image-char\" ������� \"gd-image-char-up\" ���ƤФ�ޤ���"))

		((method (string! (im <gd-image>) (f <gd-font>) (x <integer>) (y <integer>) (str <string>) (color <integer>) &keyword direction)
				 (string! (im <gd-image>) (fg <integer>) (font <string>) (pt <real>) (angle <real>) (x <integer>) (y <integer>) (str <string>))
				 (string! (im <gd-image>) (x <integer>) (y <integer>) (str <string>) &keyword font fg pt angle))
		 ("The first variant calls either \"gd-image-string\" or \"gd-image-string-up\" according to the symbol following keyword `direction'."
		  "The usage of the second one is consistent with \"gd-image-string-ft\"."
		  "The third, an abbreviation of the second, treats default values of parameters if not specified with keywords. See also \"current-ft-*\" and \"with-ft-font/fg/pt/angle\".")
		 ("�ǽ�η��ϥ������ `direction' �ȤȤ��Ϳ�����륷��ܥ�˽��ä� \"gd-image-string\" �ޤ��� \"gd-image-string-up\" ��ƤӽФ��ޤ���"
		  "2���ܤη��� \"gd-image-string-ft\" �θƤӽФ����б����ޤ���"
		  "�Ǹ�η���2���ܤ�ά���ǡ�������ɤȤȤ�˥ѥ�᡼�������ꤵ��ʤ���Хǥե���Ȥ��ͤ��Ѥ��ޤ���\"current-ft-*\" �� \"with-ft-font/fg/pt/angle\" �⻲�Ȥ��Ƥ���������"))

		((parameter current-ft-font
					current-ft-fg
					current-ft-pt
					current-ft-angle)
		 ("Sometimes it is useful to fix parameters in question to print strings subsequently on an image. These are reserved for such a case and its values are referred in a call of \"string!\" without optional arguments or keywords.")
		 ("���᡼�����Ϣ³����ʸ�������Ϥ���ݡ��ط�����ѥ�᡼������ꤹ��������ʾ�礬����ޤ����������ä����Τ���ˤ����Υѥ�᡼�����Ѱդ���Ƥ��ꡢ���ץ���ʥ�ʰ����䥭����ɤǻ��ꤵ�줺�� \"string!\" ���ƤФ줿���˻��Ȥ���ޤ���"))

		((procedure (with-ft-font/fg/pt/angle font fg pt angle thunk))
		 ("Call `thunk' with parameterized current-ft-font, current-ft-fg, current-ft-pt, and current-ft-angle. Its return value is `thunk''s one.")
		 ("current-ft-font, current-ft-fg, current-ft-pt, current-ft-angle ��Ϳ����줿�ͤˤ��� `thunk' ��ƤӤޤ������� `thunk' ���������ͤ��֤���current-ft-* ���ͤ���������ޤ���"))

		((method (gif-anim-begin (im <gd-image>) (oport <port>) (GlobalCM <integer>) (loops <integer>))
				 (gif-anim-add (im <gd-image>) (oport <port>) (localCM <integer>) (LeftOfs <integer>) (TopOfs <integer>) (Delay <integer>) (Disposal <integer>) &optional (previm <gd-image>))
				 (gif-anim-end (oport <port>)))
		 ("Create and output a GIF Animation. In case of calling gif-anim-add without the last argument `previm', GD does not optimize the resulting frames."
		  "Rather than a pair of gif-anim-begin and gif-anim-end, gif-anim-with is often preferable.")
		 ("�����Υ᥽�åɤˤ�ä� GIF ���˥᡼��������Ϥ��ޤ���gif-anim-add �κǸ�ΰ������ά������硢GD �ϼ�ưŪ�ʺ�Ŭ����Ԥ��ޤ���"
		  "gif-anim-end �� gif-anim-end ���Фˤ��ƻȤ���ꡢ��³�� gif-anim-with �����Ѥ��뤳�Ȥ򴫤�ޤ���"))

		((procedure (gif-anim-with im oport thunk &keyword global-cm loop))
		 ("Call `thunk' with gif-anim-began `im' and `port'. Before returning the procedure successfully, gif-anim-end is called expectedly.")
		 ("���μ�³���ϡ��ޤ� `im' �� `port' �ȥ�����ɤ�Ϳ����줿�ͤ�����ˤ��� gif-anim-begin ��Ƥӡ����ξ�� `thunk' ��ƤӤޤ���̵���˼�³������������� gif-anim-end ��ƤӤޤ���"))

        ((method (pixel-for-each (im <gd-image>) proc)
                 (pixel-fold (im <gd-image>) proc knil))
         ("pixel-for-each calls 'proc' once for each pixel of image 'im' with arguments: x-coordinate, y-coordinate, and its pixel value obtained by 'gd-image-get-pixel'. Its return value is unspecified."
          "This allows you to write a filter as:"
          0
          "pixel-fold calls 'proc' once for each pixel of image 'im' with 4 argumens: x-coordinate, y-coordinate, its pixel value, and the temporary seed, and returns the resulting reduction.")
         ("�᥽�å� pixel-for-each �ϳƥԥ����뤴�Ȥ� x ��ɸ��y ��ɸ������� gd-image-get-pixel ���ͤ�3�Ĥΰ����� 'proc' ��ƤӤޤ�������ͤ�����Ǥ���"
          "���Υ᥽�åɤˤ�äƼ��Τ褦�ʥե��륿����񤯤��Ȥ��Ǥ��ޤ�:"
          0
          "�᥽�å� pixel-fold �Ͻ���� knil ���Ȥˡ��ƥԥ����뤴�Ȥ� x��y���ԥ������͡�����Ӥ��λ����Ǥξ��߹��ߤ��줿�ͤ�4�Ĥΰ����� 'proc' ��ƤӤޤ�������ͤϾ��߹��ޤ줿�ͤǤ���")
         "(define-method invert (im <gd-image>)
  (pixel-for-each im
                  (lambda (x y pixel)
                    (gd-image-set-pixel im x y pixel
                                        (gd-true-color (- 255 (gd-image-red im pixel))
                                                       (- 255 (gd-image-green im pixel))
                                                       (- 255 (gd-image-blue im pixel)))))))"
         )
		))

(define (document-tree lang)
  (let ((title (if (eq? 'ja lang) "Gauche-gd ��ե���󥹥ޥ˥奢��" "Gauche-gd Reference Manual")))
	(html:html
	 (html:head
	  (if (eq? 'ja lang) (html:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8") '())
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
	   "#project { text-align: right; }\n"
	   " -->")
	  (html:p "For version " *version*)
	  (html:p :id "last_update" "last update: " *last-update*)
	  (html:p :id "project" (html:a :href "http://www.fixedpoint.jp/gauche-gd/" "http://www.fixedpoint.jp/gauche-gd/"))
	  (html:h2 "C Layer API")
	  (if (eq? 'en lang)
		  (c-layer-api en)
		  (c-layer-api ja))
	  (html:h2 "Simple API")
	  (if (eq? 'en lang)
		  (html:p "(Experimental. See graphics/gd.scm and example/*.scm if interested.)")
		  (html:p "(�ޤ��Ū�ʤ�ΤǤ�����̣�Τ������� graphics/gd.scm �� example/*.scm �򸫤Ƥ���������)"))
	  (if (eq? 'en lang)
		  (simple-api en)
		  (simple-api ja))
	  (html:address "&copy; 2006-2011 Takeshi Abe")
	  ))))

(define (main args)
  (define (usage)
	(format (current-error-port) "usage: gosh reference.scm (en|ja)\n")
	(exit 1))
  (when (< (length args) 2)
	(usage))
  (write-tree (document-tree (string->symbol (cadr args))))
  0)
