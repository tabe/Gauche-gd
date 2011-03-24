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
		("基本となるイメージのクラスです。実際には GD の \"gdImage\" への foreign pointer です。\"gd-image-\" (C では \"gdImage\")で始まるほとんどの手続きで第1引数として用いられます。"))

	   ((class <gd-font>)
		("Another fundamental class. Its instance has a foreign pointer to GD's \"gdFont\". You can get its instance by any of \"gd-font-get-*\".")
		("基本となるフォントのクラスです。実際には GD の \"gdFont\" への foreign pointer です。手続き \"gd-font-get-*\" のいずれかでインスタンスを得ることができます。"))

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
		("それぞれ C の同名の定数と同じ数値を値に持ちます。"))

	   ((variable GD2_ID)
		("It has a string value same with its C-equivalent.")
		("C の同名の定数と同じ文字列を値に持ちます。"))

	   ((procedure (gd-true-color-get-alpha c)
				   (gd-true-color-get-red c)
				   (gd-true-color-get-green c)
				   (gd-true-color-get-blue c))
		("Each of these is equivalent to gdTrueColorGetAlpha, gdTrueColorGetRed, gdTrueColorGetGreen, or gdTrueColorGetBlue respectively.")
		("それぞれ gdTrueColorGetAlpha, gdTrueColorGetRed, gdTrueColorGetGreen, gdTrueColorGetBlue に対応します。"))

	   ((procedure (gd-alpha-blend dest src))
		("This is equivalent to gdAlphaBlend.")
		("gdAlphaBlend に対応します。"))

	   ((procedure (gd-image-create sx sy)
				   (gd-image-create-palette sx sy)
				   (gd-image-create-true-color sx sy))
		("Each of these is a constructor of <gd-image> which is equivalent to gdImageCreate, gdImageCreatePalette, or gdImageCreateTrueColor respectively. If failed these return #f.")
		("それぞれ gdImageCreate, gdImageCreatePalette, gdImageCreateTrueColor に対応し、<gd-image> オブジェクトを返します。失敗した場合は #f を返します。"))

	   ((procedure (gd-image-create-from-png path)
				   (gd-image-create-from-gif path)
				   (gd-image-create-from-jpeg path)
				   (gd-image-create-from-wbmp path)
				   (gd-image-create-from-xbm path)
				   (gd-image-create-from-gd path)
				   (gd-image-create-from-gd2 path)
				   (gd-image-create-from-gd2-part path))
		("Like C's gdImageCreateFrom* family, one of these creates a <gd-image> object from a source file in its particular image format. Unlike its C's equivalents, it treats the string-value single argument as a path of source. The file handle has been closed before successful return. In case of failure #f is returned.")
		("C の gdImageCreateFrom* 関数と同様に、これらの手続きはそれぞれ特定のイメージフォーマットに応じてファイルから <gd-image> オブジェクトを作成します。ただ C の同等の関数と異なり、引数にソースファイルのパスを文字列で指定します。手続きに成功して戻った場合にはソースファイルのハンドルは閉じられています。失敗した場合には #f を返します。"))

	   ((procedure (gd-image-create-from-xpm path))
		("The gdImageCreateFromXpm equivalent, which returns a <gd-image> object in case of success, otherwise #f.")
		("C の gdImageCreateFromXpm に対応する手続きです。成功した場合には <gd-image> オブジェクトを、失敗した場合には #f を返します。"))

	   ((procedure (gd-image-destroy im))
		("Call gdImageDestroy() explicitly. Different from the C version it is idempotent, meaning that it can be repeated safely."
		 "(Because of gc, it is little necessary to use this procedure.)")
		("<gd-image> オブジェクトの指している \"gdImage\" リソースに対して明示的に gdImageDestroy を呼びます。繰り返し呼ばれても安全です。"
		 "(メモリに関する特別な制限のない限り、こういったリソースの解放は gc に任せておいて構いません。)"))

       ((procedure (gd-image-set-pixel im x y color)
				   (gd-image-get-pixel im x y)
				   (gd-image-get-true-color-pixel im x y))
        ("Each of these is equivalent to gdImageSetPixel, gdImageGetPixel, or gdImageGetTrueColorPixel respectively.")
        ("それぞれ gdImageSetPixel, gdImageGetPixel, gdImageGetTrueColorPixel に対応します。"))

       ((procedure (gd-image-line im x1 y1 x2 y2 color))
        ("The gdImageLine alternative.")
        ("関数 gdImageLine に対応する手続きです。"))

       ((procedure (gd-image-rectangle im x1 y1 x2 y2 color)
				   (gd-image-filled-rectangle im x1 y1 x2 y2 color))
        ("Each of these is equivalent to gdImageRectangle or gdImageFilledRectangle respectively.")
        ("それぞれ gdImageRectangle, gdImageFilledRectangle に対応する手続きです。"))

       ((procedure (gd-image-set-clip im x1 y1 x2 y2)
				   (gd-image-get-clip im))
        ("As well as gdImageSetClip the former one clips a rectangle from the image for subsequent drawing. The latter returns the current clip with four integer-values.")
        ("gdImageSetClip と同様に gd-image-set-clip は以後の描画に適用されるクリップ領域を設定します。gd-image-get-clip は現在のクリップ領域の座標を表す4つの数値を返します。"))

	   ((procedure (gd-image-bounds-safe im x y))
		("Given the coordinates of a point, return either 1 (if it is in the current clip) or 0 (otherwise). You should use it in order to check whether the coordinates is suitable or not for arguments of another procedure, e.g. gd-image-fill.")
		("クリップ領域内の座標があたえられた場合は1を、さもなくば0を返します。gd-image-fill などの手続きの引数に適した座標かどうかを確認するためにこの関数を利用するべきです。"))

	   ((procedure (gd-image-char im f x y c color)
				   (gd-image-char-up im f x y c color))
		("Put a single byte character on the given image with the font provided by a gd-font-get-*. If you would like to print a string containing multibyte characters, try procedure \"gd-image-string-ft\" or method \"string!\".")
		("イメージへ1バイト文字を出力します。第2引数のフォントは手続き gd-font-get-* から取得してください。マルチバイト文字を含む文字列を出力する場合には手続き \"gd-image-string-ft\" またはメソッド \"string!\" を使ってください。"))

	   ((procedure (gd-image-string im f x y s color)
				   (gd-image-string-up im f x y s color))
		("Put a string consising of single byte characters, with the font provided by a gd-font-get-*. If you would like to print a string containing multibyte characters, try gd-image-string-ft.")
		("イメージへ1バイト文字からなる文字列を出力します。第2引数のフォントは手続き gd-font-get-* から取得してください。マルチバイト文字を含む文字列を出力する場合には手続き \"gd-image-string-ft\" またはメソッド \"string!\" を使ってください。"))

	   ((procedure (gd-image-string-ft im fg fontlist ptsize angle x y str))
		("Print a string with a FreeType font specified by the path `fontlist'. Unlike the original version, it return *four* pairs of integers which represent the coordinates of the points surrounding the bounding rectangle, and coming lower-left, lower-right, upper-right, and upper-left in that order. "
		 "It is also possible to obtain these values efficiently, i.e. without printing `str', by giving #f to `im'."
		 "If your gosh is configured with option \"--enable-multibyte=utf-8\", then congratulations! and multibyte characters will be available in `str' (with an appropriate font, of course). Otherwise you had better use method \"string!\".")
		("`fontlist' でパスを指定することで FreeType フォントを用いて文字列を書き出します。オリジナルの C の関数と異なり、この手続きは*4つ*のペアを返し、それぞれが出力先の矩形を囲む座標(順に左下、右下、右上、左上)を表します。"
		 "`im' に #f を渡すことで描画せずにこの矩形の座標を効率的に求めることができます。"
		 "gosh が \"--enable-multibyte=utf-8\" というオプション付きでビルドされていれば `str' でマルチバイト文字が利用できます。そうでない場合はメソッド \"string!\" の使用を考えてください。"))

	   ((procedure (gd-image-polygon im points pointsTotal color)
				   (gd-image-filled-polygon im points pointsTotal color)
				   (gd-image-open-polygon im points pointsTotal color))
		("The second argument of these procedure, which expresses a sequence of points, must satisfy the following conditions:"
		 "- of type proper <list>, and"
		 "- its elements are of type <pair> and consist of integers which correspond to the coordinates of a point."
		 "Furthermore the third one is expected to be a non-negative integer.")
		("これらの手続きはそれぞれ gdImagePolygon, gdImageFilledPolygon, gdImageOpenPolygon に対応します。ただし、いずれも座標列を表す第2引数として循環の無いリストを指定します。このリストの各要素は座標を表すペアでなければなりません。さらに第3引数は非負整数であることが求められます。"))

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
		("それぞれ C の関数 gdImageColorAllocate, gdImageColorAllocateAlpha, gdImageColorClosest, gdImageColorClosestAlpha, gdImageColorClosestHWB, gdImageColorExact, gdImageColorExactAlpha, gdImageColorResolve, gdImageColorResolveAlpha に対応します。"))

	   ((procedure (gd-true-color r g b)
				   (gd-true-color-alpha r g b a))
		("Same as gdTrueColor or gdTrueColorAlpha resp. for each.")
		("それぞれ C の関数 gdTrueColor, gdTrueColorAlpha に対応します。"))

	   ((procedure (gd-image-color-deallocate im color))
		("Similar to gdImageColorDeallocate it reduces a color in the palette.")
		("gdImageColorDeallocate と同様にパレットの色情報を破棄し再利用できるようにします。"))

	   ((procedure (gd-image-create-palette-from-true-color im ditherFlag colorsWanted)
				   (gd-image-true-color-to-palette im ditherFlag colorsWanted))
		("Both convert a true color image into the palette one while the latter destructively returns into the result argument `im'. If the former fails it returns #f")
		("いずれも true color イメージをパレットイメージに変換します。ただし後者は与えられたイメージを破壊的に変更しその引数に結果を返します。また前者が失敗した場合 #f が返ります。"))

	   ((procedure (gd-image-color-transparent im color))
		("The gdImageColorTransparent equivalent.")
		("関数 gdImageColorTransparent に対応します。"))

	   ((procedure (gd-image-palette-copy dst src))
		("The gdImagePaletteCopy alternative.")
		("関数 gdImagePaletteCopy に対応します。"))

	   ((procedure (gd-image-filled-arc im cx cy w h s e color style)
				   (gd-image-arc im cx cy w h s e color))
		("Like gdImageFilledArc or gdImageArc respectively.")
		("それぞれ gdImageFilledArc, gdImageArc に対応します。"))

	   ((procedure (gd-image-filled-ellipse im cx cy w h color))
		("The gdImageFilledEllipse equivalent.")
		("関数 gdImageFilledEllipse に対応します。"))

	   ((procedure (gd-image-fill-to-border im x y border color)
				   (gd-image-fill im x y color))
		("Each of these is equivalent to gdImageFillToBorder or gdImageFill respectively.")
		("それぞれ gdImageFillToBorder, gdImageFill に対応します。"))

	   ((procedure (gd-image-copy dst src dstX dstY srcX srcY w h)
				   (gd-image-copy-merge dst src dstX dstY srcX srcY w h pct)
				   (gd-image-copy-merge-gray dst src dstX dstY srcX srcY w h pct)
				   (gd-image-copy-resized dst src dstX dstY srcX srcY dstW dstH srcW srdH)
				   (gd-image-copy-resampled dst src dstX dstY srcX srcY dstW dstH srcW srdH)
				   (gd-image-copy-rotated dst src dstX dstY srcX srcY srcWidth srdHeight angle))
		("Note that some of these will preserve the palette of the given destination image as well as their C-equivalents.")
		("これらの手続きの中には対応する gdImageCopy* 関数と同じように与えられたコピー先のパレットを変更しないものもあることに注意してください。"))

	   ((procedure (gd-image-set-brush im brush)
				   (gd-image-set-tile im tile)
				   (gd-image-set-anti-aliased im c)
				   (gd-image-set-anti-aliased-dont-blend im c dont_blend)
				   (gd-image-set-thickness im thickness)
				   (gd-image-interlace im interlaceArg)
				   (gd-image-alpha-blending im blending)
				   (gd-image-save-alpha im saveFlag))
		("The following functions are called respectively: gdImageSetBrush, gdImageSetTile, gdImageSetAntiAliased, gdImageSetAntiAliasedDontBlend, gdImageSetThickness, gdImageInterlace, gdImageAlphaBlending, and gdImageSaveAlpha.")
		("それぞれ gdImageSetBrush, gdImageSetTile, gdImageSetAntiAliased, gdImageSetAntiAliasedDontBlend, gdImageSetThickness, gdImageInterlace, gdImageAlphaBlending, gdImageSaveAlpha に対応します。"))

		((procedure (gd-image-set-style im style styleLength))
		 ("Given style as a list, call gdImageSetStyle.")
		 ("style をリストとして与えることで gdImageSetStyle を呼びます。"))

 	   ((procedure (gd-image-true-color im))
		("It returns 0 for a palette image, otherwise non-0.")
		("パレットイメージが与えられた場合は0を、さもなくば0以外の数値を返します。"))

	   ((procedure (gd-image-sx im)
				   (gd-image-sy im))
		("Each returns the sx(`width') or sy(`height') of the image respectively.")
		("それぞれ与えられたイメージの横もしくは縦の幅を返します。"))

	   ((procedure (gd-image-colors-total im))
		("Given a palette image it returns the number of currently allocated colors in the palette.")
		("パレットイメージが与えられた場合、パレットの色数を返します。"))

	   ((procedure (gd-image-red im c)
				   (gd-image-green im c)
				   (gd-image-blue im c)
				   (gd-image-alpha im c))
		("Each of these corresponds to gdImageRed, gdImageGreen, gdImageBlue, or gdImageAlpha respectively.")
		("それぞれ gdImageRed, gdImageGreen, gdImageBlue, gdImageAlpha に対応します。"))

	   ((procedure (gd-image-get-transparent im)
				   (gd-image-get-interlaced im))
		("Each of these is equivalent to gdImageGetTransparent or gdImageGetInterlaced respectively.")
		("それぞれ gdImageGetTransparent, gdImageGetInterlaced に対応します。"))

	   ((procedure (gd-image-palette-pixel im x y)
				   (gd-image-true-color-pixel im x y))
		("These inherits the bounds-unsafe nature from their C-alternatives.")
		("これらの手続きはマクロ gdImagePalettePixel や gdImageTrueColorPixel を直接呼び出すため、引数が適切かどうか事前に確認する必要があるでしょう。"))

	   ((procedure (gd-image-compare im1 im2))
		("Return 0 If two images are same wrt displayed components, otherwise non-0.")
		("与えられた2つのイメージが描画される構成要素に関して等しければ0を、さもなくば0以外の数値を返します。"))

	   ((procedure (gd-image-square-to-circle im radius)
				   (gd-image-sharpen im pct))
		("Like gdImageSquareToCircle or gdImageSharpen respectively. The former returns #f in case of failure.")
		("それぞれ gdImageSquareToCircle, gdImageSharpen に対応します。前者は失敗した場合 #f を返します。"))

	   ((procedure (gd-font-get-giant)
				   (gd-font-get-large)
				   (gd-font-get-medium-bold)
				   (gd-font-get-small)
				   (gd-font-get-tiny))
		("One of these procedures give you the font of size Giant, Large, MediumBold, Small, or Tiny respectively, which is for gd-image-char, gd-image-string etc. When the specified size is unavailable it return #f.")
		("これらの手続きはそれぞれサイズが Giant, Large, MediumBold, Small, Tiny のフォントを返します。このフォントは gd-image-char や gd-image-string 等の引数に利用されます。フォントが得られない場合は #f を返します。"))
	   ))

(define-macro (simple-api lang)
  `(def ,lang
		((constant *gd-version*)
		 ("The version of GD library. Detected at the compile time of the package.")
		 ("GD のバージョン。Gauche-gd のコンパイル時に取得したものです。"))

		((constant *gd-features*)
		 ("A list of symbols which mean available features of GD."
		  "Possible symbols in the list: fontconfig freetype gif jpeg png xpm."
          "You can also use feature identifiers of form 'gauche.ext.graphics.gd.*' with cond-expand."
          "Note that they are available after loading the module.")
		 ("有効な GD の機能を表すシンボルのリスト。"
		  "このリストに含まれる可能性のあるシンボルは以下の通り: fontconfig freetype gif jpeg png xpm。"
          "また 'gauche.ext.graphics.gd.*' という形の feature indentifiers を cond-expand とともに使うこともできます。"
          "これらの identifiers はモジュールのロード後に利用できます。"))

		((parameter current-gd-image-format)
		 ("It is expected to keep a symbol which decides the image format taken with \"read-gd-image\" and/or \"write\" unless specified.")
		 ("GD のイメージは本来特定のフォーマットに依存しない形に抽象化されていますが、入出力を行う際に特定のフォーマットが前提になっていることがよくあります。\"read-gd-image\" や \"write\" は このパラメータの値を参照するので、そういった場合に利用できます。"))

		((procedure (with-gd-image-format fmt thunk))
		 ("Call `thunk' with the parameterized \"current-gd-image-format\" of value `fmt'.")
		 ("上記の \"current-gd-image-format\" の値を一時的に `fmt' にして `thunk' を呼びます。戻ると \"current-gd-image-format\" の値は復元されます。`thunk' の戻り値がこの手続きの戻り値です。"))

		((method (read-gd-image (port <port>) &optional (fmt <symbol>) &keyword x y w h))
		 ("Read a image with or without format `fmt' (such as \"gif\", \"jpe\", \"jpeg\", \"jpg\", \"png\", \"wbmp\", \"gd\", and \"gd2\"). See also \"current-gd-image-format\", or #f if an error occurs."
		  "The keywords `x', `y', `w' and `h' are only for the GD2 format and corresponding to the 2nd, 3rd, 4th and 5th argument of \"gdImageCreateFromGd2PartCtx\" respectively.")
		 ("ポート `port' からイメージを入力します。エラーの場合は #f を返します。フォーマット `fmt' として取り得る値は \"gif\", \"jpe\", \"jpeg\", \"jpg\", \"png\", \"wbmp\", \"gd\", 及び \"gd2\" です。\"current-gd-image-format\" も参照してください。"
		  "キーワード `x'、`y'、`w' 及び `h' は GD2 フォーマットに対してのみ適用され、それぞれ \"gdImageCreateFromGd2PartCtx\" の 2番目、3番目、4番目及び5番目の引数に対応します。"))

		((method (write-as (im <gd-image>) (fmt <symbol>) port &keyword quality foreground))
		 ("Put a image `im' with format `fmt' (such as \"gif\", \"jpe\", \"jpeg\", \"jpg\", \"png\", and \"wbmp\")."
		  "If the JPEG format specified, you can suggest the `quality' of the resulting image. The keyword is just ignored otherwise."
		  "If the WBMP format specified, you can set the color index of pixels which are considered as `foreground'. The keyword is just ignored otherwise.")
		 ("ポートへイメージ `im' を出力します。フォーマット `fmt' に取り得る値は \"gif\", \"jpe\", \"jpeg\", \"jpg\", \"png\", 及び \"wbmp\" です。"
		  "JPEG フォーマットが指定された場合には、キーワード `quality' に与えられる値によって出力の質(と、結果的にサイズ)を調節できます。このキーワードが与えられない場合には GD によってデフォルトの値が利用されます。それ以外のフォーマットについては、単に無視されます。"
		  "WBMP フォーマットが指定された場合には、キーワード `foreground' によってビットがセットされるカラーインデックスを与えることができます。このインデックスを持たないピクセルは\"背景\"として扱われます。このキーワードが与えられない場合には GD はデフォルトの振る舞いをします。"))

		((method (display (im <gd-image>) &optional port)
				 (write (im <gd-image>) &optional port))
		 ("They are the same abbreviations of \"write-as\". See also \"current-gd-image-format\".")
		 ("\"write-as\" の省略形で、いずれも同じように振舞います。\"current-gd-image-format\" も参照してください。"))

		((method (save-as (im <gd-image>) (path <string>))
				 (save-as (im <gd-image>) (path <string>) (fmt <symbol>) &keyword quality foreground chunk-size compress))
		 ("It provides another way to output a image. It tries to create a file of path `path' even if exists and return 0 in case of success. In the former case it choices the output image format by the extension (such as \"gif\", \"jpe\", \"jpeg\", \"jpg\", \"png\", \"wbmp\", \"gd\", and \"gd2\") of the path. Explicit `fmt' is used in the latter."
		  "Available formats (if supported): GIF, JPEG, PNG, WBMP, GD, AND GD2."
		  "If the GD2 format specified, you can set the options of the resulting image by the keywords `chunk-size' and `compress'. If a true value following `compress' (default), then the image will be compressed with the given `chunk-size' (or, a default one without `chunk-size').  Otherwise it will be uncompressed."
		  "See \"write-as\" on the keyword `quality' for JPEG and `foreground' for WBMP.")
		 ("イメージを出力するメソッドの1つです。`path' として与えられたファイルを(既存であっても)新しく作成します。成功した場合は0を返します。前者の形で呼ばれた場合、出力イメージフォーマットは `path' の拡張子によって選択されます。判別される拡張子は \"gif\", \"jpg\", \"jpeg\", \"jpe\", \"png\", \"wbmp\", \"gd\", \"gd2\" です。後者の形で明示的にフォーマット `fmt' を与えられます。"
		  "(サポートしていれば)利用できるフォーマットは GIF, JPEG, PNG, WBMP, GD, GD2 です。"
		  "GD2 フォーマットが指定された場合、キーワード `chunk-size' 及び `compress' によってオプションを指定できます。`compress' に対して真の値が与えられたとき(デフォルト)には、書き出されるイメージは `chunk-size' の値を使って圧縮されます。`chunk-size' が与えられない場合、GD はデフォルトのサイズを選択します。`compress' が偽なら圧縮されません。"
		  "JPEG フォーマットのためのキーワード `quality' や WBMP フォーマットのためのキーワード `foreground' については、\"write-as\" を参照してください。"))

		((method (char! (im <gd-image>) (f <gd-font>) (x <integer>) (y <integer>) (c <integer>) (color <integer>) &keyword direction))
		 ("Put a character on the given `im'. If symbol 'up follows keyword `direction', \"gd-image-char-up\" is called instead of \"gd-image-char\".")
		 ("イメージへ文字を書き出します。キーワード `direction' に続いてシンボル 'up が指定された場合、\"gd-image-char\" の代わりに \"gd-image-char-up\" が呼ばれます。"))

		((method (string! (im <gd-image>) (f <gd-font>) (x <integer>) (y <integer>) (str <string>) (color <integer>) &keyword direction)
				 (string! (im <gd-image>) (fg <integer>) (font <string>) (pt <real>) (angle <real>) (x <integer>) (y <integer>) (str <string>))
				 (string! (im <gd-image>) (x <integer>) (y <integer>) (str <string>) &keyword font fg pt angle))
		 ("The first variant calls either \"gd-image-string\" or \"gd-image-string-up\" according to the symbol following keyword `direction'."
		  "The usage of the second one is consistent with \"gd-image-string-ft\"."
		  "The third, an abbreviation of the second, treats default values of parameters if not specified with keywords. See also \"current-ft-*\" and \"with-ft-font/fg/pt/angle\".")
		 ("最初の形はキーワード `direction' とともに与えられるシンボルに従って \"gd-image-string\" または \"gd-image-string-up\" を呼び出します。"
		  "2番目の形は \"gd-image-string-ft\" の呼び出しと対応します。"
		  "最後の形は2番目の略記で、キーワードとともにパラメータが指定されなければデフォルトの値を用います。\"current-ft-*\" や \"with-ft-font/fg/pt/angle\" も参照してください。"))

		((parameter current-ft-font
					current-ft-fg
					current-ft-pt
					current-ft-angle)
		 ("Sometimes it is useful to fix parameters in question to print strings subsequently on an image. These are reserved for such a case and its values are referred in a call of \"string!\" without optional arguments or keywords.")
		 ("イメージ上へ連続して文字列を出力する際、関係するパラメータを固定すると便利な場合があります。こういった場合のためにこれらのパラメータが用意されており、オプショナルな引数やキーワードで指定されずに \"string!\" が呼ばれた時に参照されます。"))

		((procedure (with-ft-font/fg/pt/angle font fg pt angle thunk))
		 ("Call `thunk' with parameterized current-ft-font, current-ft-fg, current-ft-pt, and current-ft-angle. Its return value is `thunk''s one.")
		 ("current-ft-font, current-ft-fg, current-ft-pt, current-ft-angle を与えられた値にして `thunk' を呼びます。戻ると `thunk' からの戻り値を返し、current-ft-* の値は復元されます。"))

		((method (gif-anim-begin (im <gd-image>) (oport <port>) (GlobalCM <integer>) (loops <integer>))
				 (gif-anim-add (im <gd-image>) (oport <port>) (localCM <integer>) (LeftOfs <integer>) (TopOfs <integer>) (Delay <integer>) (Disposal <integer>) &optional (previm <gd-image>))
				 (gif-anim-end (oport <port>)))
		 ("Create and output a GIF Animation. In case of calling gif-anim-add without the last argument `previm', GD does not optimize the resulting frames."
		  "Rather than a pair of gif-anim-begin and gif-anim-end, gif-anim-with is often preferable.")
		 ("これらのメソッドによって GIF アニメーションを出力します。gif-anim-add の最後の引数を省略した場合、GD は自動的な最適化を行いません。"
		  "gif-anim-end と gif-anim-end を対にして使うより、手続き gif-anim-with を利用することを勧めます。"))

		((procedure (gif-anim-with im oport thunk &keyword global-cm loop))
		 ("Call `thunk' with gif-anim-began `im' and `port'. Before returning the procedure successfully, gif-anim-end is called expectedly.")
		 ("この手続きは、まず `im' と `port' とキーワードに与えられた値を引数にして gif-anim-begin を呼び、その上で `thunk' を呼びます。無事に手続きから戻る前に gif-anim-end を呼びます。"))

        ((method (pixel-for-each (im <gd-image>) proc)
                 (pixel-fold (im <gd-image>) proc knil))
         ("pixel-for-each calls 'proc' once for each pixel of image 'im' with arguments: x-coordinate, y-coordinate, and its pixel value obtained by 'gd-image-get-pixel'. Its return value is unspecified."
          "This allows you to write a filter as:"
          0
          "pixel-fold calls 'proc' once for each pixel of image 'im' with 4 argumens: x-coordinate, y-coordinate, its pixel value, and the temporary seed, and returns the resulting reduction.")
         ("メソッド pixel-for-each は各ピクセルごとに x 座標、y 座標、および gd-image-get-pixel の値の3つの引数で 'proc' を呼びます。戻り値は不定です。"
          "このメソッドによって次のようなフィルターを書くことができます:"
          0
          "メソッド pixel-fold は初期値 knil をもとに、各ピクセルごとに x、y、ピクセル値、およびその時点での畳み込みされた値の4つの引数で 'proc' を呼びます。戻り値は畳み込まれた値です。")
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
  (let ((title (if (eq? 'ja lang) "Gauche-gd リファレンスマニュアル" "Gauche-gd Reference Manual")))
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
		  (html:p "(まだ試験的なものです。興味のある方は graphics/gd.scm や example/*.scm を見てください。)"))
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
