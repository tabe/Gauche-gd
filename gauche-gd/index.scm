#!/usr/bin/env gosh
;; -*- mode: scheme; coding: euc-jp -*-

(use fixedpoint.package)
(use fixedpoint.site)
(use text.html-lite)

(//
 (GD.pm "http://search.cpan.org/dist/GD/")
 (GD/Scheme "http://www196.pair.com/lisovsky/scheme/gd/index.html")
 (CL-GD "http://weitz.de/cl-gd/")
 (FreeType "http://www.freetype.org/")
 )

(define-package Gauche-gd 2011 3 24)

(define-index Gauche-gd
  (html:p (html:dfn /Gauche-gd/)
          (en/ja
           (list " is an extension package of " /Gauche/ " which provides a binding of Thomas Boutell's " /GD/ "2 library.")
           (list " は " /Scheme/ " 処理系 " /Gauche/ " で " /GD/ "2 ライブラリを利用するための拡張パッケージです。")))

  (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "News" "最新情報"))
  (html:ul
   (html:li "[2011-03-24] " (en/ja "Maintainance release 0.3.1 for Gauche 0.9.1. Export procedure 'gd-image-set-style'."
                                   "Gauche 0.9.1 向けのバージョン 0.3.1 を公開しました。手続き 'gd-image-set-style' が API に加わりました。"))
   (html:li "[2009-11-23] " (en/ja "It is confirmed that the current version 0.3.0 runs on Gauche 0.9."
                                   "Gauche 0.9 で現在のバージョン 0.3.0 が動作することを確認しました。"))
   (html:li "[2008-10-20] " (en/ja "It is confirmed that the current version 0.3.0 runs on Gauche 0.8.14."
                                   "Gauche 0.8.14 で現在のバージョン 0.3.0 が動作することを確認しました。"))
   (html:li "[2008-07-17] " (en/ja "Release 0.3.0 for Gauche 0.8.13. Feature indentifiers of form 'gauche.ext.graphics.gd.*'."
                                   "Gauche 0.8.13 向けのバージョン 0.3.0 を公開しました。'gauche.ext.graphics.gd.*' という形の機能識別子を追加しました。"))
   (html:li "[2007-11-01] " (en/ja "It is confirmed that the current version 0.2.0 runs on Gauche 0.8.12."
                                   "Gauche 0.8.12 で現在のバージョン 0.2.0 が動作することを確認しました。"))
   (html:li "[2007-08-15] " (en/ja "Release 0.2.0. Methods pixel-for-each and pixel-fold added."
                                   "バージョン 0.2.0 を公開しました。メソッド pixel-for-each および pixel-fold を追加しました。"))
   (html:li "[2007-06-21] " (en/ja "Congratulation! GD 2.0.35 released."
                                   "GD 2.0.35 がリリースされました。"))
   (html:li "[2007-02-07] " (en/ja "Congratulation! GD 2.0.34 released."
                                   "GD 2.0.34 がリリースされました。"))
   (html:li "[2007-01-18] " (en/ja "It is confirmed that the current version 0.1.4 runs on Gauche 0.8.9."
                                   "Gauche 0.8.9 で現在のバージョン 0.1.4 が動作することを確認しました。"))
   (html:li "[2006-12-11] " (en/ja "Release 0.1.4. Some bugs fixed and some options added."
                                   "バージョン 0.1.4 を公開しました。いくつかのバグを修正し、オプションを追加しました。"))
   (html:li "[2006-11-27] " (en/ja "Release 0.1.3. API for GIF Animations available."
                                   "バージョン 0.1.3 を公開しました。GIF アニメーションをサポートしました。"))
   (html:li "[2006-11-16] " (en/ja "It is confirmed that the current version 0.1.2 runs on Gauche 0.8.8."
                                   "Gauche 0.8.8 で現在のバージョン 0.1.2 が動作することを確認しました。"))
   (html:li "[2006-11-07] " (en/ja "Release 0.1.2." "バージョン 0.1.2 を公開しました。"))
   (html:li "[2006-11-03] " (en/ja "Release 0.1.1." "バージョン 0.1.1 を公開しました。"))
   (html:li "[2006-10-28] " (en/ja "Some links added." "リンクを追加しました。"))
   (html:li "[2006-10-26] " (en/ja "Release 0.1.0." "バージョン 0.1.0 を公開しました。")))

  (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Features" "特徴"))
  (html:ul
   (html:li (en/ja "Through a straitforward API you can call a C function or macro directly."
                   "対応する手続きによって直接 C の関数やマクロを呼ぶことができます。"))
   (html:li (en/ja "A simpler interface (experimental):"
                   "高レベルなインターフェース(開発中):")
            (html:ul
             (html:li (en/ja (list "Printing a Scheme string containing possible multibyte characters with a " /FreeType/ " font")
                             (list /FreeType/ " フォントによるマルチバイト文字を含む Scheme 文字列の表示")))
             (html:li (en/ja "I/O through gdIOCtx compatible ports"
                             "gdIOCtx 互換の port による入出力"))
             (html:li (en/ja "Generating GIF Animations"
                             "GIF アニメーションの生成"))
             (html:li (en/ja "Feature indentifiers of form 'gauche.ext.graphics.gd.*'"
                             "'gauche.ext.graphics.gd.*' という形の機能識別子"))
             )
            ))

  (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Requirements" "導入"))
  (html:p (en/ja "This package is for Gauche 0.8.7 or later."
                 "このパッケージは Gauche 0.8.7 またはそれ以上で動作します。"))
  (html:ul
   (html:li (en/ja (list "It requires the " /GD/ "2 library (version 2.0.28 or higher) which has been installed.")
                   (list "また別途 " /GD/ "2 ライブラリ(バージョン 2.0.28 以上)がインストールされている必要があります。"))))

  (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "License" "ライセンス"))
  (html:p "The BSD License")

  (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Download" "ダウンロード"))
  (*package-download*)

  (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Documentation" "文書"))
  (html:ul
   (html:li (html:a :href (en/ja "reference.en.html" "reference.ja.html")
                    "Gauche-gd " (en/ja "Reference Manual" "リファレンスマニュアル"))))

  (html:h2 :style "border-bottom: 1px solid #bbbbbb;" "FYI")
  (html:ul
   (html:li /GD/)
   (html:li (html:a :href "http://php.net/image" "PHP: Image Functions"))
   (html:li /GD.pm/)
   (html:li (html:a :href "http://newcenturycomputers.net/projects/gdmodule.html" "gdmodule - Python GD module"))
   (html:li (html:a :href "http://raa.ruby-lang.org/project/ruby-gd/" "ruby-gd"))
   (html:li /GD/Scheme/)
   (html:li /CL-GD/)
   (en/ja '()
          (html:li (html:a :href "http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3a%e6%8b%a1%e5%bc%b5%e3%83%a9%e3%82%a4%e3%83%96%e3%83%a9%e3%83%aa%e5%85%a5%e9%96%80" "Gauche:拡張ライブラリ入門")))
   ))

(define main package-main)
