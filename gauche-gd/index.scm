#!/usr/bin/env gosh
;; -*- encoding: euc-jp -*-

(use file.util)
(use fixedpoint.site)
(use text.html-lite)
(use text.tree)

(//
 (GD.pm "http://search.cpan.org/dist/GD/")
 (GD/Scheme "http://www196.pair.com/lisovsky/scheme/gd/index.html")
 (CL-GD "http://weitz.de/cl-gd/")
 (FreeType "http://www.freetype.org/")
 )

(define *last-update* "Thu Feb 07 2007")
(define *gauche-gd-version* (file->string "../VERSION"))
(define *gauche-gd-tarball-basename* (string-append "Gauche-gd-" *gauche-gd-version* ".tgz"))
(define *gauche-gd-tarball-size* (file-size (string-append "../../" *gauche-gd-tarball-basename*)))
(define *gauche-gd-tarball-url* *gauche-gd-tarball-basename*)

(define (index lang)
  (let-syntax ((en/ja (syntax-rules ()
						((_ en ja)
						 (if (string=? "en" lang) en ja)))))
	((fixedpoint:frame "Gauche-gd")
	 (html:p :id "lang_navi" (html:a :href (en/ja "index.html" "index.en.html")
										"[" (en/ja "Japanese" "English") "]"))
	 (html:p :id "last_update" "Last update: " *last-update*)
	 (fixedpoint:separator)
	 (fixedpoint:adsense)
	 (fixedpoint:separator)
	 (html:p (html:dfn /Gauche-gd/)
			 (en/ja
				 (list " is an extension package of " /Gauche/ " which provides a binding of Thomas Boutell's " /GD/ "2 library.")
				 (list " �� " /Scheme/ " ������ " /Gauche/ " �� " /GD/ "2 �饤�֥������Ѥ��뤿��γ�ĥ�ѥå������Ǥ���")))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "News" "�ǿ�����"))
	 (html:ul
	  (html:li "[2007-02-07] " (en/ja "Congratulation! GD 2.0.34 released."
									  "GD 2.0.34 ����꡼������ޤ�����"))
	  (html:li "[2007-01-18] " (en/ja "It is confirmed that the current version 0.1.4 runs on Gauche 0.8.9."
									  "Gauche 0.8.9 �Ǹ��ߤΥС������ 0.1.4 ��ư��뤳�Ȥ��ǧ���ޤ�����"))
	  (html:li "[2006-12-11] " (en/ja "Release 0.1.4. Some bugs fixed and some options added."
									  "�С������ 0.1.4 ��������ޤ����������Ĥ��ΥХ������������ץ������ɲä��ޤ�����"))
	  (html:li "[2006-11-27] " (en/ja "Release 0.1.3. API for GIF Animations available."
									  "�С������ 0.1.3 ��������ޤ�����GIF ���˥᡼�����򥵥ݡ��Ȥ��ޤ�����"))
	  (html:li "[2006-11-16] " (en/ja "It is confirmed that the current version 0.1.2 runs on Gauche 0.8.8."
									  "Gauche 0.8.8 �Ǹ��ߤΥС������ 0.1.2 ��ư��뤳�Ȥ��ǧ���ޤ�����"))
	  (html:li "[2006-11-07] " (en/ja "Release 0.1.2." "�С������ 0.1.2 ��������ޤ�����"))
	  (html:li "[2006-11-03] " (en/ja "Release 0.1.1." "�С������ 0.1.1 ��������ޤ�����"))
	  (html:li "[2006-10-28] " (en/ja "Some links added." "��󥯤��ɲä��ޤ�����"))
	  (html:li "[2006-10-26] " (en/ja "Release 0.1.0." "�С������ 0.1.0 ��������ޤ�����")))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Features" "��ħ"))
	 (html:ul
	  (html:li (en/ja "Through a straitforward API you can call a C function or macro directly."
					  "�б������³���ˤ�ä�ľ�� C �δؿ���ޥ����Ƥ֤��Ȥ��Ǥ��ޤ���"))
	  (html:li (en/ja "A simpler interface (experimental):"
					  "���٥�ʥ��󥿡��ե�����(��ȯ��):")
			   (html:ul
				(html:li (en/ja (list "Printing a Scheme string containing possible multibyte characters with a " /FreeType/ " font")
								(list /FreeType/ " �ե���Ȥˤ��ޥ���Х���ʸ����ޤ� Scheme ʸ�����ɽ��")))
				(html:li (en/ja "I/O through gdIOCtx compatible ports"
								"gdIOCtx �ߴ��� port �ˤ��������"))
				(html:li (en/ja "Generating GIF Animations"
								"GIF ���˥᡼����������")))
				))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Requirements" "Ƴ��"))
	 (html:p (en/ja "This package is for Gauche 0.8.7 or later."
					"���Υѥå������� Gauche 0.8.7 �ޤ��Ϥ���ʾ��ư��ޤ���"))
	 (html:ul
	  (html:li (en/ja (list "It requires the " /GD/ "2 library (version 2.0.28 or higher) which has been installed.")
					  (list "�ޤ����� " /GD/ "2 �饤�֥��(�С������ 2.0.28 �ʾ�)�����󥹥ȡ��뤵��Ƥ���ɬ�פ�����ޤ���"))))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Download" "���������"))
	 (html:p (html:a :href *gauche-gd-tarball-url*
					 *gauche-gd-tarball-basename* " (" *gauche-gd-tarball-size*  " bytes)"))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Documents" "ʸ��"))
	 (html:ul
	  (html:li (html:a :href (en/ja "reference.en.html" "reference.ja.html")
					   "Gauche-gd " (en/ja "Reference Manual" "��ե���󥹥ޥ˥奢��"))))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Links" "���"))
	 (html:ul
	  (html:li /GD/)
	  (html:li /GD.pm/)
	  (html:li (html:a :href "http://php.net/image" "PHP: Image Functions"))
	  (html:li (html:a :href "http://newcenturycomputers.net/projects/gdmodule.html" "gdmodule - Python GD module"))
	  (html:li (html:a :href "http://raa.ruby-lang.org/project/ruby-gd/" "ruby-gd"))
	  (html:li /GD/Scheme/)
      (html:li /CL-GD/)
	  (en/ja '()
			 (html:li (html:a :href "http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3a%e6%8b%a1%e5%bc%b5%e3%83%a9%e3%82%a4%e3%83%96%e3%83%a9%e3%83%aa%e5%85%a5%e9%96%80" "Gauche:��ĥ�饤�֥������")))
	  )
	 )))

(define (main args)
  (define (usage)
	(format (current-error-port) "usage: gosh ~a (en|ja)\n" *program-name*)
	(exit 1))
  (when (< (length args) 2)
	(usage))
  (write-tree (index (cadr args)))
  0)
