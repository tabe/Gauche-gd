#!/usr/bin/env gosh

(use graphics.gd)
(use srfi-1) ;; append-map

(define (main args)
  (let* ((im (gd-image-create-true-color 640 480))
 		 (white (color-resolve! im #xff #xff #xff)))
	(rectangle! im 0 0 640 480 white :filled #t)
	(let ((style (append-map
				  (lambda (x)
					(let ((c (color-resolve! im (car x) (cadr x) (caddr x))))
					  (make-list 3 c)))
				  '((#x00 #xff #x00)
					(#x33 #xcc #x33)
					(#x66 #x99 #x66)
					(#x99 #x66 #x99)
					(#xcc #x33 #xcc)
					(#xff #x00 #xff)
					))))
	  (set-style! im style))
	(set-thickness! im 10)
	(line! im 20 240 600 240 gdStyled)
	(save-as im "dashed.gif")
 	)
  0)
