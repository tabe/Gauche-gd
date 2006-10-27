#!/usr/bin/env gosh

(use graphics.gd)

(define (spiral0 im n color)
  (let lp ((i n))
	(cond ((= i 0)
		   (quote ()))
		  (else
		   (gd-image-arc im
						 (- 320 (if (even? i) 0 1))
						 240
						 (* i 2)
						 (* i 2)
						 (if (even? i) 0 180)
						 (if (even? i) 180 360)
						 color)
		   (lp (- i 1))))))

(define (main args)
  (let* ((im (gd-image-create 640 480))
		 (white (gd-image-color-allocate im #xff #xff #xff))
		 (blue (gd-image-color-allocate im #x00 #x00 #xff)))
	(gd-image-filled-rectangle im 0 0 640 480 white)
 	(spiral0 im 200 blue)
	(save-as im (if (memq 'png *gd-features*)
					"spiral0.png"
					"spiral0.gif"))
	)
  0)
