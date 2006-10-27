#!/usr/bin/env gosh

(use graphics.gd)

(define (spiral1 im n color)
  (let lp ((i n))
	(cond ((= i 0)
		   (quote ()))
		  (else
		   (gd-image-arc im
						 (- 320 (if (even? i) 0 2))
						 240
						 (* i 4)
						 (* i 4)
						 (if (even? i) 0 180)
						 (if (even? i) 180 360)
						 color)
		   (lp (- i 1))))))

(define (main args)
  (let* ((im (gd-image-create 640 480))
		 (white (gd-image-color-allocate im #xff #xff #xff))
		 (blue (gd-image-color-allocate im #x00 #x00 #xff)))
	(gd-image-filled-rectangle im 0 0 640 480 white)
 	(spiral1 im 100 blue)
	(save-as im (if (memq 'png *gd-features*)
					"spiral1.png"
					"spiral1.gif"))
	)
  0)
