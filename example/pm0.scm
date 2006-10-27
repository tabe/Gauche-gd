#!/usr/bin/env gosh

(use graphics.gd)

;; a gauche-gd port of http://search.cpan.org/dist/GD/GD.pm#SYNOPSIS

(define (main args)
  (let ((im (gd-image-create 100 100)))
	(let ((white (gd-image-color-allocate im 255 255 255))
		  (black (gd-image-color-allocate im 0 0 0))
		  (red   (gd-image-color-allocate im 255 0 0))
		  (blue  (gd-image-color-allocate im 0 0 255)))
	  (gd-image-color-transparent im white)
	  (gd-image-interlace im 1)

	  (gd-image-rectangle im 0 0 99 99 black)
	  (gd-image-arc im 50 50 95 75 0 360 blue)

	  (gd-image-fill im 50 50 red)

	  (save-as im (if (memq 'png *gd-features*)
					  "pm0.png"
					  "pm0.gif"))
	  ))
  0)
