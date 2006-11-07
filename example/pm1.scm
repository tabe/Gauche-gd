#!/usr/bin/env gosh

(use graphics.gd)

;; a gauche-gd port of http://search.cpan.org/dist/GD/GD.pm#SYNOPSIS

(define (main args)
  (let ((im (gd-image-create 100 100)))
	(let ((white (color-allocate! im 255 255 255))
		  (black (color-allocate! im 0 0 0))
		  (red   (color-allocate! im 255 0 0))
		  (blue  (color-allocate! im 0 0 255)))
	  (color-transparent! im white)
	  (interlace! im 1)

	  (rectangle! im 0 0 99 99 black)
	  (arc! im 50 50 95 75 0 360 blue)

	  (fill! im 50 50 red)

	  (current-gd-image-format (if (memq 'png *gd-features*) 'png 'gif))
	  (write im)
	  ))
  0)
