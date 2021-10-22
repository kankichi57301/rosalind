#lang racket

(require "roslibA.ss")

(define (all-cycle-kmers str k)
  (let ((dbl (string-append str str)))
    (map (lambda(x)(string-take (string-drop x dbl) k))
	 (iota (string-length str)))))

(define dat01 "GATTACA")
  
