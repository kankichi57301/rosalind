#lang racket
;; rosalind
;; 
;; [INOD] 2021/10/09
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *inod_out* "data\\inod_out.txt")

(define (ros_inod . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_inod.txt"
		    (format "data\\rs_inod~a.txt" (car n)))))
	 )
    (- (string->number(car data)) 2)
    #|
    (call-with-output-file *inod_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a " x) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))

