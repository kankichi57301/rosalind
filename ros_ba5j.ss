#lang racket
;; rosalind
;; Align Two Strings Using Affine Gap Penalties
;; [BA5J] 2021/11/09 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "align-affineA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba5j_out* "data\\ba5j_out.txt")

(define (ros_ba5j . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba5j.txt"
		    (format "data\\rs_ba5j~a.txt" (car n)))))
	 (res (apply pair-align-affine data))
	 )
    
    
    (call-with-output-file *ba5j_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln (format "~a" x) out))
		  res))
      #:exists 'truncate/replace)
    
    res
    ))



