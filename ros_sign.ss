#lang racket
;; rosalind
;; Enumerating Oriented Gene Orderings
;; [SIGN] 2021/10/10
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(require  (only-in math/number-theory factorial))
;;(require "roslibB.ss")
(define *sign_out* "data\\sign_out.txt")

(define (ros_sign . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_sign.txt"
		    (format "data\\rs_sign~a.txt" (car n)))))
	 (n (string->number (car data)))
	 (res '())
	 )
    (set! res (solve-sign n))
    
    (call-with-output-file *sign_out*
      (lambda(out)
	(displayln (* (expt 2 n)(factorial n)) out)
	(for-each (lambda(x)
		    (disp-list x  out))
		  res))
      #:exists 'truncate/replace)
    
    
))

(define (solve-sign n)
  (append-map (lambda(x)
		(map (lambda(y)
		       (map * x y))
		     (cartesian-expt '(1 -1) n)))
	      (permutations (iota n 1))))

(define (disp-list list out)
  (for-each (lambda(x)(display (format "~a " x) out)) list)
  (display "\n" out))
