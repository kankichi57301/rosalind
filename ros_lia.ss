#lang racket
;; rosalind
;; Independent Alleles
;; [LIA] 2021/10/09
;(require srfi/1)
(require srfi/13)
(require math/number-theory)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *lia_out* "data\\lia_out.txt")

(define (ros_lia . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_lia.txt"
		    (format "data\\rs_lia~a.txt" (car n)))))
	 (res 0.0)
	 )
    (set! res
	  (apply solve-lia
		 (map string->number (string-tokenize (car data)))))
    
    (call-with-output-file *lia_out*
      (lambda(out)
	(display  res out))
      #:exists 'truncate/replace)
    
    res
))

(define (solve-lia N k)
  (calc-bino-ge (expt 2 N) k 0.25))

(define (calc-bino-ge n k p)
  (apply +
	 (map (lambda(i)
		(* (binomial n i)
		   (expt p i)
		   (expt (- 1.0 p)(- n i))))
	      (range k (+ 1 n)))))
