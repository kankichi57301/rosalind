#lang racket
;; rosalind
;; 
;; [FIB] 20**/**/**
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *fib_out* "fib_out.txt")
(define *memo* '())

(define (ros_fib . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_fib.txt"
		    (format "rs_fib~a.txt" (car n)))))
	 (1stline (string-tokenize (car data)))
	 (n (string->number (car  1stline)))
	 (k (string->number (cadr 1stline)))
	 )
    (set! *memo* (make-hash))
    (rabbit n k)
    #|
    (call-with-output-file *fib_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))


(define (rabbit month offspring)
  (let ((ans (hash-ref *memo* month 0)))
    (if (positive? ans)
	ans
	(let ((ret 
	       (if (= 1 month)
		   1
		   (if (= 2 month)
		       offspring
		       (let ((gen1 (rabbit (- month 1) offspring))
			     (gen2 (rabbit (- month 2) offspring)))
			 (if (<= month 4)
			     (+ gen1 gen2)
			     (+ gen1 (* gen2 offspring))))))))
	  (hash-set! *memo* month ret)
	  ret))))
