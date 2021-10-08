#lang racket
;; rosalind
;; 
;; [FIB] 20**/**/**
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *fib_out* "data\\fib_out.txt")
(define *memo* '())

(define (ros_fib . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_fib.txt"
		    (format "data\\rs_fib~a.txt" (car n)))))
	 (1stline (string-tokenize (car data)))
	 (n (string->number (car  1stline)))
	 (k (string->number (cadr 1stline)))
	 (res '())
	 )
    (set! *memo* (make-hash))
    (set! res (rabbit n k))
    
    (call-with-output-file *fib_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)
    
    res
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
