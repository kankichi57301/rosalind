#lang racket
;; rosalind
;; Introduction to Random Strings 
;; [PROB] 2021/10/09
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(define *prob_out* "data\\prob_out.txt")

(define (ros_prob . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_prob.txt"
		    (format "data\\rs_prob~a.txt" (car n)))))
	 (dna (car data))
	 (probabilities (map string->number (string-tokenize (cadr data))))
	 (nuc-count 
	  (map (lambda(x)(count (lambda(n)(equal? n x))(string->list dna))) '(#\A #\C #\G #\T)))
	 (ATcount (+ (car nuc-count)(cadddr nuc-count)))
	 (GCcount (+ (cadr nuc-count)(caddr nuc-count)))
	 (res '())
	 )

    (set! res
	  (map (lambda(p)(roundp3 (log10 (* (expt (/ p 2) GCcount)(expt (/ (- 1.0 p) 2) ATcount)))))
	       probabilities))
    
    (call-with-output-file *prob_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a " x) out))
		  res))
      #:exists 'truncate/replace)
    res
))

(define (log10 x)
  (/ (log x)(log 10)))
