#lang racket
;; rosalind #33
;; Matching Random Motifs 
;; [RSTR] 2012/10/13 AC 

(require srfi/1)
(require srfi/13)
(require math/number-theory)
(require "readfileA.ss")
;; (define *rstr_out* "data\\rstr_out.txt")

(define (ros_rstr . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_rstr.txt"
		    (format "data\\rs_rstr~a.txt" (car n)))))
	 (pair  (string-tokenize (car data)))
	 (n (string->number (car pair)))
	 (x (string->number (cadr pair)))
	 (dna (string->list (cadr data)))
	 (a-count (length (indexes-of dna #\A)))
	 (c-count (length (indexes-of dna #\C)))
	 (g-count (length (indexes-of dna #\G)))
	 (t-count (length (indexes-of dna #\T)))
         (p (* (expt (/ x 2)     (+ c-count g-count))
	       (expt (/(- 1 x)2) (+ a-count t-count))))
	 )

         
         (- 1 (expt (- 1 p) n))
    ))
    
