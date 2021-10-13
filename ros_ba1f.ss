#lang racket
;; rosalind
;; Find a Position in a Genome Minimizing the Skew
;; [BA1F] 2021/07/08 AC
;; 2021/10/13 AC 
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(define *ba1f_out* "data\\ba1f_out.txt")

(define (ros_ba1f . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba1f.txt"
		    (format "data\\rs_ba1f~a.txt" (car n)))))
	 (dna (string->list(car data)))
	 (skewed (skew dna))
	 (minval (apply min skewed))
         (res (filter (lambda(x)(= minval (list-ref skewed x)))
		      (iota (length dna)))))

        (call-with-output-file *ba1f_out*
	  (lambda(out)
	    (for-each
	     (lambda(str)
	       (display (format "~a " str) out))
	     res))
	  #:exists 'truncate/replace)
	res
	))

(define (skew dnalist)
  (reverse (skew0 dnalist '(0) 0)))

(define (skew0 dnalist ans acc)
  (if (null? dnalist)
      ans
      (let ((next (+
	     (case (car dnalist)
	       [(#\C) -1]
	       [(#\G)  1]
	       [else   0]) acc)))
	
	(skew0 (cdr dnalist)(cons next ans) next))))

;; --*-- test prog --*--
;; (skew (string->list "CATGGGCATCGGCCATACGCC"))

