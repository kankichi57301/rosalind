#lang racket
;; rosalind
;; Counting Unrooted Binary Trees
;; [CUNR]
;; 2021/01/17 WA
;; 2021/10/15 AC
(require math/number-theory)
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")

(define (ros_cunr . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_cunr.txt"
		    (format "data\\rs_cunr~a.txt" (car n))))))
    (modulo (count-unrooted-tree (string->number (car data)))
	    (expt 10 6))))
				 

(define (count-unrooted-tree n)
  (double-fact (- (* 2 n) 5)))

(define (double-fact n)
  (if (< n 2)
      1
      (* n (double-fact (- n 2)))))
  
