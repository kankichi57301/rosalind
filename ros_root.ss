#lang racket
;; rosalind
;; Counting Rooted Binary Trees
;; [ROOT]
;;2021/10/21 AC
(require srfi/13)
(require "readfileA.ss")


(define (ros_root . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_root.txt"
		    (format "data\\rs_root~a.txt" (car n)))))
	 (nnodes (string->number (car data)))
	 )
    (inexact->exact (modulo (solve_root nnodes) 1000000))

    ))


(define (solve_root n)
  (double-fact (- (* 2 n) 3)))

(define (double-fact n)
  (if (< n 2)
      1
      (* n (double-fact (- n 2)))))
