#lang racket
;; rosalind
;; Counting Quartets
;; [CNTQ]
;; 2021/10/15 AC
(require srfi/1)
(require "readfileA.ss")

(define *cntq_out* "data\\cntq_out.txt")


(define (ros_cntq . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_cntq.txt"
		    (format "data\\rs_cntq~a.txt" (car n)))))
	 (nnodes (string->number (car data)))
	 )
    (inexact->exact (modulo (solve_cntq nnodes) 1000000))

    ))


(define (solve_cntq n)
  (apply + (map (lambda(k)(* (+ k 1)(- n k 3)(- n k 2) 0.5))(iota (- n 1)))))
