#lang racket
;; rosalind
;; Counting Optimal Alignments
;;[CTEA] 2021/02/28 AC
;; 2021/10/14 AC
(require srfi/13)
(require "readfileA.ss")
(require "align-nA.ss")
(define *ros_ctea* "rosalind_ctea.txt")
(define *ros_ctea0* "rs_ctea.txt")
(define *ros_ctea1* "rs_ctea1.txt")
(define *ctea-out* "rs_ctea_out.txt")

(define (ros_ctea . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ctea.txt"
		    (format "data\\rs_ctea~a.txt" (car n)))))
	 (data2 (edit-fasta data))
	 (res (apply edit-align data2)))

    (modulo (cadr res) 134217727)
   ))
