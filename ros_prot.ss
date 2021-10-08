#lang racket
;; rosalind
;; 
;; [PROT] 2021/**/**
;(require srfi/1)
(require (except-in srfi/13 string-replace))
(require "readfileA.ss")
(require "monoisotopicA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *prot_out* "prot_out.txt")

(define (ros_prot . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_prot.txt"
		    (format "rs_prot~a.txt" (car n)))))
	 (res "")
	 (res2 "")
	 )
    (set! res (rna->prot (car data)))
    (set! res2
	  (if (string=? "<" (string-take-right res 1))
	      (string-drop-right res 1)
	      res))

    (call-with-output-file *prot_out*
      (lambda(out)
	(displayln  res2 out))
      #:exists 'truncate/replace)

    res2
))

(define (rna->prot dnastr)
  (if (< (string-length dnastr) 3)
      ""
      (format "~a~a"
	      (cadr (assoc (string-replace (string-take dnastr 3) "U" "T") codon-table))
	      (rna->prot (string-drop dnastr 3)))))

