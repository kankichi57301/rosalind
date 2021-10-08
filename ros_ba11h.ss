#lang racket
;; rosalind
;; Compute the Size of a Spectral Dictionary
;; [BA11H] 20**/**/**
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "monoisotopicA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba11h_out* "ba11h_out.txt")

(define (ros_ba11h . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba11h.txt"
		    (format "rs_ba11h~a.txt" (car n)))))
	 (spect (map string->number (string-tokenize (car data))))
	 (thres (string->number (cadr data)))
	 (max_sc (string->number (caddr data)))
	 )
    spect
    #|
    (call-with-output-file *****_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))

(define my-hash (make-hash))
;;--*--
;;(define sp '(4 -3 -2 3 3 -4 5 -3 -1 -1 3 4 1 3))
;;(peptide-score sp "XZ" #t)
;;(map (lambda(s)(peptide-score sp s #t)) '("X" "Z" "XX" "XZ" "ZZ" "XXX" "XXZ" "XZZ" "ZZZ"))

(define (make-spect-dict0 spect spect-len prot-acc weight-acc score min max wt-table)
  ;(displayln (format "prot=~a weight=~a score=~a" prot-acc weight-acc score))
	  (when (<= min score max)
		(hash-set! my-hash prot-acc score))
	  (for-each (lambda(a)
		      (let ((wt (cdr (assoc a wt-table))))
			(when (< wt spect-len)
			      (make-spect-dict0 (drop spect wt)(- spect-len wt)(append prot-acc (list a))
						(+ weight-acc wt)
						(+ score (list-ref spect  (- wt 1)))
						min max wt-table))))
		    (map car wt-table)))

(define (make-spect-dict spect min max)
  (make-spect-dict0 spect (length spect) '() 0 0 min max '((#\X . 4)(#\Z . 5)))
  (hash-map my-hash list))
		    

;;(make-spect-dict sp 1 8)
