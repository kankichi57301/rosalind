#lang racket
;; rosalind
;; Finding a Spliced Motif
;; [SSEQ]
(require srfi/13)
(require "readfileA.ss")
(require "monoisotopicA.ss")
(define *sseq_out* "data\\sseq_out.txt")

(define (get-subseq-indecies-0 str1 str2 from)
  (if (not (non-empty-string? str2))
      '()
      (let ((pos (string-index str1 (string-ref str2 0) (+ from 1))))
	(if  pos
	     (cons (+ 1 pos)
		   (get-subseq-indecies-0 str1 (string-drop str2 1) pos))
	    '(0)))))

(define (get-subseq-indecies str1 str2)
  (get-subseq-indecies-0 str1 str2 0))

(define (ros_sseq . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_sseq.txt"
		    (format "data\\rs_sseq~a.txt" (car n)))))
	 (dnas (edit-fasta data))
	 (res '())
	 )
    (set! res (get-subseq-indecies (car dnas)(cadr dnas)))
    
    (call-with-output-file *sseq_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a " x) out))
		  res))
      #:exists 'truncate/replace)
    
    res
))
