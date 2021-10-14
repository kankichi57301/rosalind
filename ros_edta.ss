#lang racket
;; rosalind
;; Edit Distance Alignment 
;; [EDTA]
;; 2012/10/14 AC
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(require "editdist2A.ss")

(define *edta_out* "data\\edta_out.txt")

(define (ros_edta . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_edta.txt"
		    (format "data\\rs_edta~a.txt" (car n)))))
	 (dnas (edit-fasta data))
	 (res '())
	 )
    (set! res (apply edit-align dnas))
    
    (call-with-output-file *edta_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a~%" x) out))
		  res))
      #:exists 'truncate/replace)
    res
   ))

