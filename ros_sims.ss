#lang racket
;; rosalind
;; Finding a Motif with Modifications
;; [SIMS] 2021/03/08 AC
;; 2012/10/23 AC
(require srfi/13)
(require "readfileA.ss")
(require "align-fittingA.ss")

(define *sims-out*  "data\\sims_out.txt")

(define (ros_sims . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_sims.txt"
		    (format "data\\rs_sims~a.txt" (car n)))))
	 (dnas (edit-fasta data))
	 (res '())
	 )
    
    (set! res (apply fitting-align dnas))
    (call-with-output-file *sims-out*
      (lambda(out)
	(for-each (lambda(x)(displayln (format "~a" x) out))
		  (apply fitting-align dnas)))
      #:exists 'truncate/replace)
    res

))
