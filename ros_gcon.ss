#lang racket
;; rosalind
;; Global Alignment with Constant Gap Penalty
;; 2021/02/13 AC
;; 2021/10/21 AC
;; [GCON]
(require srfi/13)
(require "readfileA.ss")
(require (only-in "align2A.ss" align-gcon)) ;; constant gap penelty
(define *gcon_out* "data\\gcon_out.txt")

(define (ros_gcon . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_gcon.txt"
		    (format "data\\rs_gcon~a.txt" (car n)))))
	 (dnas (edit-fasta data))
	 (res 0)
	 )
    (set! res (apply align-gcon dnas))
    (call-with-output-file *gcon_out*
      (lambda(out)
	  (displayln res out))
      #:exists 'truncate/replace)
    res
   ))


