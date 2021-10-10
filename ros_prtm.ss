#lang racket
;;rosalind
;;Calculating Protein Mass
;;[PRTM]

(require srfi/1)
(require "monoisotopicA.ss")
(require "readfileA.ss")
(define *prtm_out* "data\\prtm_out.txt")


(define (ros_prtm . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_prtm.txt"
		    (format "data\\rs_prtm~a.txt" (car n)))))
	 (res '())
	 )
    (set! res (peptide-weight (car data)))
    
    (call-with-output-file *prtm_out*
      (lambda(out)
		    (display res out))
      #:exists 'truncate/replace)
    res
))
