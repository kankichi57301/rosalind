#lang racket
;; rosalind
;; Inferring Protein from Spectrum 
;; [SPEC]

(require "readfileA.ss")
(require "monoisotopicA.ss")


(define (ros_spec . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_spec.txt"
		    (format "data\\rs_spec~a.txt" (car n)))))
	 (data1 (map string->number data))
	 )
    
     
    (displayln (format "~a"
		       (apply string-append
			      (map (lambda(x)(symbol->string
					      (weight->amino x))) (map - (cdr data1) (drop-right data1 1))))))
    
   ))


