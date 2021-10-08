#lang racket
;; rosalind
;; Counting DNA Nucleotides
;; [DNA] 2021/10/07
;; @kankichi57301

(require srfi/13)
(require "readfileA.ss")
(define *dna_out* "data\\dna_out.txt")

(define (ros_dna . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_dna.txt"
		    (format "data\\rs_dna~a.txt" (car n)))))
	 (res '())
	 )
    (set! res 
	  (map (lambda(x)(count (lambda(n)(equal? n x))(string->list (car data)))) '(#\A #\C #\G #\T)))
    
    (call-with-output-file *dna_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a " x) out))
		  res))
      #:exists 'truncate/replace)
    res
))



