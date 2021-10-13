#lang racket
;;
;;Independent Segregation of Chromosomes
;;[INDC]
;;
;;
(require srfi/1)
(require math/number-theory)
(require "readfileA.ss")
(require "roslibA.ss")
(define *indc_out* "data\\indc_out.txt")

(define (bino-GT n k)
  (apply + (map (lambda(x)(binomial n x))(iota k))))

(define (ros_indc . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_indc.txt"
		    (format "data\\rs_indc~a.txt" (car n)))))
	 (n (* 2 (string->number (car data)))))
    
    (call-with-output-file *indc_out*
	(lambda (out)
	  (for-each (lambda(x)(display (format "~a " x) out))
		    (map (lambda(x)
			   (roundp3 (log10 (/ (bino-GT n (- n x))(expt 2 n)))))
			 (iota n))))
	  #:exists 'truncate/replace)
    
    
    
))
