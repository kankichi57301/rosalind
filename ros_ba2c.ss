#lang racket
;; rosalind
;; Find a Profile-most Probable k-mer in a String
;; [BA2C] 2021/07/10 AC 
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(define *ba2c_out* "data\\ba2c_out.txt")

(define (ros_ba2c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba2c.txt"
		    (format "data\\rs_ba2c~a.txt" (car n)))))
	 (dnalist (string->list (car data)))
	 (dnalen (length dnalist))
	 (k (string->number (cadr data)))
	 (p-mat (transpose (map (lambda(x)(map string->number(string-tokenize x)))(cddr data))))
	 (res "")
	 )
    
    (define (dna2prob dnalist)
      (apply * (map (lambda(i)(list-ref (list-ref p-mat i)(nuc2num (list-ref dnalist i))))
		    (iota k))))

    (set! res
	  (car
	   (max-item
	    (map (lambda(i)(list (apply string (take (drop dnalist i) k))
				 (dna2prob (drop dnalist i))))
		 (iota (- dnalen k -1)))
	    cadr)))
    
    (call-with-output-file *ba2c_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)
    
    res
    ))

				

