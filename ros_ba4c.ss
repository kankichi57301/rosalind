#lang racket
;;rosalind
;;Generate the Theoretical Spectrum of a Cyclic Peptide
;;[BA4C] 2021/07/10 AC
;; 2021/10/17 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "monoisotopicA.ss")
(define *ba4c_out* "data\\ba4c_out.txt")

(define (ros_ba4c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba4c.txt"
		    (format "data\\rs_ba4c~a.txt" (car n)))))
	 (peptide (car data))
	 (double  (string-append peptide peptide))
	 (all-weight (peptide-weight-int peptide))
	 (len (string-length peptide))
	 (res (cons 0 (append (cyclic-weight double len)(list all-weight))))
       )
    
    (call-with-output-file *ba4c_out*
      (lambda(out)
	(for-each (lambda(num)
		    (display (format "~a " num) out))
		  res))
      #:exists 'truncate/replace)
    res
))

(define (cyclic-weight double n)
  (sort
   (append-map (lambda(start)
		 (map (lambda(len)(peptide-weight-int (substring double start (+ start len))))
		      (iota (- n 1) 1)))
	       (iota n))
  <))
		
				

