;;#lang racket
;; rosalind
;; 	Convert a Peptide into a Peptide Vector
;; 
;; [BA11C] 2012/07/05 AC
(require srfi/1)
(require srfi/13)
(require srfi/48)

(include "readfile.ss")
(include "roslib.ss")
(include "monoisotopic.ss")
(define *ba11c_out* "ba11c_out.txt")
(define *result* #f)

(define *data* '())
(define amino-mass (append '((X 4)(Z 5)) monoiso-mass))
(define amino (map (lambda(pair)
		     
		     (cons (string-ref (symbol->string (car pair)) 0)
			   (list(floor(cadr pair)))))
		   amino-mass))
  
  

(define (ros_ba11c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba11c.txt"
		    (format "rs_ba11c~a.txt" (car n)))))
	 (result
	  (append-map  num2vect
		       (map char2weight
			    (string->list  (car data))))))
	 
	 (call-with-output-file *ba11c_out*
	   (lambda(out)
	     (displaylist result out ))
	   #:exists 'truncate/replace)
	 (set! *result* result)
	 #t
 ))

(define (char2weight char)
  (inexact->exact (floor (cadr (assoc char amino)))))
  
(define (num2vect num)
  (append (make-list (- num 1) 0) '(1)))
