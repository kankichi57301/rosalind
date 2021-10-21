#lang racket
;;
;;filename:ros_mult.ss
;;[MULT]
;;2021/02/23 AC
;;2021/10/21 AC
(require "readfileA.ss")
(require (only-in "align8A.ss" m-a-4d))
(define *mult_out* "data\\mult_out.txt")


(define (ros_mult . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_mult.txt"
		    (format "data\\rs_mult~a.txt" (car n)))))
	 (dnas (edit-fasta data))
	 (dnalist (apply m-a-4d dnas))
	 (dnalist2(cons (- (car dnalist))(cdr dnalist)))
	 )
    (call-with-output-file *mult_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln x out))
		  dnalist2))
      #:exists 'truncate/replace)
    dnalist))
