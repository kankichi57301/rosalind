#lang racket
;;rosalind
;;Find a Highest-Scoring Multiple Sequence Alignment 
;;[BA5M]
;;2021/11/06 AC
(require "readfileA.ss")
(require (only-in "align9A.ss" m-a-3d dump-hash))
(define *ba5m_out* "data\\ba5m_out.txt")


(define (ros_ba5m . n)
  (let* ((dnas (read-file*
		(if (null? n)
		    "data\\rosalind_ba5m.txt"
		    (format "data\\rs_ba5m~a.txt" (car n)))))
	 (dnalist (apply m-a-3d dnas))
	 (dnalist2(cons (- (car dnalist))(cdr dnalist)))
	 )
   
    
    (call-with-output-file *ba5m_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln x out))
		  dnalist2))
      #:exists 'truncate/replace)
    
    dnalist
    ))
