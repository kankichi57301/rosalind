#lang racket
;; rosalind
;; Assessing Assembly Quality with N50 and N75
;; [ASMQ]
;; 2021/03/04 AC
;; 2021/10/21 AC
(require srfi/13)
(require "readfileA.ss")


(define (findpos sorted n)
  (if (null? sorted)
      '()
      (if (<= n (car sorted))
	  (car sorted)
	  (findpos (cdr sorted)(- n (car sorted))))))
      

(define (ros_asmq . n)
  (let* ((dnas (read-file*
		(if (null? n)
		    "data\\rosalind_asmq.txt"
		    (format "data\\rs_asmq~a.txt" (car n)))))
	 (lenlist (map string-length dnas))
	 (sortedlenlist (sort lenlist <))
	 (sumlen  (apply + lenlist))
	 (n50pos (inexact->exact (floor (/  sumlen 2.0))))
	 (n75pos (inexact->exact (floor (/  sumlen 4.0))))
	)
    ;(list sortedlenlist n50pos n75pos)
    (display (format "~a " (findpos sortedlenlist n50pos)))
    (display (format "~a~%" (findpos sortedlenlist n75pos)))
   ))
