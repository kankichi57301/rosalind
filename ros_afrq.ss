#lang racket
;; rosalind
;; Counting Disease Carriers
;; [AFRQ] 2021/10/13 AC
(require srfi/13)
(require "readfileA.ss")


(define (ros_afrq . n)
  (let* ((line (read-file*
		(if (null? n)
		    "data\\rosalind_afrq.txt"
		    (format "data\\rs_afrq~a.txt" (car n)))))
	 (data (map string->number (regexp-match* #rx"[0-9]+[.][0-9]+" (car  line)))))
    ;data
    
    (for-each (lambda(x)(display (format "~s " x)))
    (map carrier-percentage data))
    
   ))

(define (carrier-percentage homo-ratio)
  (/ (round (* 10000
	       (- 1.0 (expt (- 1.0 (sqrt homo-ratio)) 2)))) 10000))
