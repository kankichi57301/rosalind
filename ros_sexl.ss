#lang racket
;; rosalind
;; Sex-Linked Inheritance
;; [SEXL]
;; 2021/10/21 AC
(require srfi/13)
(require "readfileA.ss")

(define (ros_sexl . n)
  (let* ((data0 (read-file*
		(if (null? n)
		    "data\\rosalind_sexl.txt"
		    (format "data\\rs_sexl~a.txt" (car n)))))
	 (data (map string->number (string-tokenize (car data0))))
	 )
    ;data
    
    (for-each (lambda(x)(display (format "~s "  x)))
	 (map famale-carrier data))
   ))

(define (famale-carrier x)
  (/ (round (* 10000 (* 2 x (- 1 x)))) 10000))
