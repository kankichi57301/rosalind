#lang racket
;; rosalind
;; Calculating Expected Offspring
;; [] 2021/10/09
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *iev_out* "data\\iev_out.txt")

(define (ros_iev . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_iev.txt"
		    (format "data\\rs_iev~a.txt" (car n)))))
	 (res 0.0)
	 )
    (set! res
	  (apply solve-iev
		 (map string->number (string-tokenize (car data)))))
    
    (call-with-output-file *iev_out*
      (lambda(out)
	(display  res out))
      #:exists 'truncate/replace)
    res
    
))

(define (solve-iev a b c d e f)
  (+ (* 2.0 (+ a b c))(* 1.5 d) e))
