#lang racket
;; rosalind 
;; Partial Permutations
;; [PPER]

(require srfi/1)
(require math/number-theory)

(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *pper_out* "data\\pper_out.txt")

(define (ros_pper . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_pper.txt"
		    (format "data\\rs_pper~a.txt" (car n)))))
	 (pair (map string->number (string-tokenize (car data))))
	 (res '())
	 )
    (set! res
	  (modulo (apply permutations pair)(expt 10 6)))
    
    
    (call-with-output-file *pper_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)
    res
    
))


