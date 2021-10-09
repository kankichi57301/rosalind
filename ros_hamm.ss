#lang racket
;; rosalind
;; Counting Point Mutations
;; [HAMM] 2021/10/08
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *hamm_out* "hamm_out.txt")

(define (ros_hamm . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_hamm.txt"
		    (format "data\\rs_hamm~a.txt" (car n)))))
	 (res '())
	 )
    (set! res (apply hamming-dist-str data))
    
    (call-with-output-file *hamm_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)
    res
))

(define (hamming-dist lst1 lst2)
  (apply + (map (lambda(x y)(if (equal? x y) 0 1)) lst1 lst2)))

(define (hamming-dist-str str1 str2)
  (hamming-dist (string->list str1)(string->list str2)))
