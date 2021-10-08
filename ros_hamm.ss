#lang racket
;; rosalind
;; 
;; [HAMM] 20**/**/**
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *hamm_out* "hamm_out.txt")

(define (ros_hamm . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_hamm.txt"
		    (format "rs_hamm~a.txt" (car n)))))
	 )
    (apply hamming-dist-str data)
    #|
    (call-with-output-file *hamm_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))

(define (hamming-dist lst1 lst2)
  (apply + (map (lambda(x y)(if (equal? x y) 0 1)) lst1 lst2)))

(define (hamming-dist-str str1 str2)
  (hamming-dist (string->list str1)(string->list str2)))
