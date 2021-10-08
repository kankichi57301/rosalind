#lang racket
;; rosalind
;; 
;; [****] 20**/**/**
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *_____out* "****_out.txt")

(define (ros_**** . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_****.txt"
		    (format "rs_****~a.txt" (car n)))))
	 )
    data
    #|
    (call-with-output-file *****_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))



