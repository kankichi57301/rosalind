#lang racket
;; rosalind
;; 
;; [BA6K] 20**/**/**
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")

;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba6k_out* "ba6k_out.txt")

(define (ros_ba6k . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba6k.txt"
		    (format "rs_ba6k~a.txt" (car n)))))
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

(include "bwt.ss")


