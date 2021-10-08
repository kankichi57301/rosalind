#lang racket
;; rosalind
;; Complementing a Strand of DNA 
;; [REVC] 20**/**/**
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(define *revc_out* "****_out.txt")

(define (ros_revc . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_revc.txt"
		    (format "rs_revc~a.txt" (car n)))))
	 )
    (m-rc (car data))
    #|
    (call-with-output-file *revc_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))



