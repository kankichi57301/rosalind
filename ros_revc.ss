#lang racket
;; rosalind
;; Complementing a Strand of DNA 
;; [REVC] 20**/**/**
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(define *revc_out* "data\\revx_out.txt")

(define (ros_revc . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_revc.txt"
		    (format "data\\rs_revc~a.txt" (car n)))))
	 (res  '())
	 )
    (set! res (m-rc (car data)))
    
    (call-with-output-file *revc_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)
    res
))



