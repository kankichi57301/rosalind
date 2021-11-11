#lang racket
;; rosalind
;; Reconstruct a String from its Burrows-Wheeler Transform
;; [BA9I] 2021/09/14 AC
;; 2021/11/11　AC モジュール構成変更
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "bwtA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")

(define *ba9j_out* "data\\ba9j_out.txt")


(define (ros_ba9j . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba9j.txt"
		    (format "data\\rs_ba9j~a.txt" (car n)))))
	 )
    
    
    (call-with-output-file *ba9j_out*
      (lambda(out)
	(displayln (rev-bwt (car data)) out))
      #:exists 'truncate/replace)
    
    #t
    
    ))




