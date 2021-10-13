#lang racket
;; rosalind
;; Compute the Hamming Distance Between Two Strings
;; [BA1G] 2021/10/13 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(define *ba1g_out* "data\\ba1g_out.txt")


(define (ros_ba1g . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba1g.txt"
		    (format "data\\rs_ba1g~a.txt" (car n)))))
	 (str1 (car data))
	 (str2 (cadr data))
       )
    (hamming-distance str1 str2)
    #|
    (call-with-output-file *ba1g_out*
      (lambda(out)
	(display data out))
      #:exists 'truncate/replace)
    |#
))



	 
				

