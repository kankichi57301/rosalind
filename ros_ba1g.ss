#lang racket
;; rosalind
;; 
;; [BA1G] 2021/05/
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(define *ba1g_out* "ba1g_out.txt")


(define (ros_ba1g . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba1g.txt"
		    (format "rs_ba1g~a.txt" (car n)))))
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



	 
				

