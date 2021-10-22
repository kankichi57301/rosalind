#lang racket
;; rosalind
;;[EBIN] 
;;2021/04/17 AC
(require srfi/13)
(require "roslibA.ss")
(require "readfileA.ss")

(define *ros_ebin_out* "data\\ebin_out.txt")
#|
(define (round4 num)
  (/ (round (* 10000 num)) 10000))
|#
(define (ros_ebin . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ebin.txt"
		    (format "data\\rs_ebin~a.txt" (car n)))))
	 (multiplicand (string->number (car data)))
	 (multiplyer (map string->number (string-tokenize (cadr data))))
	 )


    (call-with-output-file *ros_ebin_out*
      (lambda(out)
	(for-each (lambda(x) (display (format "~s "  (roundp3(* multiplicand x))) out)) multiplyer))
      #:exists 'truncate/replace) 
   ))
