;; rosalind
;; Find a Highest-Scoring Local Alignment of Two Strings
;; [BA5E] 2021/08/14 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(include "align-local.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba5f_out* "ba5f_out.txt")

(define (ros_ba5f . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba5f.txt"
		    (format "rs_ba5f~a.txt" (car n)))))
	 (res (apply local-align data))
	 )
    
    
    (call-with-output-file *ba5f_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln (format "~a" x) out))
		  res))
      #:exists 'truncate/replace)
    #t
    
))



