;; rosalind
;; Find a Highest-Scoring Alignment of Two Strings
;; [BA5E] 2021/08/13 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(include "align-grobal.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba5e_out* "ba5e_out.txt")

(define (ros_ba5e . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba5e.txt"
		    (format "rs_ba5e~a.txt" (car n)))))
	 (res (apply grobal-align data))
	 )
    
    
    (call-with-output-file *ba5e_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln (format "~a" x) out))
		  res))
      #:exists 'truncate/replace)
    #t
    
))



