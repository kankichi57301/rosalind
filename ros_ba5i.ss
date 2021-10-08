;; rosalind
;; Find a Highest-Scoring Overlap Alignment of Two Strings
;; [BA5I] 2021/08/14 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(include "align-overlap.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba5i_out* "ba5i_out.txt")

(define (ros_ba5i . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba5i.txt"
		    (format "rs_ba5i~a.txt" (car n)))))
	 (res (apply overlap-align data))
	 )
    
    
    (call-with-output-file *ba5i_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln (format "~a" x) out))
		  res))
      #:exists 'truncate/replace)
    
    res
    
))



