;; rosalind
;; Find a Highest-Scoring Fitting Alignment of Two Strings
;; [BA5H] 2021/08/14 AC

(require srfi/13)
(require "readfileA.ss")
(include "align-fitting.ss")

(define *ba5h_out* "ba5h_out.txt")

(define (ros_ba5h . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba5h.txt"
		    (format "rs_ba5h~a.txt" (car n)))))
	 (res (apply fitting-align data))
	 )
    
    
    (call-with-output-file *ba5h_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln (format "~a" x) out))
		  res))
      #:exists 'truncate/replace)
    res
    
))



