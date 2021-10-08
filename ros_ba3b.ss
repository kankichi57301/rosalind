;; rosalind
;; Reconstruct a String from its Genome Path
;; [BA3B] 2021/07/10 AC 
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(define *ba3b_out* "ba3b_out.txt")

(define (ros_ba3b . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba3b.txt"
		    (format "rs_ba3b~a.txt" (car n)))))
	 (res (format "~a~a"
		      (car data)
		      (apply string-append (map (lambda(s)(string-take-right s 1))(cdr data)))))
       )
    
    
    (call-with-output-file *ba3b_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)
    res
))

	 
				

