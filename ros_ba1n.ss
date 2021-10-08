;; rosalind
;; Generate the d-Neighborhood of a String
;; [BA1B] 2021/07/09 AC
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "roslib.ss")
(define *ba1n_out* "ba1n_out.txt")

(define (ros_ba1n . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba1n.txt"
		    (format "rs_ba1n~a.txt" (car n)))))
	 (dna (car data))
	 (d   (string->number (cadr data)))
	 (res (all-combi dna d))
       )
    
    (call-with-output-file *ba1n_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a~%" kmer) out))
		  res))
      #:exists 'truncate/replace)
#t
))



	  
