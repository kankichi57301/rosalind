;; rosalind
;; Construct the Overlap Graph of a Collection of k-mers
;; [BA3C] 2021/07/11 AC 
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(define *ba3c_out* "ba3c_out.txt")

(define (ros_ba3c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba3c.txt"
		    (format "rs_ba3c~a.txt" (car n)))))
	 (len (string-length (car data)))
	 (res '())
	 )
    (set! res
	  (map (lambda(x)(format "~a -> ~a" (car x)(cadr x)))
	       (filter (lambda(pair)(and (not (string= (car pair)(cadr pair)))
					 (string= (string-take-right (car pair)(- len 1))
						  (string-take       (cadr pair)(- len 1))))) 
		 (cartesian-product data data))))


    (call-with-output-file *ba3c_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (displayln kmer out))
		  res))
      #:exists 'truncate/replace)

    #t
))

	 
				

