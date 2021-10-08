;; rosalind
;; Compute the Edit Distance Between Two Strings
;; [BA5G] 2021/08/03 AC
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "editdist.ss")
(define *ba5g_out* "ba5g_out.txt")

(define (ros_ba5g . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba5g.txt"
		    (format "rs_ba5g~a.txt" (car n)))))
	 )
    (apply edit-distance data )
    #|
    (call-with-output-file *ba5g_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))



