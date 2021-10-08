;; rosalind
;; Find a Longest Common Subsequence of Two Strings
;; [BA6C] 2021/07/23
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "lcs.ss")
(define *ba6c_out* "ba6c_out.txt")

(define (ros_ba6c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba6c.txt"
		    (format "rs_ba6c~a.txt" (car n)))))
	 )
    (apply string (lcs (string->list (car data))(string->list (cadr data))))
    #|
    (call-with-output-file *ba6c_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))



