;; rosalind
;; Generate the Theoretical Spectrum of a Linear Peptide
;; [BA4J] 2021/08/02 AC
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "monoisotopic.ss")
(define *ba4j_out* "ba4j_out.txt")

(define (ros_ba4j . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba4j.txt"
		    (format "rs_ba4j~a.txt" (car n)))))
	 (res "")
	 )
    (set! res (theorical-weight-int (car data)))
    
    (call-with-output-file *ba4j_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    res
))



