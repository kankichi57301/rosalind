;; rosalind
;; Find Substrings of a Genome Encoding a Given Amino Acid String
;; [BA4B] 2021/07/10 AC 
;(require srfi/1)
(require srfi/13)

(include "readfile.ss")
(include "roslib.ss")
(include "monoisotopic.ss")
(define *ba4b_out* "ba4b_out.txt")

(define (ros_ba4b . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba4b.txt"
		    (format "rs_ba4b~a.txt" (car n)))))
	 (dna (car data))
	 (amino (cadr data))
	 (amino-rx (aminostr->rx amino))
	 (amino-rx-rc (aminostr->rx-rc amino))
	 (res (append (regexp-match* amino-rx dna)
		      (regexp-match* amino-rx-rc dna)))
       )
    (displayln (format "regexp=\"~a\"" amino-rx))
    (displayln res)
    
    (call-with-output-file *ba4b_out*
      (lambda(out)
	(for-each (lambda(str)
		    (displayln (format "~a" str) out))
		  res))
      #:exists 'truncate/replace)

    #t
))

	 
				

