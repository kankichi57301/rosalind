;; rosalind
;; Compute the Score of a Linear Peptide
;; [BA4K] 2021/08/02 AC
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "roslib2.ss")
(include "monoisotopic.ss")
(define *ba4k_out* "ba4k_out.txt")

(define (ros_ba4k . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba4k.txt"
		    (format "rs_ba4k~a.txt" (car n)))))
	 (peptide (car data))
	 (spect (map string->number (string-tokenize (cadr data))))
	 
	 )
   
    (solve-ba4k peptide spect)
		
    #|
    (call-with-output-file *ba4k_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))

(define (solve-ba4k peptide spect)
  (let ((therical-spect (theorical-weight-int peptide)))
    ;(displayln therical-spect)
    ;(displayln spect)
    (displayln (linear-score therical-spect spect))))
;;
;; 
;;
