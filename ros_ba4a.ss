;; rosalind
;; Translate an RNA String into an Amino Acid String
;; [BA4A] 2021/07/06 AC 
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "monoisotopic.ss")
(define *ba4a_out* "ba4a_out.txt")

(define (ros_ba4a . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba4a.txt"
		    (format "rs_ba4a~a.txt" (car n)))))
	 (rna (car data))
	 (dna (regexp-replace* #rx"U" rna "T"))
	 (protein '())
	)
    (set! protein (dna->protein  dna ))
    
    (call-with-output-file *ba4a_out*
      (lambda(out)
	(display protein out))
      #:exists 'truncate/replace)
    
))

(define (dna->protein dna)
  (if (or (< (string-length dna) 3)
	  (string=? dna "TAG")  ;; Stop codons
	  (string=? dna "TGA")
	  (string=? dna "TAA"))
      ""
      (string-append (cadr (assoc (string-take dna 3)codon-table))
		     (dna->protein (string-drop dna 3)))))
  

				

