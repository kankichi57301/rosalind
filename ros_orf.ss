#lang racket
;;rosalind
;;Open Reading Frames
;;(ORF)
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "monoisotopicA.ss") 
(define *orf_out* "data\\orf_out.txt")
;; test data
(define dna1 "AGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGACTTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG")

(define (get-orf-0 dna)
    (if (or
	 (< (string-length dna) 3)
         (not (non-empty-string? dna))
       )
      "*"
      (if (stop-codon? (string-take dna 3))
	  ""
	  (string-append
	   (cadr (assoc (string-take dna 3) codon-table))
	   (get-orf-0 (string-drop dna 3))))))

(define (get-orf-1 dna)
  ;;(displayln dna)
  (string-append "M" (get-orf-0 dna )))
  
(define (get-orf dna)
  (if (or (not (non-empty-string? dna))
	  (< (string-length dna) 3))
      '()
      (if (string= (string-take dna 3) start-codon)
	  (cons (get-orf-1 (string-drop dna 3))
		(get-orf (string-drop dna 3)))
	  (get-orf (string-drop dna 1)))))
	  
    

(define dna2
  (string-map complement (string-reverse dna1)))

(define (get-orf-all dna)
  (filter (lambda(x)(not (string=? (string-take-right x 1) "*")))
	  (delete-duplicates
	   (append
	    (get-orf dna)
	    (get-orf (string-map complement (string-reverse dna)))))))



(define (solve_orf)
  (for-each (lambda(x)(displayln (format "~a" x)))
       (get-orf-all (apply string-append (cdr (read-file "_orf"))))))


(define (ros_orf . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_orf.txt"
		    (format "data\\rs_orf~a.txt" (car n)))))
	 (dnas (edit-fasta data))
	 (res '())
	 )

    (set! res(get-orf-all (car dnas)))
    
    (call-with-output-file *orf_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln x out))
		  res))
      #:exists 'truncate/replace)
    
    res
))
