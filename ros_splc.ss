#lang racket
;;rosalind
;;RNA Splicing 
;;[SPLC]

;; test data
(require "monoisotopicA.ss")
(require "readfileA.ss")
(require srfi/13)
(define *splc_out* "data\\splc_out.txt")

(define t250 '(">DATA1" "ACTACT" "CCC" "GCA"
	       ">DATA2" "CATCAT" "TAG" "GATTAT"
	       ">DATA3" "CTG" "AAA" "GAG" "TTTCCC"
	       ))





(define dna25 "ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG")

(define intron1 "ATCGGTCGAA")

(define intron2 "ATCGGTCGAGCGTGT")



(define (dna2prot dna)
  (if (stop-codon? (string-take dna 3))
      ""
      (if
       (or(not (non-empty-string? dna))  
	  (< (string-length dna) 3))
	  "*"
	  (string-append
	   (cadr (assoc (string-take dna 3) codon-table))
	   (dna2prot (string-drop dna 3))))))

(define (splice-1 dna intron)
  (let ((pos (string-contains dna intron)))
    (if pos
	(string-append
	 (string-take dna pos)
	 (string-drop dna (+ pos (string-length intron))))
	"")))

(define (splice-m dna intronlist)
  (if (null? intronlist)
      dna
      (splice-m (splice-1 dna (car intronlist)) (cdr intronlist))))

(define *ros_splc* "rosalind_splc.txt")
(define *ros_splc0* "rs25.txt")

#|
(define (ros_splc)
  (let ((fasta-data (edit-fasta(read-file "_splc0"))))
    (format "~a"
	    (dna2prot(splice-m (car fasta-data)
			       (cdr fasta-data)
			       )))))
|#

(define (ros_splc . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_splc.txt"
		    (format "data\\rs_splc~a.txt" (car n)))))
	 (dnas (edit-fasta data))
	 (res '())
	 )
    (set! res
	  (dna2prot(splice-m (car dnas)(cdr dnas))))
    
    (call-with-output-file *splc_out*
      (lambda(out)

		    (displayln res out))

      #:exists 'truncate/replace)
))
