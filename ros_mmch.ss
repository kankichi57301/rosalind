#lang racket
;; rosalind 
;; Maximum Matchings and RNA Secondary Structures
;; [MMCH]

(require srfi/1)
(require srfi/13)
(require math/number-theory)
(require "readfileA.ss")

(define (ros_mmch . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_mmch.txt"
		    (format "data\\rs_mmch~a.txt" (car n)))))
	 (dna-list (string->list (car (edit-fasta data))))
	 (a-count (length (indexes-of dna-list #\A)))
	 (c-count (length (indexes-of dna-list #\C)))
	 (g-count (length (indexes-of dna-list #\G)))
	 (u-count (length (indexes-of dna-list #\U)))
	 )


    (* (if (>= a-count u-count)
	   (permutations a-count u-count)
	   (permutations u-count a-count))
       (if (>= g-count c-count)
	   (permutations g-count c-count)
	   (permutations c-count g-count)))
    
    ))    
