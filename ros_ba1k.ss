#lang racket
;; rosalind
;; Generate the Frequency Array of a String
;; [BA1K] 2021/07/09 AC
;; 2021/10/13 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(define *ba1k_out* "data\\ba1k_out.txt")

(define (ros_ba1k . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba1k.txt"
		    (format "data\\rs_ba1k~a.txt" (car n)))))
	 (dna (car data))
	 (k  (string->number(cadr data)))
	 (kmers (k-mers dna k))
	 (myhash '())
	 (res '())
       )
    (set! myhash (make-hash))
    (for-each (lambda(kmers)(inc-hash! myhash kmers)) kmers)
    (set! res
	  (map (lambda(kmer)
		 (hash-ref myhash kmer 0))
	       (all-dnas k)))
    
	      
    
    (call-with-output-file *ba1k_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    ;res
    #t
))

	 
				

