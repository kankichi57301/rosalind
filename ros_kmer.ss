#lang racket
;; rosalind
;; k-Mer Composition
;; [KMER] 2021/10/07 C=>scheme
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(define *kmer_out* "data\\kmer_out.txt")
(define myhash '()) 

(define (ros_kmer . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_kmer.txt"
		    (format "data\\rs_kmer~a.txt" (car n)))))
	 (res '())
	 )
    (set! myhash (make-hash))
    (for-each (lambda(dna)(hash-set! myhash dna 0))(all-dnas 4))
    (register-all-kmers (apply string-append (cdr data)) 4)
    (set! res
	  (map cadr
	       (sort (hash-map myhash list)(lambda(x y)(string<? (car x)(car y))))
	       ))
    
    (call-with-output-file *kmer_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a " x) out))
		  res)
	(display "\n")
	)
      
      #:exists 'truncate/replace)
    #t
    ))

(define (register-all-kmers str n)
  (when (>= (string-length str) n)
	(begin (inc-hash! myhash (string-take str 4))
	       (register-all-kmers (string-drop str 1) n))))

