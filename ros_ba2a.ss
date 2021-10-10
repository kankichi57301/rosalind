#lang racket
;; rosalind
;; Implement MotifEnumeration
;; [BA2A] 2021/07/10 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(define *ba2a_out* "data\\ba2a_out.txt")

(define (ros_ba2a . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba2a.txt"
		    (format "data\\rs_ba2a~a.txt" (car n)))))
	 (kd (string-tokenize (car data)))
	 (k (string->number (car kd)))
	 (d (string->number (cadr kd)))
	 (strs (cdr data))
	 (res (filter (lambda(dna)(andmap (lambda(str)(include-under-d-mismatch-motif str dna d)) strs))
		      (all-dnas k)))

       )
    
    (call-with-output-file *ba2a_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    res
))

;;
(define (include-under-d-mismatch-motif str search d)
  (include-under-d-mismatch-motif0 str search d
				   (string-length str)
				   (string-length search)))

(define (include-under-d-mismatch-motif0 str search d strlen searchlen)
  ;(displayln (format "arg=~s search=~a [~a]" str search  (hamming-distance str search) ))
  (if (< strlen searchlen)
      #f
      (if (<= (hamming-distance str search) d)
	  #t
	  (include-under-d-mismatch-motif0 (string-drop str 1) search d (- strlen 1)searchlen ))))

