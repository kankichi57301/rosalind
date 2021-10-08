;; rosalind
;; Find Frequent Words with Mismatches and Reverse Complements
;; [BA1J] 2021/07/
;(require srfi/1)
(require srfi/13)
(require srfi/19)
(require suffixtree)
(include "readfile.ss")
(include "roslib.ss")
(include "monoisotopic.ss")
(include "ba1j-test.ss")
(define *time* #f)
(define *ba1j_out* "ba1j_out.txt")
(define *dna* "")

(define (ros_ba1j . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba1j.txt"
		    (format "rs_ba1j~a.txt" (car n)))))
	 (dna (car data))
	 (2ndline (string-tokenize (cadr data)))
	 (k (string->number (car 2ndline)))
	 (d (string->number (cadr 2ndline)))
	 (res '())
	 )
    (set! *dna* dna)
    (set! *time* (current-time))
    (set! res (solve-ba1j  dna k d))
    (displayln res)
    
    (call-with-output-file *ba1j_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    (displayln (format "elapsed =~a sec" (time-second (time-difference (current-time) *time*))))
    
))

(define (solve-ba1j dna k d)
  (let ((dnatree (make-tree)))
    (tree-add! dnatree (string->label (format "~a$" dna)))
    (solve-ba1j0 dnatree k d)))
	 
(define (solve-ba1j0 dnatree k d)
  (let* (
	 (motif-count (map (lambda(motif)(list motif (tree-motif-count-rc-d dnatree motif d)))
				  (all-dnas k)))
	 (maxval (apply max (map cadr motif-count)))
	 (find (map car (filter (lambda(x)(= (cadr x) maxval)) motif-count)))
	 )
    (displayln (format "maxval=~a" maxval))
    find))
;; hammin‹——£‚ªdˆÈ‰º‚©

(define (hamming-distance-le dna motif d)
  (hamming-distance-le0 dna motif d (string-length dna)(string-length motif)))

(define (hamming-distance-le0 dna motif d dnalen motiflen)
  (if (= 0 motiflen)
      (if (= d 0)
      #t #f)
      (if (= dnalen motiflen)
	  (if (<= (hamming-distance dna motif) d) #t #f)
	  (if (char=? (string-ref dna 0) (string-ref motif 0))
	      (hamming-distance-le0 (string-drop dna 1) (string-drop motif 1) d (- dnalen 1)(- motiflen 1))
	      (hamming-distance-le0 (string-drop dna 1) (string-drop motif 1) (- d 1)(- dnalen 1)(- motiflen 1))))))


