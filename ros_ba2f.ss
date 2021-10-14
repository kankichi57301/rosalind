#lang racket
;; rosalind
;; Implement RandomMotifSearch 
;; [BA2F] 2021/09/17 AC
;; 2012/10/14 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(define *ba2f_out* "data\\ba2f_out.txt")
(define *pseudo-count* 1)

(define *DEBUG* #t)
(define dnas-str-list '())
(define iter 1000)


(define *profile* '())

(define (ros_ba2f . arg)
  (let* ((data (read-file*
		(if (null? arg)
		    "data\\rosalind_ba2f.txt"
		    (format "data\\rs_ba2f~a.txt" (car arg)))))
	 (1st-line (map string->number(string-tokenize (car data))))
	 (dnas (cdr data))
	 (res '())
	 (res0 '())
	 (k (car 1st-line))
	 (score 0)
	 (max-score 0)
	)

    (for-each
     (lambda(i)
       (set! res0 (random-motif-search dnas k))
       (set! score (cadr res0))
       (when (> score max-score)
	     (begin
	       (set! max-score score)
	       (set! res (car res0)))))
     (iota iter))
    (displayln (format "score=~a" max-score))
    (displayln (format "ans=~a" res))
    
    (call-with-output-file *ba2f_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (displayln (format "~a" kmer) out))
		  res))
      #:exists 'truncate/replace)
    #t
))
    

(define (dnas->motifs dnas k nlist)
  (map (lambda(x y)(take (drop x y) k)) 
       dnas
       nlist))

;;
;;

(define (find-max-prob* str prob-mat k)
  (apply string
	 (find-max-prob (string->list str) prob-mat k)))

(define (find-max-prob charlist prob-mat k)
  (let ((first-kmer (take charlist k)))
    (find-max-prob0 charlist prob-mat first-kmer (kmer-prob first-kmer prob-mat) k)))

(define (find-max-prob0 charlist prob-mat ans maxval k)
  (if (< (length charlist) k)
      (begin ;(displayln (format "ans=~a prob=~a" ans maxval))
	     ans)
      (let* ((kmers (take charlist k))
	     (val (kmer-prob kmers prob-mat)))
	(if (> val maxval)
	    (find-max-prob0 (cdr charlist) prob-mat kmers val k)      ;; update
	    (find-max-prob0 (cdr charlist) prob-mat ans   maxval k)   ;; not update
	))))
	
;;—”‚Å‰Šú‚Ìmotif‚ðì‚é
(define (make-random-motif dnacharlist len k)
    (map (lambda(dna)(string-take (string-drop dna (random (+ 1 (- len k)))) k)) dnacharlist))

(define (random-motif-search dnacharlist k)
  (let* ((len (string-length (car dnacharlist)))
	 (best-motif (make-random-motif dnacharlist len k))
	 (profile (motifs->profile-pseudo-count (map string->list best-motif) *pseudo-count*))
	 (score (apply * (map (lambda(x)(apply max x)) profile)))
	 (next-motif '())
	 (next-score 0)
	 (next-profile '())
	 )
    ;(displayln (format "1st motif=~a"  best-motif))
    ;(displayln (format "prof=~a" profile))

    ;(displayln (format "score=~a" score)) ;; ’Fn^k ‚ÅŠ„‚é‘O‚Ì’l(unnomalized)
    (when *DEBUG*
	  (set! *profile* profile))
    
    ;;-----------------------------
    (define (loop lc)
      (set! next-motif (map (lambda(x)(find-max-prob* x profile k)) dnacharlist ))
      ;(displayln (format "next motif=~a[~a]"  next-motif lc))
      
      (set! next-profile (motifs->profile-pseudo-count (map string->list next-motif) *pseudo-count*))
    
      ;(displayln (format "next-prof=~a" next-profile))
      (set! next-score (apply * (map (lambda(x)(apply max x)) next-profile)))
      ;(displayln (format "next score=~a" next-score))
      
    
      (if (< score next-score)
	  (begin
	    (set! score next-score)
	    (set! best-motif next-motif)
	    (set! profile next-profile)
	    (loop (+ 1 lc)))
	  (list best-motif score)))
    ;;-------------------------------
    (loop 1))
)    
    


;;--*-- test--*--
(define d1 '("GGTTGGTTAAAACCCC" "GTGTTTGAAATTCCAA" "ATTTAATAATTACCCC" "GCGTGGATAAAACCCC"))

(define d2
  '(
    "CGCCCCTCTCGGGGGTGTTCAGTAAACGGCCA"
    "GGGCGAGGTATGTGTAAGTGCCAAGGTGCCAG"
    "TAGTACCGAGACCGAAAGAAGTATACAGGCGT"
    "TAGATCAAGTTTCAGGTGCACGTCGGTGAACC"
    "AATCCACCAGCTCCACGTGCAATGTTGGCCTA"))


;;--*-- test
;(random-motif-search d1 16 5)
;(random-motif-search d2 32 8)
;; (find-max-prob* (car d1) *profile*)
;; (map (lambda(str)(find-max-prob* str *profile*)) d1)	 
    
	 




