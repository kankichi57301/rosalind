(module rosalind racket/base
	(provide (all-defined-out))
	(require (only-in racket/list group-by take drop))
	(require (only-in racket/function identity))
	(require "roslibA.ss")
	(require srfi/1)
	(require srfi/13)

(define nucs '(#\A #\C #\G #\T))
(define dnas-str
  '(
    "GGCGTTCAGGCA"
    "AAGAATCAGTCA"
    "CAAGGAGTTCGC"
    "CACGTCAATCAC"
    "CAATAATATTCG"))

;;(define n (length dnas))
;;(define len (string-length (car dnas-str)))
;;(define k 3)
#|
(define (score-1-line charlist)
  (- n (apply max (map length (group-by identity charlist)))))

(define (score-motif-matrix motifs)
  (apply + (map score-1-line (transpose motifs))))
|#

(define dnas (map string->list dnas-str))

(define tr-dnas (transpose (map string->list dnas-str)))

;;test
;; reference https://www.youtube.com/watch?v=XKARo4cqN4Q&t=818s
;;
(define (displaysp item)(display (format "~a " item)))
;(displaysp (score-motif-matrix (map string->list '("GGC" "AAG" "AAG" "CAC" "CAA"))))
;(displaysp (score-motif-matrix (map string->list '("GCG" "AAG" "AAG" "ACG" "CAA"))))
;(displaysp (score-motif-matrix (map string->list '("CGT" "AAG" "AAG" "AAT" "AAT"))))
;(displaysp (score-motif-matrix (map string->list '("CAG" "CAG" "CAA" "CAA" "CAA")))) ;; <= Ans
#|
(define (nuc->countlist char)
  (case char
    [(#\A) '(1 0 0 0)]
    [(#\C) '(0 1 0 0)]
    [(#\G) '(0 0 1 0)]
    [(#\T) '(0 0 0 1)]))

(define (dna->countlist charlist)
  (map nuc->countlist charlist))
|#    
(define (dnas->motifs dnas k nlist)
  (map (lambda(x y)(take (drop x y) k)) 
       dnas
       nlist))

;;
;;
(define a1 '((1 2)(3 4)) )
(define b1 '((2 1)(10 11)) )


#|
(define (list-add* . listlist)
  (fold list-add (car listlist)(cdr listlist)))


(define (kmer-prob dna problist)
  (apply * (map (lambda(nuc prob)(list-ref prob (nuc2num nuc)))
		dna
		problist
		)))

|#
(define (find-max-prob charlist prob-mat k)
  (let ((first-kmer (take charlist k)))
    (find-max-prob0 charlist prob-mat first-kmer (kmer-prob first-kmer prob-mat) k)
    ))

(define (find-max-prob0 charlist prob-mat ans maxval k)
  (if (< (length charlist) k)
      (begin ;(displayln (format "ans=~a prob=~a" ans maxval))
	     ans)
      (let* ((kmers (take charlist k))
	     (val (kmer-prob kmers prob-mat)))
	(if (> val maxval)
	    (find-max-prob0 (cdr charlist) prob-mat kmers val    k)      ;; update
	    (find-max-prob0 (cdr charlist) prob-mat ans   maxval k)   ;; not update
	))))
	

;; test prob-mat
(define t-p1  '((0 0 1 0)(0 0 1 0)(0 1 0 0)))  ;;   GGC
(define t-p2  '((1 0 1 0)(1 0 1 0)(0 1 1 0)))  ;; + AAG
(define t-p3  '((2 0 1 0)(2 0 1 0)(0 1 2 0)))  ;; + AAG
(define t-p4  '((2 1 1 0)(3 0 1 0)(0 2 2 0)))  ;; + CAC

(define (greedy-motif-search dnacharlists n k)
  (let* ((kmer '())
	 (prob-mat '())
	 (best (map (lambda(dna)(take dna k))dnacharlists))
	 (score (score-motif-matrix best))
	 (len (length (car dnacharlists)))
	 )
    
    
    (for-each (lambda(pos)
		(set! kmer (take (drop (car dnacharlists) pos) k))
		(let* ((motifs (list kmer))
		       (prob-mat (dna->countlist kmer))
		       (next '())
		       (nextscore 0)
		       
		       )

		  (for-each (lambda(i)
			      (set! next (find-max-prob (list-ref dnacharlists i) prob-mat k))
			      (set! motifs (cons next motifs))
			      ;(displayln prob-mat)
			      (set! prob-mat (list-list-add prob-mat (dna->countlist next)))
			      )
			    (iota (- n 1) 1))
		  
		  ;(displayln prob-mat)
		  (set! nextscore (score-motif-matrix motifs))
		  ;(displayln (format "motif=~a score=~a" motifs nextscore))
		  (when (< nextscore score)
			(begin (set! best motifs)
			       (set! score nextscore)))
		  ))
	      (iota (+ 1 (- len k))))
		  
    ;;(displayln (format "best=~a" (reverse best)))
    (reverse best)
))
)

	 
    
	 




