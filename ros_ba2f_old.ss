;; rosalind
;; Implement RandomMotifSearch 
;; [BA2F] 2021/07/17 start
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "roslib.ss")
(define *ba2f_out* "ba2f_out.txt")

(define *DEBUG* #f)
(define dnas-str "")
(define n 0)
(define len 0)
(define k 0)

(define (ros_ba2f . arg)
  (let* ((data (read-file*
		(if (null? arg)
		    "rosalind_ba2f.txt"
		    (format "rs_ba2f~a.txt" (car arg)))))
	 (1st-line (map string->number(string-tokenize (car data))))
	 (rest (cdr data))
	 (res '())
	)
    
    (set! n (cadr 1st-line))
    (set! k (car 1st-line))
    (set! dnas-str rest)
    (set! len (string-length (car dnas-str)))

    ;dnas-str
    ;;(set! res (map (lambda(x)(apply string x))
    (random-motif-search (map string->list dnas-str))
    ;;))
    
    
    #|
    (call-with-output-file *ba2f_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a~%" kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
   
))
    

(define (dnas->motifs dnas k nlist)
  (map (lambda(x y)(take (drop x y) k)) 
       dnas
       nlist))

;;
;;

(define (kmer-prob dna problist)
  (apply * (map (lambda(nuc prob)(list-ref prob (nuc2num nuc)))
		dna
		problist
		)))


(define (find-max-prob charlist prob-mat)
  (let ((first-kmer (take charlist k)))
    (find-max-prob0 charlist prob-mat first-kmer (kmer-prob first-kmer prob-mat))))

(define (find-max-prob0 charlist prob-mat ans maxval)
  (if (< (length charlist) k)
      (begin ;(displayln (format "ans=~a prob=~a" ans maxval))
	     ans)
      (let* ((kmers (take charlist k))
	     (val (kmer-prob kmers prob-mat)))
	(if (> val maxval)
	    (find-max-prob0 (cdr charlist) prob-mat kmers val)      ;; update
	    (find-max-prob0 (cdr charlist) prob-mat ans   maxval)   ;; not update
	))))
	


(define (random-motif-search dnacharlists)
  (let* (
	 
	 (best-motif (map (lambda(dna)(take (drop dna (random (+ 1 (- len k)))) k)) dnacharlists))
	 (profile (motifs->profile-pseudo-count best-motif))
	 (score (score-motif-matrix best-motif))
	 (next-motif '())
	 (next-score 0)
	 )

    (displayln (format "1st=~a" (map (lambda(x)(apply string x)) best-motif)))
    (displayln (format "score=~a" score)    )
    ;(displayln profile)    

    
    (set! next-motif (map (lambda(x)(find-max-prob x profile)) dnacharlists ))
    (displayln (format "next=~a" (map (lambda(x)(apply string x)) next-motif)))
    (set! next-score (score-motif-matrix (transpose next-motif)))
    (displayln next-score)    
    
  ))


	 
    
	 




