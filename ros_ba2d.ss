;; rosalind
;; Implement GreedyMotifSearch
;; [BA2D] 2021/07/17 AC
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "roslib.ss")
(define *ba2d_out* "ba2d_out.txt")

(define *DEBUG* #f)
(define dnas-str "")
(define n 0)
(define len 0)
(define k 0)

(define (ros_ba2d . arg)
  (let* ((data (read-file*
		(if (null? arg)
		    "rosalind_ba2d.txt"
		    (format "rs_ba2d~a.txt" (car arg)))))
	 (1st-line (map string->number(string-tokenize (car data))))
	 (rest (cdr data))
	 (res '())
	)
    
    (set! n (cadr 1st-line))
    (set! k (car 1st-line))
    (set! dnas-str rest)
    (set! len (string-length (car dnas-str)))

    (set! res (map (lambda(x)(apply string x))(greedy-motif-search (map string->list dnas-str))))
    
    
    
    (call-with-output-file *ba2d_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a~%" kmer) out))
		  res))
      #:exists 'truncate/replace)
    
    res
))

(define (kmer-prob dna problist)
  (apply * (map (lambda(nuc prob)(list-ref prob (nuc2num nuc)))
		dna
		problist
		)))


	


(define (greedy-motif-search dnacharlists)
  (let* ((kmer '())
	 (prob-mat '())
	 (best (map (lambda(dna)(take dna k))dnacharlists))
	 (score (score-motif-matrix best))
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
		  (when *DEBUG*
			(displayln (format "motif=~a score=~a" motifs nextscore)))
		  (when (< nextscore score)
			(begin (set! best motifs)
			       (set! score nextscore)))
		  ))
	      (iota (+ 1 (- len k))))
		  
    (reverse best)
))


	 
    
	 




