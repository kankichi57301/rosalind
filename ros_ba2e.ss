#lang racket
;; rosalind
;; Implement GreedyMotifSearch with Pseudocounts
;; [BA2E] 2021/07/17 AC
;; 2012/10/14 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(require "greedymotifA.ss")
(define *ba2e_out* "data\\ba2e_out.txt")

(define *DEBUG* #f)

(define (ros_ba2e . arg)
  (let* ((data (read-file*
		(if (null? arg)
		    "data\\rosalind_ba2e.txt"
		    (format "data\\rs_ba2e~a.txt" (car arg)))))
	 (1st-line (map string->number(string-tokenize (car data))))
	 (dnas-str (cdr data))
	 (res '())
	 (n (cadr 1st-line))
	 (k (car 1st-line))
	 (len (string-length (car dnas-str)))
	 )

    (set! res (map (lambda(x)(apply string x))(greedy-motif-search-pseudo-count (map string->list dnas-str) n k)))
    
    (call-with-output-file *ba2e_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a~%" kmer) out))
		  res))
      #:exists 'truncate/replace)
    
    res
))



(define (greedy-motif-search-pseudo-count dnacharlists n k)
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
		  (set! prob-mat (list-list-add prob-mat (map (lambda(x)(make-list 4 1))(iota k)))) ;;; + pseudo count
		  (for-each (lambda(i)
			      (set! next (find-max-prob (list-ref dnacharlists i) prob-mat k))
			      (set! motifs (cons next motifs))

			      (set! prob-mat (list-list-add prob-mat (dna->countlist next)))
			      (when *DEBUG*
				    (displayln prob-mat))
			      )
			    (iota (- n 1) 1))
		  (when *DEBUG*
			(displayln prob-mat))
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


	 
    
	 




