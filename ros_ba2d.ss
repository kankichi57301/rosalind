#lang racket
;; rosalind
;; Implement GreedyMotifSearch
;; [BA2D] 2021/07/17 AC
;; 2021/10/14 AC 
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(require "greedymotifA.ss")
(define *ba2d_out* "data\\ba2d_out.txt")

(define *DEBUG* #f)

;;(define n 0)
;;(define len 0)
;;(define k 0)

(define (ros_ba2d . arg)
  (let* ((data (read-file*
		(if (null? arg)
		    "data\\rosalind_ba2d.txt"
		    (format "data\\rs_ba2d~a.txt" (car arg)))))
	 (1st-line (map string->number(string-tokenize (car data))))
	 (dnas-str (cdr data))
	 (res '())
	 (n (cadr 1st-line))
	 (k (car 1st-line))
	 )
    ;;(displayln (format "n=~a k=~a" n k))
    
    (set! res (map (lambda(x)(apply string x))(greedy-motif-search (map string->list dnas-str) n k)))
    
    (call-with-output-file *ba2d_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a~%" x) out))
		  res))
      #:exists 'truncate/replace)
    
    res
))

(define (kmer-prob dna problist)
  (apply * (map (lambda(nuc prob)(list-ref prob (nuc2num nuc)))
		dna
		problist
		)))


	





	 
    
	 




