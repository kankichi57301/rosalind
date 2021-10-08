#lang racket
;; rosalind
;; Generate All Maximal Non-Branching Paths in a Graph
;; [BA3M] 20**/**/**
(require srfi/1)
(require srfi/13)
(require srfi/14)
(require "readfileA.ss")
(require "roslibA.ss")
(define *ba3m_out* "ba3m_out.txt")
(define *ans* '())

(define (ros_ba3m . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba3m.txt"
		    (format "rs_ba3m~a.txt" (car n)))))
	 (adjlist(append-map parse-adj-list data))
	 (all-nodes (delete-duplicates (flatten adjlist)))
	 (in-out (in-out-count adjlist all-nodes))
	 (starts (map car (filter (lambda(x)(and (not (and (= 1 (cadr x))
							   (= 1 (caddr x))))
						 (> (caddr x) 0)))
				  in-out)))
	 )
    
    (define (expand-1-1 nodes adjlist)
      (let* ((right (car nodes))
	     (nexts (filter (lambda(x)(equal? right (car x))) adjlist)))
	(for-each (lambda(x)(expand-1-1-a (append nodes (list (cadr x))) adjlist)) nexts) 
	nexts)
      #t
      )

    (define (expand-1-1-a nodes adjlist)
      (let* ((right (car (take-right nodes 1)))
	     (nexts (filter (lambda(x)(equal? right (car x))) adjlist)))
	(display (format "r=~a|" right))
	(set! all-nodes (delete right all-nodes))
        (if (= 1 (length nexts))
	    (expand-1-1-a (append nodes (list (cadar nexts))) adjlist)
	    (begin
	      (set! *ans* (cons nodes *ans*))
	      (displayln (format "ans=~a" nodes))
	      ))))

    (displayln adjlist)
    ;(displayln (format "all=~a" all-nodes))
    ;(displayln in-out)
    (displayln (format "start cand=~a" starts))

    (set! *ans* '())
    (for-each (lambda(i)
		(set! all-nodes (delete i all-nodes))
		(expand-1-1 `(,i) adjlist))
	      starts)
    (displayln *ans*)
    (displayln (format "rest=~a" all-nodes))
    ;; --*-- TBC isolated cycle --*--
    #|
    (call-with-output-file *ba3m_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    ))

(define (in-out-count adjlist nodes)
  (map (lambda(node)(list node
			  (count (lambda(x)(equal? node (cadr x)))  adjlist)  ;; IN
			  (count (lambda(x)(equal? node (car x)))   adjlist)))  ;; OUT count
       nodes))





