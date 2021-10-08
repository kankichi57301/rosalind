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
;(define *adjlist* '())

(define (ros_ba3m . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba3m.txt"
		    (format "rs_ba3m~a.txt" (car n)))))
	 (adjlist(append-map parse-adj-list data))
	 (nodes (delete-duplicates (flatten adjlist)))
	 (in-out (in-out-count adjlist nodes))
	 (starts (map car (filter (lambda(x)(and (not (and (= 1 (cadr x))
							   (= 1 (caddr x))))
						 (> (caddr x) 0)))
				  in-out)))
	)
    
    (displayln adjlist)
    (displayln (format "all=~a" nodes))
    (displayln in-out)
    (displayln (format "start=~a" starts))


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
			  (count (lambda(x)(equal? node (car x))) adjlist)))  ;; OUT count
       nodes))





							 
  
    


