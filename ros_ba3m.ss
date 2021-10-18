#lang racket
;; rosalind
;; Generate All Maximal Non-Branching Paths in a Graph
;; [BA3M] 2021/10/06 AC
(require srfi/1)
(require srfi/13)
(require srfi/14)
(require "readfileA.ss")
(require "roslibA.ss")
(define *ba3m_out* "data\\ba3m_out.txt")
(define *ans* '())

(define (ros_ba3m . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba3m.txt"
		    (format "data\\rs_ba3m~a.txt" (car n)))))
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
	;;(display (format "r=~a|" right))
	(set! all-nodes (delete right all-nodes))
        (if (node-1-1? right in-out) 
	    (expand-1-1-a (append nodes (list (cadar nexts))) adjlist)
	    (begin
	      (set! *ans* (cons nodes *ans*))
	      ;;(displayln (format "ans=~a" nodes))
	      ))))

    ;;(displayln adjlist)
    ;;(displayln (format "all=~a" all-nodes))
    ;;(displayln in-out)
    (displayln (format "node[207]inout=~a" (assoc 207 in-out)))
    (displayln (format "[207] 1-1?=~a" (node-1-1? 207 in-out)))
    ;;(displayln (format "start cand=~a" starts))

    (set! *ans* '())
    (for-each (lambda(i)
		(set! all-nodes (delete i all-nodes))
		(expand-1-1 `(,i) adjlist))
	      starts)
    ;(displayln (format "chuukan kekka=~a" *ans*))
    ;(displayln (format "rest[1]=~a" all-nodes))
;; ŒÇ—§ƒ‹[ƒv‚ÌŒŸo
    (define (isolate-cycle nodes adjlist)
      (let* ((right (car nodes))
	     (nexts (filter (lambda(x)(equal? right (car x))) adjlist)))
	(set! all-nodes (delete right all-nodes))
	(for-each (lambda(x)(isolate-cycle-a (append nodes (list (cadr x))) adjlist right)) nexts) 
      #t
      ))

    (define (isolate-cycle-a nodes adjlist st)
      (let* ((right (car (take-right nodes 1)))
	     (nexts (filter (lambda(x)(equal? right (car x))) adjlist)))
	;;(display (format "r=~a|" right))
	(set! all-nodes (delete right all-nodes))
        (if (= 1 (length nexts))
	    (let ((next-append (append nodes (list (cadar nexts)))))
	      (if (equal? (cadar nexts) st)
		  (begin
		    ;;(displayln (format "ans=~a" next-append))
		    (set! *ans* (cons next-append *ans*)))
		  (isolate-cycle-a  next-append adjlist st)))
	    (begin
	      (set! *ans* (cons nodes *ans*))
	      (displayln (format "ans=~a" nodes))
	      ))))
 
    ;(isolate-cycle '(14) adjlist) 
    (define (isolate-cycle-main)
      (if (null? all-nodes)
	  #f
	  (begin
	    (isolate-cycle (list (car all-nodes)) adjlist)
	    (isolate-cycle-main))))
    
    (isolate-cycle-main)
        
    ;;(displayln (format "saishuu kekka=~a" *ans*))
    ;;(displayln (format "rest[2]=~a" all-nodes))
    

    
    ;; --*-- TBC isolated cycle --*--
    
    (call-with-output-file *ba3m_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (displayln kmer out))
		  (map conv-path (reverse *ans*))))
      #:exists 'truncate/replace)
    #t
    ))

(define (in-out-count adjlist nodes)
  (map (lambda(node)(list node
			  (count (lambda(x)(equal? node (cadr x)))  adjlist)  ;; IN
			  (count (lambda(x)(equal? node (car x)))   adjlist)))  ;; OUT count
       nodes))


(define (conv-path nlist)
  (string-join (map number->string nlist) " -> "))
;;
;; chech  1-in-1-out node
;;
(define (node-1-1? node in-out)
      (equal? '(1 1)(cdr (assoc node in-out))))


