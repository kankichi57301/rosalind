#lang racket
;; rosalind
;; Find the Longest Path in a DAG
;; [BA5D] 2021/12/04 AC
(require srfi/1)
(require srfi/13)
(require data/queue)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *myqueue* '())
(define *result* '())
(define *inlet*  '())
(define *dp* #f)
(define *tb* #f)
(define *ba5d_out* "data\\ba5d_out.txt")

(define (ros_ba5d . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba5d.txt"
		    (format "data\\rs_ba5d~a.txt" (car n)))))
	 (source (string->number (car data)))
	 (sink   (string->number (cadr data)))
	 (dag (map parse-adj-list2 (cddr data)))
	 (tpsorted (topological-sort (map (lambda(x)(take x 2)) dag) sink))
	 (maxlen 0)
	 (longest-path '())
	 (ans "")
	 )
    (displayln tpsorted)
    (set! *dp* (make-hash))
    (set! *tb* (make-hash))
    (hash-set! *dp* source 0)
    (fill-dp tpsorted dag)
    (set! maxlen (hash-ref *dp* sink))
    (displayln (format "maxlen=~a" maxlen))    
    (set! longest-path (trace-back source sink))
    (set! ans (string-join (map number->string longest-path) "->"))
    (displayln (format "longest path=~a" ans))
    
    (call-with-output-file *ba5d_out*
      (lambda(out)
	(displayln maxlen out)
	(displayln ans    out)
	)
      #:exists 'truncate/replace)
    
    
))

(define (parse-adj-list2 str)
  (map string->number
       (cdr (regexp-match #rx"([0-9]+)->([0-9]+):([0-9]+)" str))))


(define (topological-sort graph n)
  (ts-init graph n)
  (reverse (top-sort-main graph '()))
  )

(define (top-sort-main graph result)
  (if (= 0 (queue-length *myqueue*))
      result
      (let ((node (dequeue! *myqueue*)))
	;(displayln (format "deq ~a" node))
	(map (lambda(node)(dec-in-push-queue-if-zero node))
	 (map cadr (filter (lambda(edge)(= node (car edge))) graph)))
	(top-sort-main graph (cons node result))
      )
  ))

(define (ts-init graph n)
  (set! *inlet*  (make-list (+ n 1) 0))
  
  (set! *myqueue* (make-queue))
  (set! *result* '())
  
  (for-each
   (lambda(edge)
     (let ((out (car  edge))
	   (in  (cadr edge))
	   )
       ;(displayln edge)
       (inc-in   in)
    ))
   graph)
  
					;(for-each (lambda(edge)(displayln edge)) graph)
  
					;(displayln *inlet*)

  (for-each (lambda(n)(when (= 0 (list-ref *inlet* n))
			    (enqueue! *myqueue* n)))
	    (iota (+ n 1)))
)

(define (inc-in node)
  (set! *inlet*  (list-set *inlet* node (+ (list-ref *inlet* node) 1))))

(define (dec-in-push-queue-if-zero node)
  (let ((indegree (list-ref *inlet* node)))
    (when (= 1 indegree)
	  (enqueue! *myqueue* node))
    (set! *inlet*  (list-set *inlet* node (- indegree 1)))))


(define (fill-dp tpsorted dag)
  (for-each (lambda(node)(fill-dp0 node dag)) (cdr tpsorted)))

(define (fill-dp0 n dag)
  ;;(displayln (format "n=~a" n))
  (let ((nodes-to-n (filter (lambda(arc)(equal? n (cadr arc))) dag)))
    (when (not (null? nodes-to-n))
	  (for-each (lambda(node)(let ((it (hash-ref *dp* (car node) #f)))
				   ;;(displayln (format "arc=~a" nodes-to-n))
				   (when it
					 (let ((lensum (+ (caddr node) it)))
					   (when (> lensum (hash-ref *dp* n 0))
						 (hash-set! *dp* n lensum)
						 (hash-set! *tb* n (car node))
						 )))))
		    nodes-to-n))))


(define (trace-back source sink)
  (trace-back0 source sink '()))

(define (trace-back0 source sink acc)
  (if (equal? source sink)
      (cons source acc)
      (trace-back0 source  (hash-ref *tb* sink) (cons sink acc))))
