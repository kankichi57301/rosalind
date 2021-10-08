;; rosalind
;; Topological Sorting 
;; [BA5N] 2021/08/06 AC
(require srfi/1)
(require srfi/13)
(require srfi/14)
(require data/queue)
(require "readfileA.ss")
;(include "sepby.ss")
(define *ba5n_out* "ba5n_out.txt")

(define *myhash* #f)
(define *inlet*  '())

(define *result* '())
(define *myqueue* '())

(define (ros_ba5n . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba5n.txt"
		    (format "rs_ba5n~a.txt" (car n)))))
	 (graph (append-map edit++ data))
	 (n (apply max (apply append graph)))
	 (res '())
	 )
    ;(displayln graph)
    ;(displayln n)
    (set! res (string-join (map number->string(topological-sort graph n)) ", "))
    
    
    (call-with-output-file *ba5n_out*
      (lambda(out)
	(displayln res out))
	
      #:exists 'truncate/replace)
    res
))
(define (edit++ arg)
  (let ((x (map string->number (string-tokenize arg char-set:digit))))
    (map (lambda(y)`(,(car x) ,y))(cdr x))
    ))
    
;; --*-- tbc
;; (define (topological-sort graph n)
;; )
(define (ts-init graph n)
  (set! *inlet*  (make-list (+ n 1) 0))
  ;(set! *outlet* (make-list (+ n 1) 0))
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
					;(displayln *outlet*)
  (for-each (lambda(n)(when (= 0 (list-ref *inlet* n))
			    (enqueue! *myqueue* n)))
	    (iota (+ n 1)))
)

(define (topological-sort graph n)
  (ts-init graph n)
  (reverse (top-sort-main graph '()))
  )

(define (inc-in node)
  (set! *inlet*  (list-set *inlet* node (+ (list-ref *inlet* node) 1))))
(define (dec-in-push-queue-if-zero node)
  (let ((indegree (list-ref *inlet* node)))
    (when (= 1 indegree)
	  (enqueue! *myqueue* node))
    (set! *inlet*  (list-set *inlet* node (- indegree 1)))))

;(define (inc-out i)
;  (set! *outlet* (list-set *outlet* i (+ (list-ref *outlet* i) 1))))

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
	    
