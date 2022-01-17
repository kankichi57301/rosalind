(module rosalind racket/base
	(provide find-cycles)
	(require (only-in racket/list empty?))
	(require "roslibB.ss")

(define (find-cycles graph)
  (let ((fst (caar graph)))
    (find-cycles0 graph `((,fst)) fst fst)))

(define (my-caar x)
  (if (empty? x)
      '()
      (caar x)))
#|
(define (find-cycles0 graph acc next start)
  (display   (format "acc=~a " acc))
  (displayln (format "node=~a" next))
  (if (empty? graph)
      acc
      (let*-values ([(findarc nextnode)(my-assoc next graph)]
		    [(restgraph) (remove findarc graph)])
	     
	(if (= start nextnode)
	    (find-cycles0 (remove findarc graph) (cons (list (my-caar restgraph)) acc)     (my-caar restgraph) (my-caar restgraph))
	    (find-cycles0 (remove findarc graph) (cons (cons nextnode (car acc))(cdr acc))  nextnode        start         )
	    ))))
|#
(define (find-cycles0 graph acc next start)
  ;;(display   (format "acc=~a " acc))
  ;;(displayln (format "node=~a" next))
      (let*-values ([(findarc nextnode)(my-assoc next graph)]
		    [(restgraph) (remove findarc graph)])
	(if (empty? restgraph)
	    acc
	    (if (= start nextnode)
		(find-cycles0 (remove findarc graph) (cons (list (my-caar restgraph)) acc)     (my-caar restgraph) (my-caar restgraph))
		(find-cycles0 (remove findarc graph) (cons (cons nextnode (car acc))(cdr acc))  nextnode        start         )
		))))  


(define (my-assoc val pairlist)
  (let ((it (find-first (lambda(x)(equal? (car x) val)) pairlist)))
    (if it
	(values it (cadr it))
	(let ((it2 (find-first (lambda(x)(equal? (cadr x) val)) pairlist)))
	  (if it2
	      (values it2 (car it2))
	      (values '() '())
	      )))))
)
;;(define test1 (shuffle '((1 2)(2 3)(3 4)(4 5)(5 1)(6 7)(7 8)(8 9)(9 10)(10 6))))
;;(define test1 '((5 1) (1 2) (9 10) (4 5) (8 9) (6 7) (10 6) (2 3) (7 8) (3 4)))
