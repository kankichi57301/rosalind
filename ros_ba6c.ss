#lang racket
;; rosalind
;; Find a Longest Common Subsequence of Two Strings
;; [BA6C] 2022/01/14 AC
(require srfi/1)
(require srfi/13)
(require mzlib/string)
(require "readfileA.ss")
(require "roslibA.ss")
(require "roslibB.ss")

(define *ba6c_out* "ba6c_out.txt")

(define (ros_ba6c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba6c.txt"
		    (format "data\\rs_ba6c~a.txt" (car n)))))
	 (from (read-from-string (format "(~a)" (car data))))
	 (to   (read-from-string (format "(~a)" (cadr data))))
	 (bp-graph (append (append-map cycle->num from)
			   (append-map cycle->num to  )))
	 (blocks (length (flatten from)))
	 (node-c (* 2 blocks)) 
	 )
    ;(displayln bp-graph)
    ;(displayln node-c)
    ;(displayln blocks) 
    ;(calc-cycles bp-graph )
    (- blocks (calc-cycles bp-graph))
    #|
    (call-with-output-file *ba6c_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))

(define (synt->num0 num)
  `(,(- (* 2 num) 1) ,(* 2 num)))

(define (synt->num1 num)
  (if (> num 0)
      (synt->num0 num)
      (reverse (synt->num0 (- num)))))


(define (cycle->num numlist)
  (let ((nums (append-map synt->num1 numlist)))
    (group-per
     (cons (car (take-right nums 1))(drop-right nums 1))
     2)
     ))

;;;--*-- TBC --*--
;; arg:
;; graph =list of number pairs(arcs)
;;result:
;; # of cycles
;;

(define (calc-cycles graph)
  (calc-cycles0 graph 0 (caar graph)(caar graph)))

(define (my-caar x)
  (if (empty? x)
      '()
      (caar x)))

(define (calc-cycles0 graph acc next start)
  ;;(displayln (format "node=~a" next))
  (if (empty? graph)
      acc
      (let*-values ([(findarc nextnode)(my-assoc next graph)]
		    [(restgraph) (delete-once findarc graph)])
	     
	(if (= start nextnode)
	    (calc-cycles0 (delete-once findarc graph) (+ 1 acc) (my-caar restgraph) (my-caar restgraph))
	    (calc-cycles0 (delete-once findarc graph) acc       nextnode        start         )
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

(define (2break-dist nlist1 nlist2)
  ;(- (length nlist1)
  (calc-cycles
      (append (cycle->num nlist1)
	      (cycle->num nlist2)))
;  )
)

(2break-dist '(1 2 3 4) '(1 2 3 4))
