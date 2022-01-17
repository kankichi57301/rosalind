#lang racket
;; rosalind
;; Find a Shortest Transformation of One Genome into Another by 2-Breaks 
;; [BA6D] 2022/01/15 AC
(require srfi/1)
(require srfi/13)
(require mzlib/string)
(require "readfileA.ss")
(require "roslibA.ss")
(require "roslibB.ss")

(define *ba6d_out* "data\\ba6d_out.txt")

(define (ros_ba6d . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba6d.txt"
		    (format "data\\rs_ba6d~a.txt" (car n)))))
	 (from (read-from-string (format "~a" (car data))))
	 (to   (read-from-string (format "~a" (cadr data))))
	 (res '())
	 )
    (displayln from)
    (displayln to)
    ;(displayln blocks) 
    ;(calc-cycles bp-graph )
    (set! res (map format-signed-nlist (cons from (transform-2break from to))))
    
    (call-with-output-file *ba6d_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (displayln (format "~a" kmer) out))
		  res))
      #:exists 'truncate/replace)
    res
    
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
  

;;‘o•ûŒü‚Ìassoc
(define (my-assoc val pairlist)
  (let ((it (find-first (lambda(x)(equal? (car x) val)) pairlist)))
    (if it
	(values it (cadr it))
	(let ((it2 (find-first (lambda(x)(equal? (cadr x) val)) pairlist)))
	  (if it2
	      (values it2 (car it2))
	      (values '() '())
	      )))))

(define (2break-dist synteny-list1 synteny-list2)
  (let ((nlist1 (cycle->num synteny-list1))
	(nlist2 (cycle->num synteny-list2)))
    ;(displayln nlist1)
    ;(displayln nlist2)
    (- (length nlist1)
       (calc-cycles
	(append nlist1 nlist2)))
))


;(2break-dist '(1 2 3 4) '(1 2 3 4))
(define (all-2breaks from)
  (map (lambda(y)(2break-span from (car y)(cadr y)))(dup-combi (length from))))

(define (dec-2break-dist from to)
  (let ((dist (2break-dist from to)))
    (dec-2break-dist0 from to (- dist 1))))

(define (dec-2break-dist0 from to dist)
  (find-first
   (lambda(x)
     (= dist (2break-dist x to)))
   (all-2breaks from)))

(define (dup-combi n)
  (filter (lambda(x)(<= (car x)(cadr x)))(cartesian-product (reverse (iota n))(iota n))))

(define (2break-span nlist pos1 pos2)
  (append (take nlist pos1)
	  (map - (reverse (drop-right (drop nlist pos1)(- (length nlist) pos2 1))))
	  (take-right nlist (- (length nlist) pos2 1))))




(define (transform-2break from to)
  (if (= 1 (2break-dist from to))
      (list to)
      (let ((next (dec-2break-dist from to)))
	(cons next 
	      (transform-2break next to)))))

(define (format-signed-nlist nlist)
  (map (lambda(n)(if (> n 0)
		     (format "+~a" n)
		     (format "~a" n)))
       nlist))
	      
(ros_ba6d 2)
