#lang racket
;; rosalind
;; Reconstruct a String from its k-mer Composition
;; [BA3J] 2021/07/30 AC
;; 2021/10/17 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibB.ss")

(define *ba3h_out* "data\\ba3h_out.txt")

(define (ros_ba3h . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba3h.txt"
		    (format "data\\rs_ba3h~a.txt" (car n)))))
	 (k (string->number (car data)))
	 (edges (cdr data))
	 (adj-list (map str2edge edges))
	 (res '())
	 )
    (set! res (con-all-kmer (apply find-path (cons adj-list (calc-start-end adj-list)))))
    
    
    (call-with-output-file *ba3h_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)
    res
))

(define (str2edge str)
  (list (string-drop-right str 1)
	(string-drop str 1)))


(define (con-all-kmer strlist)
  (apply string-append (cons (car strlist)
		       (map (lambda(x)(string-take-right x 1))(cdr strlist)))))


(define (find-path adj-list start end)
  (call-with-values (lambda()(find-path0 adj-list (list start) end))
    (lambda(acc rest)
      ;(displayln (format "~a::~a" acc rest))
      (find-cycle1 rest acc))))
    
    

(define (find-path0 adj-list acc end)
  ;(displayln (format "acc=~s" acc))
  (let ((next-edge (find-first (lambda(edge)(equal? (car acc)(car edge)))
			       adj-list
			       )))
      ;(displayln (format "next=~s" next-edge))
      (if (equal? next-edge end)
	  (values (reverse acc) adj-list)
	  (find-cycle0 (remv next-edge adj-list)(cons (cadr next-edge) acc)))))

(define (calc-start-end adj-list)
  (let ((s-e
	 (map car
	      (filter (lambda(x)(odd? (length x)))(group-by identity (apply append adj-list))))))
    (if (start-node? adj-list (car s-e))
	s-e
	(reverse s-e))))

(define (find-cycle adj-list )
  (call-with-values (lambda()(find-cycle0 adj-list (list (caar adj-list))))
    (lambda(acc rest)
      (find-cycle1 rest acc))))
    
    

(define (find-cycle0 adj-list acc)
  ;(displayln (format "acc=~s" acc))
  (let ((next-edge (find-first (lambda(edge)(equal? (car acc)(car edge)))
			       adj-list
			       )))
      ;(displayln (format "next=~s" next-edge))
      (if next-edge
	  (find-cycle0 (remv next-edge adj-list)(cons (cadr next-edge) acc)) ;; not remove (remv)
	  (values (reverse acc) adj-list))))

  
(define (find-cycle1 adj-list acc)
  ;(displayln (format "acc=~s" acc))
  (if (empty? adj-list)
      acc
      (let* ((rest-start (map car adj-list))
	     (next (find-first (lambda(node)(member node rest-start))
			       (reverse acc))))
	;(displayln (format "next=~a" next))
	
	(call-with-values (lambda()(find-cycle0 adj-list (list next)))
	  (lambda(nextloop rest)
	    ;(displayln (format "next=~a:rest=~a" nextloop rest))
	    ;(displayln (format "marged=~a" (splice-loop acc nextloop)))
	    (find-cycle1 rest (splice-loop acc nextloop))
	    )))))


(define (start-node? adj-list node)
  (> (count (lambda(x)(equal? node (car x)))  adj-list)
     (count (lambda(x)(equal? node (cadr x))) adj-list)))

;;
;; '(A B X C D) '(X 1 2 3 X) ==> '(A B X 1 2 3 X C D)
(define (splice-loop main sub)
  (let ((pos (index-of main (car sub))))
    (append (take main pos)
	    sub
	    (drop main (+ 1 pos)))))
