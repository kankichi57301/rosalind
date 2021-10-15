#lang racket
;; rosalind
;; Find an Eulerian Cycle in a Graph 
;; [BA3F] 2021/07/27 AC
;(require srfi/1)
(require srfi/13)
(require srfi/14)
(require "readfileA.ss")
(require "roslibA.ss")
(require (only-in "roslibB.ss" find-first))
(define *ba3f_out* "data\\ba3f_out.txt")
(define *dat* '())

(define (ros_ba3f . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba3f.txt"
		    (format "data\\rs_ba3f~a.txt" (car n)))))
	 )
    (set! *dat* data)
    (solve-ba3f data)
    
    
    (call-with-output-file *ba3f_out*
      (lambda(out)
	(displayln (solve-ba3f data) out))
      #:exists 'truncate/replace)
    #t
    
))

(define (solve-ba3f str)
  (let ((adj-list (extend-adj-list
		   (map parse-adj-list str)))
	(res '())
    )
    ;;(displayln (format "#ofEdge=~a" (length adj-list)))
    (set! res
	  (find-cycle adj-list))
    ;;(displayln (format "#ofEdge=~a" (length res)))
    (string-join
     (map number->string res)
     "->"
     )))

(define (extend-adj-list1 pair)
  (map (lambda(dst)(list (car pair) dst))(cadr pair)))

(define (extend-adj-list adj-list)
  (append-map extend-adj-list1 adj-list))
;;
;; '(A B X C D) '(X 1 2 3 X) ==> '(A B X 1 2 3 X C D)
(define (splice-loop main sub)
  (let ((pos (index-of main (car sub))))
    (append (take main pos)
	    sub
	    (drop main (+ 1 pos)))))

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

;(ros_ba3f 1)
