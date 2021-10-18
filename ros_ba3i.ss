#lang racket
;; rosalind
;; Find a k-Universal Circular String 
;; [BA3I] 2021/07/29 AC
;; 2021/10/17 AC
(require srfi/13)
(require srfi/14)
(require "readfileA.ss")
(require "roslibA.ss")
(require "roslibB.ss")

;;(include "ros_ba3f.ss")

(define *ba3i_out* "ba3i_out.txt")


(define (ros_ba3i . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba3i.txt"
		    (format "data\\rs_ba3i~a.txt" (car n)))))
	 (res (solve-ba3i (string->number (car data))))
	 )
    
    
    (call-with-output-file *ba3i_out*
      (lambda(out)
	(display res out))
		  
      #:exists 'truncate/replace)
    res
))

(define (all-binary n)
  (map (lambda(x)(apply string x))
       (cartesian-expt '(#\0 #\1) n)))

(define (make-edge binstr)
  (let ((a0 (format "0~a" binstr))
	(a1 (format "1~a" binstr))
	(b0 (format "~a0" binstr))
	(b1 (format "~a1" binstr)))
    `((,a0 ,b0)
      (,a0 ,b1)
      (,a1 ,b0)
      (,a1 ,b1))))

(define (make-all-edges n)
  (append-map make-edge (all-binary n)))

(define (solve-ba3i n)
  (make-loop
   (find-cycle (make-all-edges (- n 2)))
   n))
  

(define (make-loop eu-cycle n)
  (apply string-append (cons (car eu-cycle)
		       (map (lambda(s)(string-take-right s 1))
			    (drop-right (cdr eu-cycle) (- n 1))))))

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

(define (splice-loop main sub)
  (let ((pos (index-of main (car sub))))
    (append (take main pos)
	    sub
	    (drop main (+ 1 pos)))))
