#lang racket
;; rosalind
;; Quartets
;; [QRT]
;; 2021/01/17 WA
;; 2021/01/19 AC 重複データに注意　(a b)|(c d) and (c d)|(a b)
;; 2021/10/15 AC 
(require srfi/1)
(require srfi/13)
(require math/number-theory)
(require "readfileA.ss")
(require "roslibB.ss")

(define *ros_qrt_out* "data\\qrt_out.txt")

(define (ros_qrt . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_qrt.txt"
		    (format "data\\rs_qrt~a.txt" (car n)))))
	 (nodes (string-tokenize (car data)))
	 (table (cdr data))
	 )
    
    (call-with-output-file *ros_qrt_out* 
      (lambda(out)
	(for-each (lambda(x)(displayln x out))
		  (delete-duplicates
		   (append-map (lambda(x)(make-quartets nodes x)) table))))
      #:exists 'replace)
    
   ))

(define (m-nc2 x)
  (* (binomial (car x) 2)(binomial (cadr x) 2)))

(define (ros_qrt2)
  (let* ((data (read-file "_qrt1"))
	 (nodes (string-tokenize (car data)))
	 (table (cdr data))
	 )
    (apply +
	   (map (lambda(x)(m-nc2 (bin-count (string->list x)))) table))
   ))

(define (collect-position0 lst pred)
  (append-map (lambda(x)(if (pred (list-ref lst x))
			    (list x)
			    '()))
	      (iota (length lst))))

(define (collect-position lst val)
  (collect-position0 lst (lambda(x)(equal? val x))))


(define (make-quartets nodes chars)
  (let* ((charlist (string->list chars))
	 (pos0 (collect-position charlist #\0))
	 (pos1 (collect-position charlist #\1))
        )

    (map (lambda(x)(apply make-quartets0 nodes x))
    
	 (cartesian-product (combinations pos0 2)
			    (combinations pos1 2)))
    ))

(define (make-quartets0 nodes zero-pos one-pos)
  (if (< (car zero-pos)(car one-pos ))
      (format "{~a,~a}{~a,~a}"
	      (list-ref nodes (car zero-pos))
	      (list-ref nodes (cadr zero-pos))
	      (list-ref nodes (car one-pos ))
	      (list-ref nodes (cadr one-pos )))
      (format "{~a,~a}{~a,~a}"
	      (list-ref nodes (car one-pos ))
	      (list-ref nodes (cadr one-pos ))
	      (list-ref nodes (car zero-pos))
	      (list-ref nodes (cadr zero-pos)))))
