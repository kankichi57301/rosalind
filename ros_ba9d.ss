#lang racket
;; rosalind
;; Find the Longest Repeat in a String
;; [BA9D] 2021/09/13 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(require suffixtree)
(define *ba9d_out* "ba9d_out.txt")
(define my-tree '())
(define my-hash '())

(define (ros_ba9d . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba9d.txt"
		    (format "rs_ba9d~a.txt" (car n)))))
	 )
    (set! my-tree (make-tree))
    (tree-add! my-tree (string->label (format "~a$"  (car data))))
    (set! my-hash (make-hash))
    
    
    
    (call-with-output-file *ba9d_out*
      (lambda(out)
	(displayln
	 (longest-multiple (tree-root my-tree))
	 out
	 ))
      #:exists 'truncate/replace)
    #t
    
    ))

(define (longest-multiple node)
  (string-times node "")
  (max-item
   (hash-keys my-hash)
  string-length)
  )

(define (string-times node acc)
  (let* ((children (node-children node))
	 (lab (format "~a~a" acc (label->string (node-up-label node))))
	 (kaisuu (if (null? children)
		     1
		     (apply + (map (lambda(x)(string-times x lab)) children)))))
    (when (> kaisuu 1)
    (hash-set! my-hash lab kaisuu)
    )
    kaisuu))
	 
