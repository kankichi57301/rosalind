#lang racket
;; rosalind
;;Find the Longest Substring Shared by Two Strings
;; [BA9E] 2021/09/
;(require srfi/1)
(require racket/hash)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(require suffixtree)
(define *ba9e_out* "ba9d_out.txt")
(define my-tree1 '())
(define my-tree2 '())
(define my-hash1  '())
(define my-hash2  '())

(define (ros_ba9e . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba9e.txt"
		    (format "rs_ba9e~a.txt" (car n)))))
	 )
    
    #|
    (call-with-output-file *ba9e_out*
      (lambda(out)
	(displayln
	 (longest-multiple (tree-root my-tree))
	 out
	 ))
      #:exists 'truncate/replace)
    |#
    
    
    ))


(define (hash-all-nodes node my-hash)
  (hash-all-nodes0 node "" my-hash))

(define (hash-all-nodes0 node acc my-hash)
  (let* ((children (node-children node))
	 (lab (format "~a~a" acc (label->string (node-up-label node))))
	 (kaisuu (if (null? children)
		     1
		     (apply + (map (lambda(x)(hash-all-nodes0 x lab my-hash)) children)))))

    (hash-set! my-hash lab kaisuu)
    
    kaisuu))
	 
