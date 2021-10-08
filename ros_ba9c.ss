#lang racket
;; rosalind
;; Construct the Suffix Tree of a String
;; [BA9C] 2021/09/12 AC

(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(require suffixtree)
(define *ba9c_out* "ba9c_out.txt")
(define my-tree '())

(define (ros_ba9c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba9c.txt"
		    (format "rs_ba9c~a.txt" (car n)))))
	 (root '())
	 )
    (set! my-tree (make-tree))
    (tree-add! my-tree (string->label (car data)))
    (set! root (tree-root my-tree))

    
    (call-with-output-file *ba9c_out*
      (lambda(out)
	(print-all-nodes root out))
      #:exists 'truncate/replace)

    #t
    
    ))

(define (print-all-nodes node out)
  (let ((label (label->string (node-up-label node))))
    (when (non-empty-string? label) 
	  (displayln label out)))
  (for-each (lambda(node)(print-all-nodes node out)) (node-children node)))
	     



