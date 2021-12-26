(module rosalind racket/base
	(provide (all-defined-out))
	(require (only-in racket/list empty? append-map))
;;
;;suffix-treeB.ss
;;
(require srfi/13)
(require suffixtree)
(require "roslibB.ss")

(define (nodestr x) (label->string (node-up-label x)))
(define *tree* '())

(define *result* '())
(define *root* '())
(define *node-path* '())
(define (find-str obj search)
  (when (empty? *tree*)
	(set! *tree* (make-tree))
	(tree-add! *tree* (string->label obj))
	(set! *root* (tree-root *tree*)))
  (find-str0 *root* search)

  ;(map nodestr (reverse  *node-path*))
  )

(define (find-str0 node str)
  ;;(displayln (format ":sear=~a" str))
  (let ((child (node-children node)))
    (if (null? child)
	(begin
	  (set! *node-path* '())
	  #f)
	(let ((match1-child (find-first (lambda(node)(equal? (string-ref str 0)(string-ref (nodestr node) 0)))
					child)))
	  (if (not match1-child)
	      (begin
		(set! *node-path* '())
		#f
		)
	      (let ((cur-str (nodestr match1-child)))
		;(displayln (format "find str=~a" cur-str))
		(if (string-prefix? str cur-str)
		    (begin
		      (set! *node-path* (cons match1-child *node-path*))
		      #t)
		    (if (string-prefix? cur-str str)
			(begin
			  (set! *node-path* (cons match1-child *node-path*))
			  (find-str0 match1-child (string-drop str (string-length cur-str))))
			(begin
			  (set! *node-path* '())  ;; 2021/12/24 bug‚ÌŒ´ˆö
			#f)))))))))

	
;; node‚Ìæ“ª‚©‚çÅŒã‚Ì•¶š‚Ü‚Å‚Ì”
(define (node-pos tree node)
  (let ((mylen (string-length (nodestr node)))
	(child (node-children node)))
    (if (empty? child)
	(list mylen)
	(map (lambda(n)(+ n mylen))
	     (append-map (lambda(x)(node-pos tree x)) child)))))
  

#|
(define (find-str-pos obj search)
  (find-str obj search)
  (list
   (node-pos *tree* (car *node-path*))
   (apply + (map (lambda(x)(string-length (nodestr x)))(cdr *node-path*)))))
|#
(define (find-str-pos obj search)
  (set! *node-path* '())
  (find-str obj search)
  (if (empty? *node-path*)
      '()
      (let ((len2 (apply + (map (lambda(x)(label-length (node-up-label x)))(cdr *node-path*))))
	    (alllen (string-length obj))
	    )
	(map (lambda(x)(- alllen (+ len2 x)))
	     (node-pos *tree* (car *node-path*))))))


(define str1 "ACGTACGTACGTACGT$")
(define sear "GA")
(define searlist '("AC" "CG"))
;;(find-str-pos str1 sear)

(define (find-str-pos-M str searlist)
  (set! *tree* '())
  (append-map (lambda(sear)
		(set! *node-path* '())
		(find-str-pos str sear))
	      searlist))


)
