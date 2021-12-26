(module rosalind racket/base
	(provide (all-defined-out))
	(require (only-in racket/list empty? append-map))
	(require "sft-misc.ss")
;;
;;suffix-treeC.ss
;;
(require srfi/13)
(require suffixtree)
(require "roslibB.ss")

(define (nodestr x) (label->string (node-up-label x)))
(define *tree* '())

;;(define *node-path* '())
(define *root* '())
(define *res* '())

;; find str approximate
;; d mismatch allowance
(define (find-str-apx obj search d)
  ;;(when (empty? *tree*)
  (set! *tree* (make-tree))
  (tree-add! *tree* (string->label obj))
  (set! *root* (tree-root *tree*))
	
  (set! *res* '())
  (find-str-apx0 *root* search d '())
  (append-map (lambda(x)(calc-pos obj x)) *res*)
  )

(define (find-str-apx0 node str d acc)
  ;;(displayln (format ":sear=~a" str))
  (let ((childs (node-children node))
	(strlen (string-length str))
	)
    
    (if (null? childs)
	#f
	(for-each (lambda(child)
		    (let* ((cur (nodestr child))
			   (curlen (string-length cur)))
		      ;(displayln (format "find=~a" cur))   
		      (if (<= strlen curlen)
			  (if (str-apx-match str (string-take cur strlen) d)
			      (set! *res* (cons (cons child acc) *res*))
			      #f)
			  (let ((d2 (str-apx-match (string-take str curlen) cur d)))
			    (if d2
				(find-str-apx0 child (string-drop str curlen) d2 (cons child acc))
				#f))))) 
		  childs))))
;;		     
;;--*- TBC --*--
	
;; node‚Ìæ“ª‚©‚çÅŒã‚Ì•¶Žš‚Ü‚Å‚Ì”
(define (node-pos tree node)
  (let ((mylen (string-length (nodestr node)))
	(child (node-children node)))
    (if (empty? child)
	(list mylen)
	(map (lambda(n)(+ n mylen))
	     (append-map (lambda(x)(node-pos tree x)) child)))))
  


(define (find-str-pos-apx-M str searlist d)
  (set! *tree* '())
  (for-each (lambda(sear)
		(find-str-apx str sear d))
	      searlist))




(define stra "ACGACTACCACA$")

;; (find-str-apx stra "ACT" 1)

(define (dump-res)
  (for-each (lambda(ans)(displayln (map nodestr (reverse ans)))) *res*))

#|
(define (find-str-pos-apx obj search d)
  (set! *node-path* '())
  (find-str-apx obj search d)
|#

(define (calc-pos obj node-path)
  (if (empty? node-path)
      '()
      (let ((len2 (apply + (map (lambda(x)(label-length (node-up-label x)))(cdr node-path))))
	    (alllen (string-length obj))
	    )
	(map (lambda(x)(- alllen (+ len2 x)))
	     (node-pos *tree* (car node-path))))))



(define (find-str-pos-M str searlist d)
  (set! *tree* '())
  (sort
   (append-map (lambda(sear)
		 (find-str-apx str sear d))
	       searlist)
   <))



;;
;;--*-- end of module --*--
)
