#lang racket
;; rosalind
;; Creating a Character Table 
;; [CTBL]
;; 2021/01/16 AC
;; 2021/10/14 AC
(require srfi/1)
(require srfi/13)
(require srfi/14)
(require "readfileA.ss")
(require "roslibA.ss")
(require "roslibB.ss")

(define *ros_ctbl_out* "data\\ctbl_out.txt")

(define *dist* #f)
(define *chars* #f)

(define (ros_ctbl . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ctbl.txt"
		    (format "data\\rs_ctbl~a.txt" (car n))))))
        
    (set! *chars* (make-hash))

    (register-tree (car data))
    (call-with-output-file *ros_ctbl_out*
      (lambda(out)
	(solve-ctbl out))
      #:exists 'truncate/replace)
    
  ))


(define (add-last lst item)
  (if (null? lst)
      (list (list item))
      (append (drop-right lst 1)(list (append (car (take-right lst 1))(list item))))))

(define (add-last-new lst )
  (append lst (list '())))

(define (newick-split0 lst acc lv)
 (if (empty? lst)
      acc
      (let ((c (car lst)))
	(case c
	  [(#\()(newick-split0 (cdr lst)(add-last acc c)(+ lv 1))]
	  [(#\))(newick-split0 (cdr lst)(add-last acc c)(- lv 1))]
	  [(#\,)
	   (if (= lv 0)
	       (newick-split0 (cdr lst)(add-last-new acc) lv )
	       (newick-split0 (cdr lst)(add-last acc c)   lv ))
	   ]
	  [else (newick-split0 (cdr lst)(add-last acc c)   lv )]
	  )
	
)))
	  
(define (newick-split str)
  (let ((str1 (string-drop-right (string-drop str 1) 1)))
    (map (lambda(x)(apply string x))
	 (newick-split0 (string->list str1 )'() 0))))


(define *node-counter* 1)
(define *parent* (make-hash))
(define *child*  #f)


(define (newick-tree? str)
  (and (string= ";" (string-take-right str 1))
       (newick-inner? (string-drop-right str 1))))


(define (newick-inner? str)
  (and (string= "(" (string-take       str 1))
       (string= ")" (string-take-right str 1))))


(define (new-inner-name)
  (let ((new-name (format "node~s" *node-counter*)))
    (set! *node-counter* (+ 1 *node-counter*))
    new-name
  ))

(define my-charset (char-set-delete char-set:graphic #\:))
;; all nodes
;; 2021/01/15 underscore
(define newick-node-pat #rx"^([A-Za-z0-9_(),]*)(:([0-9]+))?$")
;; inner node
;;(define newick-inner-pat #rx"^\\(.*\\):([0-9]+)$")

;; top level tree only

(define (register-tree-top str0)
    (begin

      (let ((str (string-drop-right str0 1))) ;;erase last semicoron
	(if (newick-inner? str)
	    (let ((name (new-inner-name))
		  (child (newick-split str))
		  )
	      
	      (hash-set! *child* name
			 (map (lambda(x)(register-tree0 x name)) child))
	  name
	  )
	#f
      ))))

(define (register-tree0 str parent)
    (let* ((node-list (regexp-match newick-node-pat str))
	   (body (cadr node-list))
	   (dist (if (cadddr node-list)
		     (string->number (caddr node-list))
		     #f))
	   )
      
      (if (newick-inner? body)
	(let ((name (new-inner-name))
	      (child (newick-split body))
	      )

	  (when parent
		(hash-set! *parent* name parent))

	  (hash-set! *child* name
		     (map (lambda(x)(register-tree0 x name)) child))
	  (when dist
		(hash-set! *dist* name dist))
	  name
	
	  )
	(begin
	  (when parent
		(hash-set! *parent* body parent))

	  (when dist
		(hash-set! *dist* body dist))
	  str
	)
      )))


(define (register-tree str)
  (set! *child* (make-hash))
  (set! *dist* (make-hash))
  (set! *node-counter* 1)
  (register-tree-top str)
  )


(define inner-node-pat #rx"node[0-9]+")


(define (display-char-table tbl len out)
  (for-each (lambda(x)
	      (for-each
	       (lambda(y)
		 (display (format "~a" (if (member x y) 1 0)) out)
		 ) tbl)
	      (display "\n" out)
	      )
	    (iota (- len 1) 2)
))

(define (dump-ctbl)
  (displayln (hash-map *child* list))
  
)

(define (add-char node char)
  (hash-set! *chars* node (cons char (hash-ref *chars* node '()))))


(define (add-one node n)
  (if (regexp-match inner-node-pat node)
      (let ((child (hash-ref *child* node)))
	(for-each (lambda(x)(add-one x n)) child))
      (add-char node n)))

(define *ctbl-res* #f)
;全ての内部ノードに対し、全ての葉がその内部ノードの下にある葉かそうでないかで１か０を付与する。
(define (solve-ctbl out)
  (set! *chars* (make-hash))
  (for-each (lambda(n)(add-one (format "node~a" n) n))
	    (iota (- *node-counter* 1) 1))
  
  (set! *ctbl-res* (sort
		    (hash-map *chars* list)
		    (lambda(x y)(string< (car x)(car y)))
		    ))

  (displayln (format "n=~s" *node-counter*))
  (display-char-table
   (map cadr *ctbl-res*) (- *node-counter* 1) out) 
  )
  
