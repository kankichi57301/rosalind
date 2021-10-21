(module rosalind racket/base
	(provide (all-defined-out))
	(require (only-in racket/list list-set))
	(require (only-in racket/function identity))
;;	      
;;filename:suffix-tree2A.ss
;;
;;Invoked from ros_mrep.ss
;;	
(require srfi/1)
(require srfi/13)
(require "roslibA.ss")
(require "roslibB.ss")

(define (node-pair? x)
  (and (list? x)
       (number? (car x))
       (number? (cadr x))))

(define (matched-len pair1  pair2)
  (matched-len0 (car pair1) (cadr pair1)(car pair2) (cadr pair2)))


(define (matched-len0 start1 len1 start2 len2)
  (if (or (= 0 len1)(= 0 len2)
	  (not (equal? (list-ref *dna* start1)(list-ref *dna* start2))))
	  0
	  (+ 1 (matched-len0 (+ 1 start1)(- len1 1)(+ 1 start2)(- len2 1)))))

;; 第１文字が合致しているか
(define (first-match pair1  pair2)
  (equal? (list-ref *dna* (car pair1))
	  (list-ref *dna* (car pair2))))
		    
(define (init-suffix-tree len)
  (list (list 0 len)))

(define (pair-drop start-len-pair n)
  (list (+ (car  start-len-pair) n)
	(- (cadr start-len-pair) n)))
(define (pair-take start-len-pair n)
  (list (car  start-len-pair) 
	n))
;;pairは*DNA*の開始位置と長さの組
;;pairのリストならcarを返す。pairならそのまま
(define (car0A x)
  (if (node-pair? x)
      x
      (car  x)))
(define (add-one0 x)
  (if (number? x)
      (+ 1 x)
      x))
;;
;; top level only
;;
(define (add-suffix-tree tree pair)
  (let ((pos (find-first-pos
	      (lambda(x)(> (matched-len (car0A x) pair)
			   0))
	      tree)))
    ;(displayln (format ">pos=~s" pos))
    (if pos
	(list-set tree pos (add-suffix-tree2 (list-ref tree pos) pair))
	(append tree `(,pair)))
  ))

  

(define (add-suffix-tree2 tree pair)
;(displayln (format ":tree=~s add=~s" tree pair))
  (if (node-pair? tree)
      ;; --*-- dst string start & len pair--*--	
      (let* ((mlen (matched-len tree pair))
	     (toplen (cadr tree)))
	;(displayln (format "*AAA*mlen=~a toplen=~a" mlen toplen))
	(cond
	 [(= 0 mlen)
	    (list 
		  (list tree)
		  (list pair))
	 ]
	 [ (< mlen toplen)
	    (list (list (min(car pair)(car tree)) mlen) ;;(pair-take tree mlen) ;; 2021/01/30
		  (pair-drop tree mlen)
		  (pair-drop pair mlen))
	 ]
	 [else
	    (let ((listrest (list(pair-drop pair mlen))))
	      (list tree listrest)
	    )
	 ]
	))		
     ;; --*-- dst pair list--*--	
      (let* ((mlen (matched-len (car tree) pair))
	     (top  (car tree))
	     (toplen (cadr top)))
;(displayln (format "mlen=~s toplen=~s" mlen toplen))
	(cond
	 [(= 0 mlen)
	  (append
		   (cons (pair-drop top mlen)
			      (cdr tree))
		   (list (pair-drop pair mlen)))
	 ]
	 [(< mlen toplen)
	  
	    (append   (list (pair-take top mlen))
		      (list (cons (pair-drop top mlen)
				(cdr tree)))
		      (list (list (pair-drop pair mlen))))
	 ]   
	 [  (let* ((pairrest (pair-drop pair mlen))
		   (pos (add-one0 (find-first-pos
				   (lambda(x)(first-match (car0A x) pairrest))(cdr tree)))))

	      (if pos
		  (list-set tree pos (add-suffix-tree2 (list-ref tree pos) pairrest))
		  (append tree (list (pair-drop pair mlen)))
	      )
	    )
         ])
)))

;;--*-- test data --*--
(define *dna0* "ATATAGG$")
(define *dna* (string->list *dna0*))

(define tree0 (init-suffix-tree (length *dna*)))
(define tree1 (add-suffix-tree tree0 '(1 5)))
(define tree2 (add-suffix-tree tree1 '(2 4)))
(define tree3 (add-suffix-tree tree2 '(3 3)))
(define tree4 (add-suffix-tree tree3 '(4 2)))
(define tree5 (add-suffix-tree tree4 '(5 1)))

(define (all-suffix-tree)
  (let* ((len (length *dna*))
	 (tree (init-suffix-tree len)))
    (for-each
     (lambda(x)
       (set! tree (add-suffix-tree tree (list (- len x) x)))
       ;(when (= 0 (modulo x 10))(displayln x))
       ;(when (= 99 (modulo x 100)) (collect-garbage 'incremental))
       )
	      (iota (- len 1) 1))
    tree))
  

(define (conv-pair-str pair)
  (let ((start (car pair))
	(len   (cadr pair)))
    (if (= 0 start)
	(string-take *dna0* len)
	(string-take (string-drop *dna0*  start ) len))))

(define (conv-tree pairtree)
  (cond
   [(null? pairtree) '()]
   [(node-pair? pairtree)
    (conv-pair-str pairtree)]
   [else
      (cons (conv-tree (car pairtree))
	    (conv-tree (cdr pairtree)))
   ]))

(define (count-distinct-substring str)
  (set! *dna* (string->list str))
  (count-distinct-substring0 (all-suffix-tree) (length *dna*))
)
(define (count-distinct-substring0 suffix-tree-pair totallen)
  (cond
   [(null? suffix-tree-pair) 0]
   [(node-pair? suffix-tree-pair)
    (length-pair suffix-tree-pair totallen)]
   [else
      (+ (count-distinct-substring0 (car suffix-tree-pair) totallen)
	 (count-distinct-substring0 (cdr suffix-tree-pair) totallen))]
  ))
;
;length of pair (except last char ($))
;
(define (length-pair pair totallen)
  (let ((start (car pair))
	(len   (cadr pair)))
    (if (= (+ start len) totallen)
	(- len 1)
	len)))
    
(define (make-suffix-tree str)
  (set! *dna0* str)
  (set! *dna* (string->list *dna0*))
  (all-suffix-tree)
)

(define (get-start tree)
  (if (string? tree)
      tree
      (get-start (car tree))))

(define (multiple-longest suffix-tree)
  (max-item (map multiple-longest-sub suffix-tree) string-length))

(define (multiple-longest-sub suffix-tree)
  ;(displayln (format ":arg=~s" (conv-tree suffix-tree)))
  (if (node-pair? suffix-tree)
      ""
      (if (= 1 (length suffix-tree))
	  ""
	  (if (andmap node-pair? (cdr suffix-tree))
	      (conv-tree  (car suffix-tree))
	      (let* ((res (map multiple-longest0 (cdr suffix-tree )))
		     (pos (max-index-of res identity))
		     (subtree (list-ref suffix-tree (+ 1 pos)))
		 )
		(string-append
		 (conv-tree  (car suffix-tree))
		 (multiple-longest-sub  subtree))
		)))))

    
(define (multiple-longest0 suffix-tree)
  
  (if (node-pair? suffix-tree)
      0
      (if (= 1 (length suffix-tree))
	  0
	  (+ (cadar suffix-tree)
	     (apply max (map multiple-longest0 (cdr suffix-tree)))))))

;;
(define (multiple-longest-str str)
  (multiple-longest (make-suffix-tree str)))

(define *maximal-repeat-count* 0)
(define *maximal-repeat* #f)

(define (maximal-repeat str minlen)
  (set! *maximal-repeat-count* 0)
  (set! *maximal-repeat* (make-hash))
  (for-each (lambda(x)(maximal-repeat0 x minlen 0 ""))(make-suffix-tree str))
  (hash-map *maximal-repeat* (lambda(x y)y))
  )

#|
(define (maximal-repeat0 suffix-tree minlen acc accstr)
  (when (and (not (node-pair? suffix-tree))
	     (> (length suffix-tree) 1))
	(let ((slen (+ acc (cadar suffix-tree))))
	  (if (andmap node-pair? (cdr suffix-tree))
	      (when (>= slen minlen)
		    (begin 
		      (hash-set! *maximal-repeat* *maximal-repeat-count* (string-append accstr (conv-tree (car suffix-tree))))
		      (set! *maximal-repeat-count* (+ 1 *maximal-repeat-count*))
		    )
	      )
	      (for-each (lambda(x)(maximal-repeat0 x minlen slen (string-append accstr (conv-tree (car suffix-tree)))))
		      (cdr suffix-tree))))))
|#
(define (maximal-repeat0 suffix-tree minlen acc accstr)
  (when (and (not (node-pair? suffix-tree))
	     (> (length suffix-tree) 1))
	(let ((slen (+ acc (cadar suffix-tree))))
	  (begin
	      (when (>= slen minlen)
		    (begin 
		      (hash-set! *maximal-repeat* *maximal-repeat-count* (string-append accstr (conv-tree (car suffix-tree))))
		      (set! *maximal-repeat-count* (+ 1 *maximal-repeat-count*))
		    )
	      )
	      (for-each (lambda(x)(maximal-repeat0 x minlen slen (string-append accstr (conv-tree (car suffix-tree)))))
		      (cdr suffix-tree))))))
  
(define str0 "TAGAGATAGAAT$")
(define aaa (make-suffix-tree str0))
(define str1 "TAGAGATAGAATGGGTCCAGAGTTTTGTAATTTCCATGGGTCCAGAGTTTTGTAATTTATTATATAGAGATAGAATGGGTCCAGAGTTTTGTAATTTCCATGGGTCCAGAGTTTTGTAATTTAT$")
(define ans1 (maximal-repeat str1 20))

(define (allpos str1 str2 pos acc)
  (let* ((res (string-contains str1 str2 pos)))
    (if (not res)
	acc
	(allpos str1 str2 (+ 1 res) (cons res acc)))))

(define (string-ref* str num)
  (if (< num 0)
      #\^
      (string-ref str num)))

(define (all-prev-positions str1 str2)
  (map (lambda(x)(string-ref* str1 (- x 1)))
       (reverse (allpos str1 str2 0 '()))))

(define (all-equal? lst)
  (if (= 2 (length lst))
      (apply equal? lst)
      (and (apply equal? (take lst 2))
	   (all-equal? (cdr lst)))))

(define (maximal-repeated-string str minlen)
  (filter (lambda(x)(not (all-equal? (all-prev-positions str x))))(maximal-repeat str minlen)))

)
