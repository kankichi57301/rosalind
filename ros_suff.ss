#lang racket
;; rosalind
;; Encoding Suffix Trees
;; [SUFF]
;; 2021/01/14 AC
;; 2021/10/15 AC 
(require srfi/1)
(require srfi/13)
(require srfi/19)
(require "readfileA.ss")
(require "roslibB.ss")
(define *time* #f)

(define *ros_suff_out* "data\\suff_out.txt")

(define (ros_suff . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_suff.txt"
		    (format "data\\rs_suff~a.txt" (car n))))))
    (set! *time* (current-time))
    (call-with-output-file *ros_suff_out*
      (lambda(out)
	(for-each (lambda(x)(displayln (format "~a" x) out))
		  (flatten (solve_suff (car data)))))
      #:exists 'replace
    )
    (displayln (format "elapsed =~a sec" (time-second (time-difference (current-time) *time*))))
    
   ))


(define (match-prefix-len0 lst1 lst2)
  (if (or (null? lst1)(null? lst2)(not (equal? (car lst1)
					       (car lst2))))
      0
      (+ 1 (match-prefix-len0 (cdr lst1)(cdr lst2)))))
  
(define (match-prefix-len str1 str2)
  (match-prefix-len0 (string->list str1)
		     (string->list str2)))


(define (make-suffix-tree-first str)
   (list str))


(define (add-suffix-tree tree str)
  (let ((pos (find-first-pos
	      (lambda(x)(> (match-prefix-len (if (list? x)(car x) x) str)
			   0))
	      tree)))
    ;;(displayln (format ":pos=~s" pos))
    (if pos
	(list-set tree pos (add-suffix-tree2 (list-ref tree pos) str))
	(append tree `(,str)))
  ))
;;ƒŠƒXƒg‚Å‚È‚¢‚Æ‚«‚Í‚»‚Ì‚Ü‚Ü•Ô‚·
(define (car0 x)
  (if (list? x)
      (car  x)
      x))
(define (add-one0 x)
  (if (number? x)
      (+ 1 x)
      x))

(define (add-suffix-tree2 tree str)
  ;(displayln (format ">arg=~s add=~s" tree str))
  (if (list? tree)
      (let* ((mlen (match-prefix-len (car tree) str))
	     (top (car tree))
	     (toplen (string-length top)))
	(cond
	 [(= mlen 0)
	  (append  tree
		   (list str))
	 ]
	 [(< mlen toplen)
	  (append (list (string-take top mlen))
		  (list (cons (string-drop top mlen)(cdr tree)))
		  (list (list (string-drop str mlen))))
	 ]
	 [else
	  (let* ((strrest (string-drop str mlen))
		 (pos  (add-one0 (find-first-pos (lambda(x)(> (match-prefix-len (car0 x) strrest) 0)) (cdr tree)))))
	
	    (if pos
		(list-set tree pos (add-suffix-tree2 (list-ref tree pos) strrest))
		(append tree  (list (string-drop str mlen)))
		)
	    )
	 ]
	))
      ;; --*-- dst string
      (let* ((mlen (match-prefix-len tree str))
	     (top tree)
	     (toplen (string-length top)))
	(cond 
     	 [(< mlen toplen)
	  (append (list (string-take top mlen))
		  (list (string-drop top mlen))
		  (list (string-drop str mlen)))
	  ]
	 [else
	  (let* ((strrest (string-drop str mlen)))
	    (list tree strrest)   ;;2021/01/23
	    )]))))
;
;
;(define (add-suffix-tree3 tree str)
  

(define (make-suffix-tree  . strs)
  (if (null? strs)
      '()
      (apply add-suffix-tree* (make-suffix-tree-first (car strs)) 
	     (cdr strs))))

(define (add-suffix-tree* tree . strs)
  (if (null? strs)
      tree
      (apply add-suffix-tree*
	     (add-suffix-tree tree (car strs))
	     (cdr strs))))

(define (all-suffix str)
  (map (lambda(x)(string-drop str x))(iota (string-length str) )))


(define (solve_suff str)
  (apply make-suffix-tree (all-suffix str)))


