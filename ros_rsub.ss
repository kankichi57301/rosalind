#lang racket
;; rosalind
;; Identifying Reversing Substitutions
;; [RSUB] 2021/10/31 AC 
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(define *path* '())   ;; root->leafのパス全部

(define *tree* '())
(define *species-alist* '())
(define *rsub_out* "data\\rsub_out.txt")

(define *mytree* '())
(define *dnalen* 0)
(define myhash #f)

(define (ros_rsub . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_rsub.txt"
		    (format "data\\rs_rsub~a.txt" (car n)))))
	 (tree (newick->sexp (format "(~a)" (car data))))
	 (head (filter (lambda(x)(header? x))(cdr data)))
	 (dnas (edit-fasta (cdr data)))
	 (dnalen (string-length (car dnas)))
	 (species-alist (zip (map (lambda(s)(string->symbol (string-drop s 1)))head) dnas))
	 (res '())
	 )

    (set! *tree* (all-reverse tree))     ;; newickをパーレンで囲んでカンマを除去してS式として読める形にしてから全レベルでreverseしたもの
    (set! *species-alist* species-alist)   ;;　葉
    (set! *dnalen* dnalen)
    
    ;;(displayln *tree*)
    ;;(displayln species-alist)

    ;;(all-path *tree*)
    
    (set! res 
	  (delete-duplicates
	   (append-map (lambda(path)(path-rev-subst-whole path))
		       (all-path *tree*))))
    
    
    (call-with-output-file *rsub_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln x out))
		  res))
      #:exists 'truncate/replace)
    res
))

(define (all-reverse bintree)
  (if (symbol? bintree)
      bintree
      (reverse (map all-reverse bintree))))

;;
(include "rsub1.ss")
;;
;;--*-- test data
(define x1 '(A A B B A A))

(define (all-path tree)
  (set! *path* (make-hash))
  (all-path0 tree '())
  (hash-keys *path*))

(define (all-path0 tree acc)
  ;(displayln (format "tree=~a" tree))
  (if (= (length tree) 1)
      (if (symbol? (car tree))
	  (hash-set! *path* (reverse (cons (car tree) acc)) #t)
	  "Illegal format[1]")
      (if (symbol? (car tree))
	  (if (list? (cadr tree))
	      (begin
		(all-path0 (cadr tree)(cons (car tree) acc))
		(when (not (null? (drop tree 2)))
		      (all-path0 (drop tree 2) acc)))
	      (begin
		(hash-set! *path* (reverse (cons (car tree) acc)) #t)
		(all-path0 (cdr tree) acc))
	  )
	  "Illegal fromat"
      )))
;;

;;
;;
;;
(define (path-rev-subst? path pos)
  (let* ((dnas (map (lambda(sp)(cadr (assoc sp *species-alist*))) path))
	 (nuc-trans (map (lambda(x)(string-ref x pos)) dnas))
	 (change-point (reverse-subst nuc-trans))
	 )
    
    ;;(disp-dnas dnas pos)
    ;;(displayln nuc-trans)
    ;;(displayln change-point)
    (if (not (empty? change-point))
	(append-map (lambda(x)
		      (list
		       (format "~a ~a ~a ~a->~a->~a"
			       (list-ref path (car  x))
			       (list-ref path (cadr x))
			       (+ pos 1)       ;; ! base
			       (list-ref nuc-trans (- (car x) 1))
			       (list-ref nuc-trans (car  x))
			       (list-ref nuc-trans (cadr x)))))
		    change-point)
	'()
     )
    ))



;;(path-rev-subst? (ROOT CD CXY CY) 0)
(define (highlight-nth str n)
  (display
   (format "~a\x1b[42m~a\x1b[0m~a "
	   (string-take str n)
	   (string-take (string-drop str n) 1)
	   (string-drop str (+ n 1)))))

(define (disp-dnas strlist n)
  (for-each (lambda(s)(highlight-nth s n)) strlist)
  (display "\n")
  )

;;(ros_rsub 2)

(define (path-rev-subst-whole path)
  (append-map (lambda(n)(path-rev-subst? path n))
	      (iota *dnalen*)))

;;(path-rev-subst-whole '(ROOT CD DXY DX))
