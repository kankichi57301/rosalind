#lang racket
;; rosalind
;; Implement UPGMA 
;; [BA7D] 2021/11/20 WA
;; 2021/11/20 AC
(require (except-in srfi/1 remove))
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")

(define *ba7d_out* "data\\ba7d_out.txt")
(define *matD* #t)
(define *nodes* '())  
(define *inner-node-no* 0)
(define *inner-nodes* #f)
(define *dist-from-leaf* '())
(define *direct-child* #f)
(define *answer* '())
(define +INF 9999)

(define (ros_ba7d . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba7d.txt"
		    (format "data\\rs_ba7d~a.txt" (car n)))))
	 (n (string->number (car data)))
	 (node-count n)
	 (all-leaves (iota n))
	 (matD '())
	 )
    
    (set! *dist-from-leaf* (make-hash))
    (set! *direct-child* (make-hash))
    (set! *inner-nodes* (make-hash))
    (set! *inner-node-no* n)
    (set! *nodes* (map list (iota n)))
    
    (set! *matD* (make-weak-hash))
    
    (set! matD (map(lambda(str)(map string->number (string-tokenize str)))(cdr data)))
    (matD->hash matD *matD* n)
    ;;(displayln matD)
    

    (define (loop)
      (if (= n 2)
	  (let ((last-dist (hash-ref *matD* *nodes*)))
	    ;(displayln (format "last-dist=~a" last-dist))
	    (hash-set! *dist-from-leaf* all-leaves (/ last-dist 2.0))
	    (hash-set! *direct-child* all-leaves *nodes*)
	    (hash-set! *inner-nodes* all-leaves *inner-node-no*)   ;; root node (last no)
	    (upgma-out *direct-child* *dist-from-leaf* all-leaves)
	  )
	  (let-values (((node-i node-j mindist)(find-min-matD* *matD* *nodes* n)))
	    (let ((node-ij (sort (append node-i node-j) <)))
	      (hash-set! *inner-nodes* node-ij *inner-node-no*)
	      (set! *inner-node-no* (+ 1 *inner-node-no*))
	      (displayln (format "min i,j=~a,~a:[~a]" node-i node-j mindist))
	      (hash-set! *dist-from-leaf* node-ij (/ mindist 2))
	      (hash-set! *direct-child* node-ij (list node-i node-j))
	      
	      (for ([x *nodes*])
		   (when (and (not (equal? x node-i))(not (equal? x node-j)))
			 (let ((ave-dist (calc-ave-dist x node-ij *matD*)))
			   (hash-set! *matD* `(,x ,node-ij) ave-dist)
			   (hash-set! *matD* `(,node-ij ,x) ave-dist))))

	      (set! *nodes* (delete node-i  *nodes*))
	      (set! *nodes* (delete node-j  *nodes*))
	      (set! *nodes* (cons   node-ij *nodes*)))
	    (set! n (- n 1))
	    (loop))))
    (loop))

    (call-with-output-file *ba7d_out*
      (lambda(out)
	(for-each 
	 (lambda(y)(displayln (format "~a->~a:~a" (car y)(cadr y)(caddr y)) out))
	 (sort
	  *answer*
	  (lambda(x y)(< (car x)(car y))))
	 ))
      #:exists 'truncate/replace)
    #t
  )
    

;; x,y 0-based
(define (mat-ref nlistlist x y)
  (list-ref (list-ref nlistlist x) y))


(define (matD->hash matD hash n)
  (for-each (lambda(i)
	      (for-each (lambda(j)
			  (hash-set! hash `((,i) (,j)) (list-ref (list-ref matD i) j)))
			(iota n)))
	    (iota n)))
	      

(define (find-min-matD* matD nodes n)
  (let ((minval +INF)
	(findx #f)
	(findy #f)
	)
    (for ([x nodes])
	 (for ([y nodes])
	      (unless (equal? x y)
		      (let ((val (hash-ref matD `(,x ,y))))
			;(displayln (format "x,y=~a ,~a [~a]" x y val))
			(when (< val minval)
			      (set! minval val)
			      (set! findx x)
			      (set! findy y))))))
    (values findx findy minval)      ;;; also return minimum value 2021/11/19
    ))

(define (calc-ave-dist leaves1 leaves2 matD)
  ;;(displayln (format "ave dist ~a: ~a" leaves1 leaves2 ))
  (let* ((c1  (length leaves1))
	 (c2  (length leaves2))
	 (sum (apply + (map (lambda(x)(hash-ref matD `((,(car x))(,(cadr x)))))
			    (cartesian-product leaves1 leaves2))))
	 (res (/ sum c1 c2)))

    res))

(define (inner->no inner)
  (if (= 1 (length inner))
      (car inner)
      (hash-ref *inner-nodes* inner #f)))




(define (upgma-out direct-child dist-from-leaf node)
  
  (define (write-answer par chld)
    (let ((d0 (hash-ref dist-from-leaf par  0))
	  (d1 (hash-ref dist-from-leaf chld 0)))
      ;;(displayln (format "~a;~a | ~a:~a" par chld d0 d1))
      (set! *answer*
	    (cons
	     (list (inner->no par)(inner->no chld)(roundp3 (abs (- d0 d1))))
	     *answer*))))

  (let ((child (hash-ref direct-child node #f)))
    (when child
	  (write-answer node (car child))
	  (write-answer (car child) node)
	  (write-answer node (cadr child))
	  (write-answer (cadr child) node)
	  (upgma-out direct-child dist-from-leaf (car child))
	  (upgma-out direct-child dist-from-leaf (cadr child))
	  )))
		     
;;
;;(ros_ba7d 1)
;;
