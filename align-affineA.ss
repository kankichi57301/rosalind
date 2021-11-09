(module rosalind racket/base
	(provide (all-defined-out))
	(require (only-in racket/function identity))
;;
;; align.ss
;; 2021/02/26 copied from align.ss
;; affine gap penelty
;; invoked from ros_gaff.ss
(require srfi/1)

(define *dp* '())
(define *tb* '())

(require "roslibA.ss")
(require "blosum62A.ss")
(define -INF -99)
;; affine gap penalty
(define *a* -11)   ;; opening
(define *b* -1)    ;; extension


(define (init-affine len1 len2)
  (set! *dp* (make-hash))
  (set! *tb* (make-hash)) ;; trace back hash
  (hash-set! *dp* '(0 0) '(0 0 0))
  (for-each (lambda(x)(hash-set! *dp* `(,x 0) `(,(+ *a* (* (- x 1) *b*)) ,-INF ,-INF)))(iota len1 1))
  (for-each (lambda(y)(hash-set! *dp* `(0 ,y) `(,-INF ,(+ *a* (* (- y 1) *b*)) ,-INF))) (iota len2 1))
  
)

(define (dp-ref x y)
  (hash-ref *dp* `(,x ,y) '(-99 -99 -99)))
(define (tb-ref x y)
  (hash-ref *tb* `(,x ,y) 9))

(define (fill-affine-table str1 str2 len1 len2)
  (for-each (lambda(y)
	      (for-each (lambda(x)
			  (let* ((c   (b62score
					 (string-ref str1 (- x 1))
					 (string-ref str2 (- y 1)))
				  ))
			    
			    (hash-set! *dp*
				       `(,x ,y)
				       (dp-next (hash-ref *dp* `(,(- x 1) ,y))
						(hash-ref *dp* `(,x ,(- y 1)))
						(hash-ref *dp* `(,(- x 1) ,(- y 1)))
						c x y
						))
			    
			  ))

			(iota len1 1)))
	    (iota len2 1)))
;; --*-- TBC
#|
(define (dp-next triple1 triple2 triple3 c)
  (let ((left 0)
	(upper 0)
	(left-upper 0))
    (set! left  (max (+ *b* (car triple1))(+ *a* (cadr triple1))(+ *a* (caddr triple1))))
    (set! upper (max (+ *a* (car triple2))(+ *b* (cadr triple2))(+ *a* (caddr triple2))))
    (set! left-upper (+ c (apply max triple3))) 
;;--*-- TBC
    (list left upper left-upper)
    ))
|#
(define (dp-next triple1 triple2 triple3 c x y)
  (let ((left-list  (list (+ *b* (car triple1))(+ *a* (cadr triple1))(+ *a* (caddr triple1))))
	(upper-list (list (+ *a* (car triple2))(+ *b* (cadr triple2))(+ *a* (caddr triple2)))))

    #|
    (when (and (= x 8)(= y 2))
	  (displayln (format "[*]left list=~a" left-list))
	  (displayln (format "max index=~a" (max-index-of left-list identity))))
    |#

    
    (when (= 0 (max-index-of left-list identity))
	  (hash-set! *tb* `(,(- x 1) ,y)  0))
    (when (= 1 (max-index-of upper-list identity))
	  (hash-set! *tb* `(,x ,(- y 1))  1))
    
    (list (apply max left-list)
	  (apply max upper-list)
	  (+ c (apply max triple3)))
    ))



(define (disp-matrix mat)
  (for-each (lambda(line)(displayln (format "~a" line)))
	    mat))
(define *DUMP* #t)


(define (dump-dp len1 len2)
  (for-each (lambda(y)
	      (for-each (lambda(x)
			  (let ((tri (dp-ref x y)))
			    (display (format "~a|~a|~a " (car tri)(cadr tri)(caddr tri)))))
			(iota (+ len1 1)))
	      (display "\n"))
	    (iota (+ len2 1)))
  )

(define (dump-tb len1 len2)
  (for-each (lambda(y)
	      (for-each (lambda(x)
			  (display (format "~a " (tb-ref x y))))
			(iota (+ len1 1)))
	      (display "\n"))
	    (iota (+ len2 1)))
)

(define (pair-align-affine str1 str2)
  (let ((len1 (string-length str1))
	(len2 (string-length str2))
	)
    (init-affine len1 len2)
    (fill-affine-table str1 str2 len1 len2)
    ;(dump-dp len1 len2)
    
    (define (my-string-ref str n)
      (string-ref str (- n 1)))
    ;;; --*-- trace back dp --*--
    #|
    (define (fukugen len1 len2 acc1 acc2)
      ;;(displayln (format "x=~a y=~a dp=~a" len1 len2 (dp-ref len1 len2)))
      (if (and (= len1 0)(= len2 0))
	  (list acc1 acc2)
	(let ((ext-flag (tb-ref len1  len2)))
	;;(let ((ext-flag 9)) 
	    (if (= 0 ext-flag)
		(fukugen (- len1 1) len2     (cons (my-string-ref str1 len1) acc1)(cons #\- acc2))
		(if (= 1 ext-flag)
		    (fukugen len1 (- len2 1)     (cons #\- acc1) (cons (my-string-ref str2 len2)acc2))
		    (case (max-index-of (dp-ref len1 len2) identity)
		      [(0) (fukugen (- len1 1) len2     (cons (my-string-ref str1 len1) acc1)(cons #\- acc2))]
		      [(1) (fukugen len1 (- len2 1)     (cons #\- acc1) (cons (my-string-ref str2 len2)acc2))]
		      [(2) (fukugen (- len1 1)(- len2 1)(cons (my-string-ref str1 len1) acc1)(cons (my-string-ref str2 len2)acc2))]
		      ))))))
    |#
    (define (fukugen len1 len2 acc1 acc2 dir)
      ;;(displayln (format "x=~a y=~a dp=~a" len1 len2 (dp-ref len1 len2)))
      (if (and (= len1 0)(= len2 0))
	  (list acc1 acc2)
	(let ((ext-flag (tb-ref len1  len2)))
	;;(let ((ext-flag 9)) 
	    (if (and (= 0 ext-flag)(equal? 0 dir ))
		(fukugen (- len1 1) len2     (cons (my-string-ref str1 len1) acc1)(cons #\- acc2) 0)
		(if (and (= 1 ext-flag)(equal? 1 dir))
		    (fukugen len1 (- len2 1)     (cons #\- acc1) (cons (my-string-ref str2 len2)acc2) 1)
		    (case (max-index-of (dp-ref len1 len2) identity)
		      [(0) (fukugen (- len1 1) len2     (cons (my-string-ref str1 len1) acc1)(cons #\- acc2) 0)]
		      [(1) (fukugen len1 (- len2 1)     (cons #\- acc1) (cons (my-string-ref str2 len2)acc2) 1)]
		      [(2) (fukugen (- len1 1)(- len2 1)(cons (my-string-ref str1 len1) acc1)(cons (my-string-ref str2 len2)acc2) #f)]
		      ))))))
    
    (cons
     (apply max (hash-ref *dp* (list len1 len2)))
     (map (lambda(x)(apply string x))(fukugen len1 len2 '() '() #f)))
    
    ))
(define (affine-score str1 str2)
  (affine-score0 (string->list str1)(string->list str2) 0))

(define (affine-score0 charlist1 charlist2 gap-flag)
  (if (or (null? charlist1)(null? charlist2))
      0
      (if (equal? (car charlist1) #\-)
	  (+ (if (= 1 gap-flag) *b* *a*)
	     (affine-score0 (cdr charlist1)(cdr charlist2) 1))
	  (if (equal? (car charlist2) #\-)
	      (+ (if (= 2 gap-flag) *b* *a*)
		 (affine-score0 (cdr charlist1)(cdr charlist2) 2))
	      (+ (amino-score (car charlist1)(car charlist2))
		 (affine-score0 (cdr charlist1)(cdr charlist2) 0))))))

)



