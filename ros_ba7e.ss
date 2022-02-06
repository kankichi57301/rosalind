#lang racket
;; rosalind
;; Implement the Neighbor Joining Algorithm
;; [BA7E] 2022/02/06 AC
;;
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(define +INF 9999)
(define *ba7e_out* "data\\ba7e_out.txt")
(define *matD* #t)
(define *answer* '())
(define *matDprime* #t)
(define *total-dist*  #f)
(define *nodes* '())
(define *delta* '())


 
(define (ros_ba7e . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba7e.txt"
		    (format "data\\rs_ba7e~a.txt" (car n)))))
	 (n (string->number (car data)))
	 (res 0)
	 (matD '())
	 (deltaij -1)

	 (limb-i -1)
	 (limb-j -1)
	 (new-row '())
	 (m 0) ;; last node num
	 (nodei -1)
	 (nodej -1)
	 )
    ;;(set! *inner-node-no* n)
    (set! *nodes* (map identity (iota n)))
    
    (set! *matD* (make-weak-hash))
    (set! *answer* '())
    ;;(set! *matDprime* (make-weak-hash))

    ;;(set! *total-dist* (make-weak-hash))
    
    (set! matD (map(lambda(str)(map string->number (string-tokenize str)))(cdr data)))
    ;(displayln matD)
    (matD->hash matD *matD* n)

    (define (D->Dprime n)
      (set! *matDprime* (make-weak-hash))
      (for-each (lambda(node-i)
		  (for-each (lambda(node-j)
			      (let ((val
				     (if (= node-i node-j)
					 0
					 (- (* (- n 2)(hash-ref *matD* `(,node-i ,node-j)))
					    (hash-ref *total-dist* node-i)
					    (hash-ref *total-dist* node-j)))))

				;;(displayln (format "IJ=~a ~a [~a]" node-i node-j val))
				(hash-set! *matDprime* `(,node-i ,node-j) val)))
			    *nodes*))
		*nodes*))

    (define (createDelta n)
      (let ((delta (make-hash)))
	(for-each (lambda(i)
		    (for-each (lambda(j)
				(hash-set! delta `(,i ,j)(/ (- (hash-ref *total-dist* i)
							       (hash-ref *total-dist* j))
							   (- n 2))))
			      *nodes*))
		  *nodes*)
	delta
	))
    (define (disp-mat mat name n)
      (displayln name)
      (displayln
       (group-per
	(map cadr
	     (sort
	      (hash-map mat list)
	      (lambda(x y)(if (= (caar x)(caar y))
			      (< (cadar x)(cadar y))
			      (< (caar x)(caar y))))))
	n
	)))
;;--*-- TBC --*--
    (define (make-new-row i j)
      (map (lambda(k)(* 0.5 (+ (hash-ref *matD* `(,k ,i))
			       (hash-ref *matD* `(,k ,j))
			       (- (hash-ref *matD* `(,i ,j))))))
	   *nodes*))
      	  
    (define (disp-new-row)
      (displayln "--new row--")
      (displayln new-row))
    ;;--*-- 2022/02/05
    (define (add-row-colomn mat row n)
      (for-each (lambda(i val)
		  (hash-set! mat `(,i ,n) val)
		  (hash-set! mat `(,n ,i) val))
		*nodes* row)
      (hash-set! mat `(,n ,n) 0)
      )
      
    (define (loop n)
      ;;(displayln (format "node count=~a:" n))
      (if (= n 2)
	  (begin

	    (set! *answer* (cons (list (car *nodes*)(cadr *nodes*)
				       (roundp3 (hash-ref *matD* `(,(car *nodes*) ,(cadr *nodes*)))))
				 *answer*)))
		       
	    
	  
	  (begin
	    ;;(disp-mat *matD* "-D-" n)
	    (calc-total-dist *matD* *nodes*)
	    (D->Dprime n)
	    ;;(disp-mat *matDprime* "-Drime-" n)
	    ;; select most neighbor nodes => i,j
	    (let-values (((i j minval)(find-min-matD *matDprime* *nodes* n)))
	      ;(displayln (format "min i,j=~a,~a:[~a]" i j minval))
	      (set! *delta* (createDelta n))
	      ;(disp-mat *delta* "delta" n)
	      ;;(displayln (format "D[il[j]=~a" (hash-ref *matD* `(,i ,j))))
	      ;;(displayln (format "delta[il[j]=~a" (hash-ref *delta* `(,i ,j))))
	      (set! limb-i (/ (+ (hash-ref *matD* `(,i ,j))(hash-ref *delta* `(,i ,j))) 2))
	      (set! limb-j (/ (- (hash-ref *matD* `(,i ,j))(hash-ref *delta* `(,i ,j))) 2))
	      ;(displayln (format "limbij= ~a,~a" limb-i limb-j))
	      (set! new-row (append (make-new-row i j) '(0)))
	      ;(disp-new-row)
	      ;(disp-mat *matD* "-*D*-" n)
	      (set! m (+ 1 (car (take-right *nodes* 1))))
	      (add-row-colomn *matD* new-row m)
	      
	      ;(displayln (format "limb ij=~a,~a" limb-i limb-j))
	      ;(disp-mat *matD* "--D--" (+ n 1))
	      
	      (set! *matD* (rem-mat j (rem-mat i *matD*)))
	      ;(disp-mat *matD* "-Dnew-" (- n 1))
	      (set! m (+ 1 (car (take-right *nodes* 1))))
	      (set! *nodes* (append *nodes* (list m)))
	      ;(displayln (format "m=~a" m))
	      (set! nodei i)
	      (set! nodej j)
	      
	      (set! *nodes* (delete nodej (delete nodei *nodes*)))
	      
	      
	      (set! *answer* (cons (list nodei m (roundp3 limb-i)) *answer*))
	      (set! *answer* (cons (list nodej m (roundp3 limb-j)) *answer*))
	      
	      	     
	      (loop (- n 1))
	    ))))
	    
    (loop n)
    (define (both-way triples)
      (append triples
	      (map (lambda(x)(cons (cadr x)(cons (car x)(cddr x))))
		   triples)))

    
    (call-with-output-file *ba7e_out*
      (lambda(out)
	(for-each (lambda(x)(displayln (format "~a->~a:~a" (car x)(cadr x)(caddr x)) out))
		  (sort
		   (both-way *answer*)
		   (lambda(x y)(< (car x)(car y))))
		   )
	)
      #:exists 'truncate/replace)

      *answer*
    ))

(define (matD->hash matD hash n)
  (for-each (lambda(i)
	      (for-each (lambda(j)
			  (hash-set! hash `(,i ,j) (list-ref (list-ref matD i) j)))
			(iota n)))
	    (iota n)))

(define (calc-total-dist matD nodes)
  (set! *total-dist* (make-hash))
  (for ([x nodes])
       (let ((sum 0))
	 (for ([y nodes])
	      (unless (equal? x y)
		      (set! sum (+ sum (hash-ref matD `(,x ,y))))))
	 (hash-set! *total-dist* x sum))))

(define (find-min-matD matD nodes n)
  (let ((minval +INF)
	(findx #f)
	(findy #f)
	)
    (for ([x nodes])
	 (for ([y nodes])
	      (unless (equal? x y)
		      (let ((val (hash-ref matD `(,x ,y))))
			;;(displayln (format "x,y=~a ,~a [~a]" x y val))
			(when (< val minval)
			      (set! minval val)
			      (set! findx x)
			      (set! findy y))))))
    (values findx
	    findy minval)))




	
		
(define (rem-mat i mat)
  (let ((newmat (make-hash))
	(n (apply max (map car (hash-keys mat)))))
    (map (lambda(keys)(hash-set! newmat keys (hash-ref mat keys)))
	 (filter (lambda(key)(and (not (= i (car key)))
				  (not (= i (cadr key)))))
		 (hash-keys mat)))
    newmat))

;;
;;(ros_ba7e 3)
;;
;;--*-- --*--
