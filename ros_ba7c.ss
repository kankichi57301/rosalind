#lang racket
;; rosalind
;; mplement AdditivePhylogeny
;; [BA7C] 2021/11/18 AC
;;�Q�l:https://www.youtube.com/watch?v=Y0QWFFWQzds&t=249s
(require (except-in srfi/1 remove))
(require srfi/13)
(require "readfileA.ss")
(define +INF 9999)
(define *ba7c_out* "data\\ba7c_out.txt")
(define *matD* #t)
(define *matD2* #t)
(define *matD** #t)
(define *total-dist*  '())
(define matD-hash '())
(define *nodes* '())  
(define *inner-node-no* 0)
(define *inner-nodes* #f)
 
(define (ros_ba7c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba7c.txt"
		    (format "data\\rs_ba7c~a.txt" (car n)))))
	 (n (string->number (car data)))
	 (res 0)
	 (matD '())
	 (deltaij 0)
	 (result '())
	 )
    (set! *inner-node-no* n)
    (set! *nodes* (map list (iota n)))
    
    (set! *matD* (make-weak-hash))
    (set! *matD2* (make-weak-hash))
    (set! *matD** (make-weak-hash))
    (set! *total-dist* (make-weak-hash))
    (set! *inner-nodes* (make-weak-hash))
    
    (set! matD (map(lambda(str)(map string->number (string-tokenize str)))(cdr data)))
    ;;(displayln matD)
    (matD->hash matD *matD* n)
    
    (define (loop)
      ;;(display (format "node count=~a:" n))
      (if (= n 3)
	  (begin
	    (displayln "3nodes")
            (calc-limb3 *matD* *nodes* *matD2*)
	    (set! result (output-adj *matD2*))
	  )
	  (begin   
	    (calc-total-dist *total-dist* *matD* *nodes*)
	    (calc-matD* *matD* *total-dist* *matD** *nodes* n) ;; *matD** = neighbor joining matrix

	    ;; select most neighbor nodes => i,j
	    (let-values (((node-i node-j)(find-min-matD* *matD** *nodes* n)))
	      ;;(displayln (format "min i,j=~a,~a:" node-i node-j))
	      (let* ((delta (/ (- (hash-ref *total-dist* node-i)
				  (hash-ref *total-dist* node-j))
			       (- n 2)))
		     (limb-i (/ (+ (hash-ref *matD* `(,node-i ,node-j)) delta) 2))
		     (limb-j (/ (- (hash-ref *matD* `(,node-i ,node-j)) delta) 2))
		     (node-ij (append node-i node-j))
		     )
		(hash-set! *inner-nodes* `(,@node-i ,@node-j) *inner-node-no*)
		(set! *inner-node-no* (+ 1 *inner-node-no*))
		;;(display (format "node ij=~a:" node-ij))	      
		;;(display (format "delta=~a:" delta))
		;;(display (format "limb i,j=~a,~a" limb-i limb-j))
		
		(for ([x *nodes*])
		     (for([y *nodes*])
			 (when (and (equal? x node-i)(not (or (equal? y node-i)(equal? y node-j))))
			       (hash-set! *matD* `(,node-ij ,y) (- (hash-ref *matD* `(,x ,y)) limb-i)) 
			       (hash-set! *matD* `(,y ,node-ij) (- (hash-ref *matD* `(,x ,y)) limb-i))
			       (hash-remove! *matD* `(,x ,y))
			       (hash-remove! *matD* `(,y ,x))
			       )
			 (when (and (equal? x node-j)(not (or (equal? y node-i)(equal? y node-j))))
			       (hash-remove! *matD* `(,x ,y))
			       (hash-remove! *matD* `(,y ,x))
			       )
			 ))
		(hash-remove! *matD* `(,node-i ,node-j))
		(hash-remove! *matD* `(,node-j ,node-i))

		(hash-set! *matD2* `(,node-i  ,node-ij) limb-i)
		(hash-set! *matD2* `(,node-ij ,node-i)  limb-i)
		(hash-set! *matD2* `(,node-j  ,node-ij) limb-j)
		(hash-set! *matD2* `(,node-ij ,node-j)  limb-j)
		
		(set! *nodes* (delete node-i *nodes*))
		(set! *nodes* (delete node-j *nodes*))
		(set! *nodes* (cons   node-ij *nodes*))
		(hash-remove! *total-dist* node-i)
		(hash-remove! *total-dist* node-j)
		(set! n (length *nodes*))
		(loop)
		)))))
    (loop)
    
    
    (call-with-output-file *ba7c_out*
      (lambda(out)
	(for-each (lambda (x)
		 (displayln x out))
	result))
      #:exists 'truncate/replace)
    
    result
    ))

;; x,y 0-based
(define (mat-ref nlistlist x y)
  (list-ref (list-ref nlistlist x) y))

(define (make-matD* matD total-dist n)
  (map (lambda(x)
	 (map (lambda(y)
		(if (= x y)
		    0
		    (- (* (- n 2)(mat-ref matD x y))
		       (list-ref total-dist x)
		       (list-ref total-dist y))))
	      (iota n)))
       (iota n)))

(define (matD->hash matD hash n)
  (for-each (lambda(i)
	      (for-each (lambda(j)
			  (hash-set! hash `((,i) (,j)) (list-ref (list-ref matD i) j)))
			(iota n)))
	    (iota n)))
	      
(define (calc-total-dist total-dist matD nodes)
  (for ([x nodes])
       (let ((sum 0))
	 (for ([y nodes])
	      (unless (equal? x y)
		      (set! sum (+ sum (hash-ref matD `(,x ,y))))))
	 (hash-set! total-dist x sum))))
       
;; D*[i][j]=(n-2)*D[i][j]-TD[i]-TD[j]
;; where TD[i] = total distance from i to all other
;;
(define (calc-matD* matD total-dist matD* nodes n)

  (for ([x nodes])
       (for ([y nodes])
	    (hash-set! matD*
		       `(,x ,y)
		       (if (equal? x y)
			   0
			   (- (* (- n 2)(hash-ref matD `(,x ,y)))
			      (hash-ref total-dist x)
			      (hash-ref total-dist y)))))))

(define (find-min-matD* matD* nodes n)
  (let ((minval +INF)
	(findx #f)
	(findy #f)
	)
    (for ([x nodes])
	 (for ([y nodes])
	      (unless (equal? x y)
		      (let ((val (hash-ref matD* `(,x ,y))))
			;;(displayln (format "x,y=~a ,~a [~a]" x y val))
			(when (< val minval)
			      (set! minval val)
			      (set! findx x)
			      (set! findy y))))))
    (values findx findy)))
;;
;;|nodelist|=3
;;
(define (calc-limb3 adjhash nodelist result-hash)
  (let* ((node1 (car   nodelist))
  	 (node2 (cadr  nodelist))
	 (node3 (caddr nodelist))
	 (a (hash-ref adjhash `(,node1 ,node2)))
	 (b (hash-ref adjhash `(,node2 ,node3)))
	 (c (hash-ref adjhash `(,node3 ,node1)))
	 (sumabc (+ a b c))
	 (center-name (apply append nodelist))
	 (limb1 (- (/ sumabc 2) b))
	 (limb2 (- (/ sumabc 2) c))
	 (limb3 (- (/ sumabc 2) a)))
    (hash-set! *inner-nodes* center-name *inner-node-no*)
    (set! *inner-node-no* (+ 1 *inner-node-no*))
    (hash-set! result-hash `(,center-name ,node1) limb1)
    (hash-set! result-hash `(,center-name ,node2) limb2)
    (hash-set! result-hash `(,center-name ,node3) limb3)
    (hash-set! result-hash `(,node1 ,center-name) limb1)
    (hash-set! result-hash `(,node2 ,center-name) limb2)
    (hash-set! result-hash `(,node3 ,center-name) limb3)
    ))

(define (test-calc-limb3 p q r)
  (let ((adjhash (make-hash))
	(nodelist '())
	(result-hash (make-hash)))
    (hash-set! adjhash `((1)(2)) p)
    (hash-set! adjhash `((2)(3)) q)
    (hash-set! adjhash `((3)(1)) r)
    (set! nodelist '((1)(2)(3)))
    (calc-limb3 adjhash nodelist result-hash)
    (hash-map result-hash list)
    ))

(define (inner->no inner)
  (if (= 1 (length inner))
      (car inner)
      (hash-ref *inner-nodes* inner #f)))

(define (out-adj-mat adj-mat-hash)
  (map (lambda(x)
	 (cons (inner->no (caar x))
	       (cons (inner->no (cadar x))
		     (cdr x))))
       (filter (lambda(x)(not (equal? (caar x)(cadar x))))
	       (hash-map adj-mat-hash list))))

(define (output-adj adj-mat-hash)
  (map (lambda(x)
	 (format "~a->~a:~a" (car x)(cadr x)(caddr x)))
       (sort
	(out-adj-mat adj-mat-hash)
	(lambda(x y)(< (car x)(car y))))))
	
		
;;
;;(ros_ba7c 1)
;;
