;; rosalind
;; 
;; Compute the Probability of an Outcome Given a Hidden Path 
;; [BA10C]
(require srfi/1)
(require srfi/13)
(require srfi/48)
(include "readfile.ss")
(include "roslib.ss")
(define *DEBUG* #f)
(define fl-pat #rx"[0-9]*[.][0-9]+")
(define *matA* '())
(define *matB* '())
(define *seq* "")
(define *prob* 0)
(define *from* 0)
(define *state* '())
(define *sigma* '())

(define (ros_ba10c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba10c.txt"
		    (format "rs_ba10c~a.txt" (car n)))))
	 (seq (car data))
	 (seqlen (string-length seq))
	 (sigma (map (lambda(x)(string-ref x 0))
		     (regexp-match* #rx"[A-Za-z]" (third data))))
	 (state (map (lambda(x)(string-ref x 0))
		     (regexp-match* #rx"[A-Za-z]" (fifth data))))
	 (nstat (length state))
	 (matA  (map (lambda(x)(map string->number (regexp-match* fl-pat x)))
		     (take (drop  data 7) nstat)))
	 (matB  (map (lambda(x)(map string->number (regexp-match* fl-pat x)))
		     (take (drop  data (+ 9 nstat)) nstat)))
	 (first-out         (string-ref seq 0))
	 (first-state-index (max-index-of matB (lambda(x)(list-ref x (index-of  sigma first-out)))))
	 )

    (set! *prob* (make-hash))
    (set! *from* (make-hash))
    (set! *seq* seq)
    (set! *state* state)
    (set! *sigma* sigma)
    (set! *matA* matA)
    (set! *matB* matB)

    
    (define (init-vitervi)
      (set! *prob* (make-hash))
      (set! *from* (make-hash))
      (for-each (lambda(x)(hash-set! *prob* `(0 ,x) 0))(iota nstat))
      (hash-set! *prob* `(0 ,first-state-index) (trans-sigma2 first-state-index first-out ))
    )

    (define (step-vitervi0 step)
      (map (lambda(to)
	     (map (lambda(from)
		    (let* ((p1 (hash-ref *prob* (list (- step 1) from)))           ;;一つ前の状態での最大の累積確率
			   (p2 (list-ref (list-ref *matA* from) to))                ;;注目する状態への状態遷移確率
			   (ch (string-ref seq step))
			   (p3 (list-ref (list-ref *matB* to) (index-of sigma ch))) ;;注目する状態での出力確率
			   (pall (* p1 p2 p3)))
		      (when *DEBUG*
			    (when (<= step 2)
				  (displayln (format "p=~1,5F:~1,5F:~1,5F[~a]::~1,5F" p1 p2 p3 ch pall))))
		      pall))	   
		       
		  (iota nstat)))
	   (iota nstat)))

    (define (step-vitervi step)
      (let ((wk (step-vitervi0 step)))
	;;(when (<= step 2)
	  ;;    (displayln (format "[~a]wk=~a" step wk)))
	(for-each (lambda(x y)(hash-set! *prob* (list step x) y))
		  (iota nstat)
		  (map (lambda(x)(apply max x)) wk))
	(for-each (lambda(x y)(hash-set! *from* (list step x) (max-index-of y identity)))
		  (iota nstat)
		  wk)
      ))

    (define (fukugen1)
      (fukugen2 (- seqlen 1)))
    
    (define (fukugen2 n)
      
      (list-ref
       *state*
       (max-index-of
      
	(map (lambda(x)
			    (hash-ref *prob* (list n x)))
	     (iota nstat ))
	identity
	))
     )
	     
		  

    
    (init-vitervi)
    (for-each (lambda(x)(step-vitervi x))(iota (- seqlen 1) 1)) 
    (displayln (hash-map *prob* myformat))
    (displayln (hash-map *from* list))
    (displayln (fukugen1))
 ))

(define (myformat x y)
  (cons x (list (format "~1,9F" y))))
;; (load "ros_ba10c.ss")
;; (ros_ba10c 5)

;;--*-- test prog --*--
(define (trans-zip state sigma)
  (transition (append-map list (string->list state)(string->list sigma))))
(define (transition3  arglist)
  (if (< (length arglist) 2)
      1.0
      (* (trans-state (caddr arglist)(car arglist))
	 (transition2 (cdr arglist)))))

(define (transition2  arglist)
  (* (trans-sigma (cadr arglist)(car arglist))
     (transition3 (cdr arglist))
  ))
;; [AB]->[xyz]->[AB]->[xyz]
(define (transition   arglist)
  (transition2  (reverse arglist))
  )
;;; #\A,B -> #\A,B prob
(define (trans-state from to)
  (let ((x (index-of *state* from))
	(y (index-of *state* to)))
    (if (and x y)
	(list-ref (list-ref *matA* x) y)
	#f
	)))
;;; #\A,B -> #\x,y,z prob
(define (trans-sigma from to)
  (let ((x (index-of *state* from))
	(y (index-of *sigma* to)))
    (if (and x y)
	(list-ref (list-ref *matB* x) y)
	#f
	)))
;; 第一引数はindex (#\A,B でなく)　0,1->#\x,y,z prob
(define (trans-sigma2 x to)
  (let (
	(y (index-of *sigma* to)))
    (if (and x y)
	(list-ref (list-ref *matB* x) y)
	#f
	)))

;;--*-- test 2
(ros_ba10c 5)



(define (all-seq n)
  (map (lambda(x)(apply string x))(cartesian-expt *state* n)))
