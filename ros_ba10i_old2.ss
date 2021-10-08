;;#lang racket
;; rosalind
;; 
;; 
;; [BA10I] 2012/07/03
(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "roslib.ss")
(define *ba10i_out* "ba10i_out.txt")
(define fl-pat #rx"[0-9]*[.][0-9]+")
(define *prob* '())
(define *from* '())
;(define *sigma* '())
;(define *matA* #f)
;(define *matB* #f)
;(define *state* #f)
(define *DEBUG* #f)
(define *FILE_OUT* #f)

(define (ros_ba10i . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba10i.txt"
		    (format "rs_ba10i~a.txt" (car n)))))
	 
	 (seq (caddr data))
	 (seqlen (string->number (car data)))
	 
	 (sigma (map (lambda(x)(string-ref x 0))
		     (regexp-match* #rx"[A-Za-z]" (fifth data))))
	 (state (map (lambda(x)(string-ref x 0))
		     (regexp-match* #rx"[A-Za-z]" (seventh data))))
	 (nstat (length state))
	 
	 (matA  (map (lambda(x)(map string->number (regexp-match* fl-pat x)))
		     (take (drop  data 9) nstat)))
	 
	 (matB  (map (lambda(x)(map string->number (regexp-match* fl-pat x)))
		     (take (drop  data (+ 11 nstat)) nstat)))
	 (first-out (string-ref seq 0))
	 
	 )
    ;(displayln seqlen)
    (displayln seq)
    (displayln sigma)
    (displayln state)
    (displayln matA)
    (displayln matB)
    
    (define (init-vitervi)
      (set! *prob* (make-hash))
      (set! *from* (make-hash))
      (for-each (lambda(x)(hash-set! *prob*
				     `(0 ,x)
				     ;;最初の状態の初期値は全部[1/状態数]で等しくする。
				     (* (/ 1.0 nstat)
					(list-ref (list-ref matB x)(index-of sigma first-out)))))
		(iota nstat))
      ;;(hash-set! *prob* `(0 ,first-state-index) (trans-sigma2 first-state-index first-out ))
    )

    (define (step-vitervi0 step)
      (map (lambda(to)
	     (map (lambda(from)
		    (let* ((p1 (hash-ref *prob* (list (- step 1) from)))           ;;一つ前の状態での最大の累積確率
			   (p2 (list-ref (list-ref matA from) to))                ;;注目する状態への状態遷移確率
			   (ch (string-ref seq step))
			   (p3 (list-ref (list-ref matB to) (index-of sigma ch))) ;;注目する状態での出力確率
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
    
    (define (fukugen-butlast n index acc)
      (if (< n 1)
	  acc
	  (let ((prev (hash-ref *from* (list n index))))
	    (when *DEBUG*
		  (displayln (format "res[~a]=~a" n (list-ref state prev))))
	    (fukugen-butlast (- n 1)
			     prev
			     (cons prev acc)))))
    
    (define (fukugen-last)
      (fukugen-last2 seqlen))
    
    (define (fukugen-last2 n)
      (let ((last (max-index-of
		   (map (lambda(x)
			  (hash-ref *prob* (list (- n 1) x)))
			(iota nstat ))
		   identity
		   )))
	(when *DEBUG*
	      (displayln (format "re:[~a]=~a" n (list-ref state last))))
	(fukugen-butlast (- n 1) last (list last))
      ))
    
    (define (sort-car-pair x y)
      (if (= (caar x)(caar y))
	  (< (cadar x)(cadar y))
	  (< (caar x)(caar y))))
    ;; --*-- main routine --*--
    (init-vitervi)
    (for-each (lambda(x)(step-vitervi x))(iota (- seqlen 1) 1)) 

    ;;(displayln (sort (hash-map *prob* list) sort-car-pair))
    ;;(displayln (sort (hash-map *from* list) sort-car-pair))
    
    (let ((result (map (lambda(x)(list-ref state x))
			   (fukugen-last))))

      (displayln (apply string result))
      (include "newmat.ss")
    )
    
 ))

;(ros_ba10i 2)
  
