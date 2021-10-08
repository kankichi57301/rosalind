#lang racket
;; rosalind
;; 
;; Implement Viterbi Learning
;; [BA10I] 2012/07/03 AC
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
	 (nsigma (length sigma))
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
    ;(displayln seq)
    ;(displayln sigma)
    ;(displayln state)
    ;(displayln matA)
    ;(displayln matB)
    
    (define (init-vitervi)
      (set! *prob* (make-hash))
      (set! *from* (make-hash))
      (for-each (lambda(x)(hash-set! *prob*
				     `(0 ,x)
				     ;;�ŏ��̏��Ԃ̏����l�͑S��[1/���Ԑ�]�œ����������B
				     (* (/ 1.0 nstat)
					(list-ref (list-ref matB x)(index-of sigma first-out)))))
		(iota nstat))
      ;;(hash-set! *prob* `(0 ,first-state-index) (trans-sigma2 first-state-index first-out ))
    )

    (define (step-vitervi0 step)
      (map (lambda(to)
	     (map (lambda(from)
		    (let* ((p1 (hash-ref *prob* (list (- step 1) from)))           ;;���O�̏��Ԃł̍ő��̗ݐϊm��
			   (p2 (list-ref (list-ref matA from) to))                ;;���ڂ������Ԃւ̏��ԑJ�ڊm��
			   (ch (string-ref seq step))
			   (p3 (list-ref (list-ref matB to) (index-of sigma ch))) ;;���ڂ������Ԃł̏o�͊m��
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
    (include "newmat.ss")

    (define (ba10i_loop n)
      (if (= 0 n)
	  #f
	  (begin
	    (init-vitervi)
	    (for-each (lambda(x)(step-vitervi x))(iota (- seqlen 1) 1)) 

	    (let ((result (map (lambda(x)(list-ref state x))
			   (fukugen-last))))
	      ;(displayln (apply string result))
	      (let-values (((A B)(makenewmat result (string->list seq))))
		;(displayln A)
		;(displayln B)
		(set! matA A)
		(set! matB B))
	      (ba10i_loop (- n 1))
	      ))))

    (define (outA out)
      (displaylist (cons "  " state) out)
      (for-each (lambda(x)
		  (displaylist
		   (cons (list-ref state x)
			 (conv-ratio
			  (list-ref matA x)))
		   out))
		(iota nstat)))
    
    (define (outB out)
      (displaylist (cons "  " sigma) out)
      (for-each (lambda(x)
		  (displaylist
		   (cons (list-ref state x)
			 (conv-ratio
			  (list-ref matB x)))
		   out))
		(iota nstat)))


    
    (ba10i_loop 100)
    
    (call-with-output-file *ba10i_out*
      (lambda(outport)
	(outA outport)
	(displayln "--------" outport)
	(outB outport)
      )
      #:exists 'truncate/replace)

 ))

(ros_ba10i 2)
  
