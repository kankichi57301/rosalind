#lang racket
;; rosalind
;;Construct a Profile HMM with Pseudocounts
;; [BA10F] 2012/10/03 AC
(require srfi/1)
(require srfi/13)

(require "readfileA.ss")
(require "roslibA.ss")
(require "makeprofileA.ss")
(require "monoisotopicA.ss")
(define *DEBUG* #f)
(define *FILE_OUT* #f)
(define *ba10f_out* "ba10f_out.txt")
(define *pseudo-count* #t)
;;--*-- output tables --*--
(define *transition* '()) ;; key pair of string,value real number
(define *emit* '())       ;; key string,value real number

(define *sigma* '())
(define *aligns* '())
(define *tr-align* '())
(define *all-state* '())
(define *tr* '())


;;
;; states S,E,Mn,In,Dn
;;
(define (ros_ba10f . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba10f.txt"
		    (format "rs_ba10f~a.txt" (car n)))))
	 (1st-line  (string-tokenize (car data)))
	 (theta (string->number (car 1st-line)))   ;; threshold
	 (delta (string->number (cadr 1st-line)))  ;; pseudo count ratio
	 (sigma (map (lambda(x)(string-ref x 0))  ;;���ϵ���
		     (regexp-match* #rx"[A-Za-z]" (third data))))
	 (sigmas (cons #\- sigma))  ;;�����ϵ���+GAP
	 (sym-cnt (length sigma))   ;;���μ���
	 (aligns (map (lambda(y)
			(map (lambda(x)(string-ref x 0))
			     (regexp-match* #rx"[A-Za-z\\-]" y)))
		      (drop data 4)))
	 (seqlen (length (car aligns)))    	 ;;�������󥹤�Ĺ��
	 (seqcnt (length aligns))              	 ;;�������󥹤ο�
	 (tr-align (transpose aligns))	         ;;
	 (st '())
	 (states '())
	 (all-state '()) ;;���Ƥ�state (include S,E,I0)
	 (main-count 0)  ;;M(main,match)�ο�
	 (step-alist '())
	 )
    #|
    (displayln (format "1st line=~a" 1st-line))
    (displayln (format "thres=~a" theta))
    (displayln (format "pseudo c=~a" delta))
    |#
  (include "init_ba10e.ss")

    (set! step-alist (zip states tr-align))
    ;(for-each displayln step-alist)

    ;;
    ;;Mn 
    ;;
    (define (make-profile-B step m0bin i0bin m1bin)
      ;(display (format "m0bin=~a " m0bin))
      ;(display (format "i0bin=~a " i0bin))
      ;(display (format "m1bin=~a~%" m1bin))
      (let ((m0s (format "M~a" step))
	    (m1s (format "M~a" (+ step 1)))
	    (d0s (format "D~a" step))
	    (d1s (format "D~a" (+ step 1)))
	    (i0s (format "I~a" step))
	    )
	
	(if (not i0bin) ;;I���ʤ�step
	    (let ((trans-MM (transpose (list m0bin m1bin))))
	      ;;(displayln (format "\nM-M~a" trans-MM))
	      (set-trans m0s m1s (div* (count-trans-pair '(1 1) trans-MM)
				       (count-trans-pair '(1 2) trans-MM)))
	      
	      (set-trans m0s d1s (div* (count-trans-pair '(1 0) trans-MM)
				       (count-trans-pair '(1 2) trans-MM)))
	      
	      (set-trans d0s m1s (div* (count-trans-pair '(0 1) trans-MM)
				       (count-trans-pair '(0 2) trans-MM)))
	      
	      (set-trans d0s d1s (div* (count-trans-pair '(0 0) trans-MM)
				       (count-trans-pair '(0 2) trans-MM)))
	      
	      )
	    (begin ;;I��¸�ߤ���step
	      (let ((trans-MIM (transpose (list m0bin (list-ior i0bin) m1bin))))
		;(displayln (format "\x1b[42mMIM=~a\x1b[0m" trans-MIM))
		
		(set-trans m0s i0s (div* (count-trans-pair '(1 1 2) trans-MIM)
					 (count-trans-pair '(1 2 2) trans-MIM)))
		(set-trans m0s m1s (div* (count-trans-pair '(1 0 1) trans-MIM)
					 (count-trans-pair '(1 2 2) trans-MIM)))
		(set-trans m0s d1s (div* (count-trans-pair '(1 0 0) trans-MIM)
					 (count-trans-pair '(1 2 2) trans-MIM)))
		(set-trans d0s i0s (div* (count-trans-pair '(0 1 2) trans-MIM)
					 (count-trans-pair '(0 2 2) trans-MIM)))
		(set-trans d0s m1s (div* (count-trans-pair '(0 0 1) trans-MIM)
					 (count-trans-pair '(0 2 2) trans-MIM)))
		(set-trans d0s d1s (div* (count-trans-pair '(0 0 0) trans-MIM)
					 (count-trans-pair '(0 2 2) trans-MIM)))
		(let* ((i-cnt (all-1-count i0bin)) ;; I-n��gap�ʳ���ʸ����
		       (i-line (apply + (list-ior i0bin)))
		       (self-ratio (* 1.0 (/ (- i-cnt i-line) i-cnt)))
		       (next-ratio (- 1.0 self-ratio))
		       )
		  (when *DEBUG* (begin
				  (display (format "i0bin=~a|" i0bin))
				  (display (format "\x1b[31mall i-cnt=~a\x1b[0m|" i-cnt))
				  (display (format "\x1b[4m\x1b[32mall i-line=~a\x1b[0m" i-line))
				  (display (format "\x1b[46mall i-self-ratio=~a\x1b[0m|" (round3 self-ratio)))
				  (displayln (format "\x1b[46mall i-next-ratio=~a\x1b[0m" (round3 next-ratio)))))
		  (set-trans i0s i0s self-ratio)
		  (set-trans i0s m1s
			     (* next-ratio
				(div* (count-trans-pair '(2 1 1) trans-MIM)
				      (count-trans-pair '(2 1 2) trans-MIM))))
		  (set-trans i0s d1s
			     (* next-ratio
				(div* (count-trans-pair '(2 1 0) trans-MIM)
				      (count-trans-pair '(2 1 2) trans-MIM))))
	       ))))))
	      
    ;; 2��0,1�ɤ����Ȥ�����
    
    (define (make-prof-A step m0 i0 m1)
      (let ((m0bin (if (empty? m0)
		       (make-list seqcnt 1)
		       (chatlist->binlist (cadar m0))))
	    (i0bin (if (empty? i0)
		       #f
		       (map (lambda(x)(chatlist->binlist (cadr x))) i0)))
	    (m1bin (if (empty? m1)
		       (make-list seqcnt 1)
		       (chatlist->binlist (cadar m1)))))
	(make-profile-B step m0bin i0bin m1bin)
	))
	      
      (define (set-trans  from to prob)
      (when (> prob 0.0)
	    (let ((from* (if (string=? from "M0")
			     "S" from))
		  (to* (if (string=? to (format "M~a" (+ 1 main-count)))
			   "E" to)))
	      (when *DEBUG*
		    (displayln (format "\x1b[34mtr ~a=>~a [~a]\x1b[30m" from* to* (round3 prob))))
	      (hash-set! *transition* `(,from* ,to*) prob))))
    
    (define (between-M-M step)
      (make-prof-A
       step
       (filter (lambda(x)(string=? (format "M~a" step) (car x))) step-alist)
       (filter (lambda(x)(string=? (format "I~a" step) (car x)))  step-alist)
       (filter (lambda(x)(string=? (format "M~a" (+ step 1)) (car x))) step-alist)))	      
;; --*-- STUB of make-profile --*--    
    (define (make-profile)
      (for-each (lambda(n)
		  ;;(displayln "--------")
		  (between-M-M n))
		(iota (+ 1 main-count))))

    (make-profile)

    (include "pseudo-count.ss")
    (when *pseudo-count*
	  (trans-pseudo-count))
    
    
    (define (find-chars state)
      (let ((it (filter (lambda(x)(string=? state (car x))) step-alist)))
	(if (null? it)
	    #f
	    (map cadr it))))

    (define (set-emit state step charlist)
      ;(displayln (format "~a~a:~a" state step (map round3 (normalize-num (sym-count-m charlist sigma)))))
      (hash-set! *emit* (format "~a~a" state step)  (normalize-num (sym-count-m charlist sigma))))
;;�����ϵ�����Ʊ����Ψ��      
    (define (set-emit-eq state step cnt)
      (hash-set! *emit* (format "~a~a" state step)(make-list cnt (/ 1.0 cnt))))
      
    (define (make-emit)
      (for-each (lambda(i)
		  (let ((m0 (find-chars (format "M~a" i)))
			(i0 (find-chars (format "I~a" i)))
			)
		    ;(displayln (format "M~a =~a" i m0))
		    ;(displayln (format "I~a =~a" i i0))
		    (when m0
			  (set-emit "M" i m0))
		    (when (list? i0)
			  (set-emit "I" i i0))))
		(iota (+ main-count 1))))
    
    (make-emit)
    (when *pseudo-count*
	  (emit-pseudo-count))

    ;;����J������ɽ������������P����s������������o�����ͤ񎿎�������������B
    
    (call-with-output-file *ba10f_out*
      (lambda(out)
	(disp-head all-state out)
	(out-trans-line out)
	(displayln "--------" out)
	(disp-head sigma out)
	(out-emit-line out)
	)
      #:exists 'truncate/replace)
 ))
;;--*-- --*--

;;
(define (gap-ratio chars)
  (let ((len (length chars)))
    (* 1.0 (/ (gap-count chars) len))))

(define (decideMI chars dnacnt theta)
  ;;(displayln (format "chars=~a theta=~a" chars theta))
  (if (< (gap-ratio chars) theta)
      "M"   
      "I"))
;;
;; MDI����estate�������񎿎������Ԥ񎿎�������������B
;;

(define (number-states states)
  (reverse (number-states0 states 0 '())))

(define (number-states0 states num acc)
  (if (null? states)
      acc
      (let ((st (string-ref (car states) 0)))
	(if (equal? st #\I)
	    (number-states0 (cdr states) num (cons num acc))
	    (number-states0 (cdr states) (+ num 1) (cons (+ num 1) acc))))))


;; GAP=>0 ���������������������ȳ���������1�������Ѵ񎿎�
(define (chatlist->binlist charlist)
 (map (lambda(c)(if (equal? #\- c) 0 1)) charlist))

(define (all-nogap-count charlistlist)
  (apply + (append-map chatlist->binlist charlistlist)))
(define (all-1-count binlistlist)
  (apply + (apply append binlistlist)))
  
(define (count1 binlist)
  (apply + binlist))

;; bit������������]
(define (inv-list binlist)
  (map (lambda(x)(- 1 x)) binlist)) 

;(ros_ba10e 9)

(define (round3 n)
  (/ (round (* 1000 n)) 1000.0))

(define (list-ior binlistlist)
  (map (lambda(x)(apply bitwise-ior x))(apply zip binlistlist)))

(define (bin-eq-wild0 n1 n2)
  (or (= n1 2)(= n1 n2)))
    
(define (bin-eq-wild-pair npair1 npair2)
  (andmap (lambda(x y)(bin-eq-wild0 x y))
	  npair1 npair2))

(define (count-trans-pair pair pairlist)
  (count (lambda(x)(bin-eq-wild-pair pair x)) pairlist))

;;--*-- test data --*--
(define xx '((1 1)(1 1)(1 1)(1 0)(1 0)(0 1)(0 1)(0 1)(0 0)))
;; ������0�ʤ����̤⥼���ˤ���
(define (div* x y)
  (if (= y 0)
      0
      (* 1.0 (/ x y))))

(ros_ba10f 13)
