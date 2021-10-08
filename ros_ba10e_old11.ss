#lang racket
;; rosalind
;;Construct a Profile HMM
;; [BA10E] 2012/06/20 
(require srfi/1)
(require srfi/13)

(require "readfileA.ss")
(require "roslibA.ss")
(require "makeprofileA.ss")
(require "monoisotopicA.ss")
(define *DEBUG* #t)
(define *FILE_OUT* #t)
(define *ba10e_out* "ba10e_out.txt")
;;--*-- output tables --*--
(define *transition* #f) ;; key pair of string,value real number
(define *emit* #f)       ;; key string,value real number

(define *sigma* '())
(define *aligns* '())
(define *tr-align* '())
(define matA #f)
(define matB #f)
(define *all-state* '())
(define *tr* '())


;;
;; states S,E,Mn,In,Dn
;;
(define (ros_ba10e . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba10e.txt"
		    (format "rs_ba10e~a.txt" (car n)))))
	 (theta (string->number (car data)))   ;; threshold
	 (sigma (map (lambda(x)(string-ref x 0))  ;;�o�͋L��
		     (regexp-match* #rx"[A-Za-z]" (third data))))
	 (sigmas (cons #\- sigma))
	 (sym-cnt (length sigma))   ;;�o�͋L���̎���
	 (aligns (map (lambda(y)
			(map (lambda(x)(string-ref x 0))
			     (regexp-match* #rx"[A-Za-z\\-]" y)))
		      (drop data 4)))
	 (seqlen (length (car aligns)))    	 ;;�A���C�����g���̒����i���ʁj
	 (seqcnt (length aligns))              	 ;;�A���C�����g�̖{��
	 (tr-align (transpose aligns))	         ;;�]�u�����A���C�����g
	 (st '())
	 (states '())
	 (all-state '()) ;;�S�Ă�state (S,E,I0���܂�)
	 (main-count 0)
	 (step-alist '())
	 )

    (include "init_ba10e.ss")

    (set! step-alist (zip states tr-align))

    ;(for-each displayln step-alist)

    ;;
    ;;Mn ����Mn+1�ւ̑J�ڂ��o��
    ;;
    (define (make-profile-B m0bin i0bin m1bin)
      (display (format "m0bin=~a " m0bin))
      (display (format "i0bin=~a " i0bin))
      (display (format "m1bin=~a " m1bin))


      )


    
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
	(make-profile-B m0bin i0bin m1bin)
	;;--*-- TBC
	))
	      
      (define (set-trans step from to prob)
      (when (> prob 0.0)
	    (let ((from* (if (string=? from "M0")
			     "S" from))
		  (to* (if (string=? to (format "M~a" (+ 1 main-count)))
			   "E" to)))
	      (when *DEBUG*
		    (displayln (format "\x1b[34mtr ~a=>~a [~a]\x1b[30m" from* to* (round3 prob))))
	      (hash-set! *transition* `(,from ,to) prob))))
    
    (define (between-M-M step)
      (make-prof-A
       step
       (filter (lambda(x)(string=? (format "M~a" step) (car x))) step-alist)
       (filter (lambda(x)(string=? (format "I~a" step) (car x)))  step-alist)
       (filter (lambda(x)(string=? (format "M~a" (+ step 1)) (car x))) step-alist)))	      
;; --*-- STUB of make-profile --*--    
    (define (make-profile)
      (for-each (lambda(n)
		  (displayln "--------")
		  (between-M-M n))
		(iota (+ 1 main-count))))

    (make-profile)

    ;#t
    ;;�J�ڕ\���P�s���o�͂����B
    #|
    (call-with-output-file *ba10e_out*
      (lambda(out)
	(disp-head all-state out)
	(out-trans-line out)
	(displayln "--------" out)
	(disp-head sigma out)
	(out-emit-line out)
	)
      #:exists 'truncate/replace)
    |#
 ))
;;--*-- --*--

;;
(define (gap-ratio chars)
  (let ((len (length chars)))
    (* 1.0 (/ (gap-count chars) len))))

(define (decideMI chars dnacnt theta)
      (if (< (gap-ratio chars) theta)
	  "M"   
	  "I"))
;;
;; MDI�estate�ɕ��Ԃ����B
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


;; GAP=>0 �����ȊO��1�ɕϊ�
(define (chatlist->binlist charlist)
 (map (lambda(c)(if (equal? #\- c) 0 1)) charlist))

;;
;; ((#\A #\- #\- #\- #\- )(#\B #\B #\- #\- #\-)) => 2 
;; ((#\A #\- #\- #\- #\- )(#\- #\B #\B #\- #\-)) => 3
;;
;; 
(define (sizeI charlistlist)
  (apply +
	 (map (lambda(x)(apply bitwise-ior x))
	      (apply zip (map chatlist->binlist charlistlist)))))


(define (all-nogap-count charlistlist)
  (apply + (append-map chatlist->binlist charlistlist)))

(define (count1 binlist)
  (apply + binlist))

;; bit���]
(define (inv-list binlist)
  (map (lambda(x)(- 1 x)) binlist)) 

;(ros_ba10e 9)

(define (round3 n)
  (/ (round (* 1000 n)) 1000.0))

(define (list-ior binlistlist)
  (map (lambda(x)(apply bitwise-ior x))(apply zip binlistlist)))

;(ros_ba10e 11)

#|
	(let* (
	       
	       (andpat (map bitwise-and m0pat* m1pat*))
	       (m0pat-inv* (inv-list m0pat*))
	       )
	  (if (empty? i0)
	      (begin
		(set-trans step (format "M~a" step)(format "M~a" (+ step 1))(* 1.0 (/ (count1 andpat)(count1 m0pat*))))
		(set-trans step (format "M~a" step)(format "D~a" (+ step 1))(- 1.0 (/ (count1 andpat)(count1 m0pat*))))
		(displayln (format "m0inv=~a" m0pat-inv*))

		(when (positive? (count1 m0pat-inv*))
		      (let ((and-inv-pat (map bitwise-and m0pat-inv* m1pat*)))
			(set-trans step (format "D~a" step)(format "M~a" (+ step 1))(* 1.0 (/ (count1 and-inv-pat)(count1 m0pat-inv*))))
			(set-trans step (format "D~a" step)(format "D~a" (+ step 1))(- 1.0 (/ (count1 and-inv-pat)(count1 m0pat-inv*)))))))
		  
	      (begin
		#f
		)))
	|#
	
