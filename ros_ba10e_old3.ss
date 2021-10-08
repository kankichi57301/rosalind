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
(define *DEBUG* #f)
(define *FILE_OUT* #t)
(define *ba10e_out* "ba10e_out.txt")
;;--*-- output tables --*--
(define *transistion* #f)
(define *emit* #f)

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
	 ;; threshold
	 (theta (string->number (car data)))
	 ;;出力記号
	 (sigma (map (lambda(x)(string-ref x 0))
		     (regexp-match* #rx"[A-Za-z]" (third data))))
	 (sigmas (cons #\- sigma))

	 
	 ;;出力記号の種類
	 (sym-cnt (length sigma))
	 (aligns (map (lambda(y)
			(map (lambda(x)(string-ref x 0))
			     (regexp-match* #rx"[A-Za-z\\-]" y)))
		      (drop data 4)))
	 ;;アライメント後の長さ（共通）
	 (seqlen (length (car aligns)))
	 ;;アライメントの本数
	 (seqcnt (length aligns))
	 ;;転置したアライメント
	 (tr-align (transpose aligns))
	 (st '())
	 (states '())
	 (all-state '()) ;;全てのstate (S,E,I0を含む)
	 (main-count 0)
	 )
    (set! *transistion* (make-hash))
    (set! *emit* (make-hash))
    ;(displayln aligns)
    
    (set! st (map (lambda(x)(decideMI x (length x) theta)) tr-align))
    ;(displayln (format "st=~a" st))
    
    (set! states (zip st (number-states st)))
    ;(displayln states)
    
    (set! *sigma* sigma)
    
    (set! *tr* tr-align)
    (set! main-count (count (lambda(x)(or (equal? #\M (string-ref x 0))
					  (equal? #\N (string-ref x 0))))
			    st))
    
    ;; 存在しなものも含め全部のstateを作る
    (define (all-state0)
      	       (map (lambda(x)(format "~a~a" (cadr x)(car x)))
		    (cartesian-product (iota main-count 1)'("M" "D" "I"))))
    ;(displayln (format "all-state0=~a" (all-state0)))
    
    (set! all-state `("S" "I0" ,@(all-state0) "E"))
    
    (displayln (format "main count=~a" main-count))
    (displayln (format "all-state= ~a" all-state ))

    ;; st-n というstateがいくつ存在するか
    ;; st = one of "M" "N" "I"
    (define (count-state st n)
      (count
       (lambda(x)(equal? x `(,st ,n)))
       states))

    ;; --*-- test --*-- check of countI
    (define (test01)
      (displayln (map (lambda(x)(cons (format "I~a" x)(count-state "I" x)))(iota (+ main-count 1)))))

      

    (test01)
    #|
    (makep-profile)
     (makeS)
     (for-each (lambda(i)(makeI i))(iota (+ 1 main-count)))
     (for-each (lambda(i)(makeM i))(iota  main-count 1))
     (for-each (lambda(i)(makeD i))(iota  main-count 1))
     (output-prof)C
    )
    
    (define (makeS)
      (if (= 0 (count-state "I" 0))
	  
    |#
    
    #|
    (call-with-output-file *ba10e_out*
      (lambda(out)
	(for-each (lambda(lis)
		    (for-each (lambda(x)
				(display (format "~a " x) out))
			      Lis)
		    (display "\n" out)
		  )
		  *result*))
      #:exists 'truncate/replace)
    |#
 ))
;;--*-- --*--

;;
(define (gap-ratio chars dnacnt)
  (/ (gap-count chars) dnacnt))

(define (decideMI chars dnacnt theta)
  (if (= 0 (gap-count chars))
      "M"
      (if (< (gap-ratio chars dnacnt) theta)
	  "N"   ;; means M & D 2021/09/22
	  "I")))

;;(map (lambda(x)(decideMI x (length x) 0.25)) *tr*)
;;(solve-ba10e tr1 0.251)

    

;; --*--
;;(ros_ba10e 1)

(define tr1 '((#\- #\- #\- #\- #\A #\- #\- #\-)
	      (#\- #\C #\C #\- #\E #\E #\E #\E)
	      (#\- #\C #\C #\C #\C #\C #\C #\-)
	      (#\E #\E #\E #\E #\E #\E #\E #\E)
	      (#\B #\B #\B #\E #\B #\B #\- #\B)
	      (#\A #\D #\- #\D #\D #\E #\D #\D)))

(define tr2 (cons '(#\- #\A #\A #\A #\B #\B #\B #\B)
		  tr1))
;;
;; MDI各stateに附番する。
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

;;
;; ((#\A #\- #\- #\- #\- )(#\B #\B #\- #\- #\-)) => 2 
;; ((#\A #\- #\- #\- #\- )(#\- #\B #\B #\- #\-)) => 3
;;
(define tr3 '((#\A #\- #\- #\- #\- )(#\B #\B #\- #\- #\-)))
(define tr4 '((#\A #\- #\- #\- #\- )(#\B #\B #\- #\- #\-)(#\C #\- #\- #\C #\-)))
(define tr5 (cons '(#\D #\- #\- #\- #\D ) tr4))
(define tr6 (cons '(#\E #\- #\E #\- #\D ) tr5))
(define sigmas0 '(#\A #\B #\C #\D #\E))

(define (notgap1 charlist)
  (map (lambda(c)(if (equal? #\- c) 0 1)) charlist))

(define (sizeI charlistlist)
  (apply +
	 (map (lambda(x)(apply bitwise-ior x))
	      (apply zip (map notgap1 charlistlist)))))


;;(sizeI tr3)
;;(sym-count-m tr3 sigma0)
