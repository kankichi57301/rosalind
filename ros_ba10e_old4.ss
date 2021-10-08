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
	 (step-alist '())
	 )
    (set! *transition* (make-hash))
    (set! *emit* (make-hash))
    
    
    (set! st (map (lambda(x)(decideMI x (length x) theta)) tr-align))
    ;(displayln (format "exist st=~a" st))
    
    (set! states (map (lambda(x)(format "~a~a" (car x)(cadr x)))
		      (zip st (number-states st))))
    
    ;(displayln  states)
    
    (set! *sigma* sigma)
    
    (set! *tr* tr-align)
    (set! main-count (count (lambda(x)(or (equal? #\M (string-ref x 0))
					  (equal? #\N (string-ref x 0))))
			    st))
    
    ;; 存在しいなものも含め全部のstateを作る
    (define (all-state0)
      	       (map (lambda(x)(format "~a~a" (cadr x)(car x)))
		    (cartesian-product (iota main-count 1)'("M" "D" "I"))))
    ;(displayln (format "all-state0=~a" (all-state0)))
    
    (set! all-state `("S" "I0" ,@(all-state0) "E"))
    
    ;;(displayln (format "main count=~a" main-count))
    ;;(displayln (format "all-state= ~a" all-state ))

    ;; st-n というstateがいくつ存在するか
    ;; st = one of "M" "N" "I"
    (define (count-state st n)
      (count
       (lambda(x)(equal? x `(,st ,n)))
       states))

    ;; --*-- test --*-- check of countI
    (define (test01)
      (displayln (map (lambda(x)(cons (format "I~a" x)(count-state "I" x)))(iota (+ main-count 1)))))

      

    ;(test01)
    #|
    (makep-profile)
     (makeS)
     (for-each (lambda(i)(makeI i))(iota (+ 1 main-count)))
     (for-each (lambda(i)(makeM i))(iota  main-count 1))
     (for-each (lambda(i)(makeD i))(iota  main-count 1))
     (output-prof)
    )
    
    
  
    (define (state->
    |#
    
    ;; n列目のたての並びを出力
    (define (find-chars state)
      (let ((it (filter (lambda(x)(string=? state (car x))) step-alist)))
	(if (null? it)
	    #f
	    (map cadr it))))

    (set! step-alist (zip states tr-align))
    (map displayln step-alist)

    (define (test02)
      (for-each (lambda(s)(displayln (find-chars s)))'("M1" "N1" "M2" "N2" "M3" "N3")))
    ;(test02)

    ;;transition table に遷移確率をセット
    (define (set-trans from to ratio)
      (hash-set! *transition* (list from to) ratio))

    (define (makeS)
      (makeM 0))
    
    (define (makeM n)
      (let* ((m0 (format "M~a" n))
	     (m1 (format "M~a" (+ n 1)))
	     (n1 (format "N~a" (+ n 1)))
	     (d1 (format "D~a" (+ n 1)))
	     (i0 (format "I~a" n))
	     (m1-l (find-chars m1))
	     (i0-l (find-chars i0))
	     (n1-l (find-chars n1))
	     )
	#|   
	(displayln m0)
	(displayln m1)
	(displayln d1)
	(displayln i0)
	(displayln i0-l)
	(displayln n1-l)
	|#
	
	(if (not i0-l)  ;; Iがない場合
	    (if m1-l
		(set-trans m0 m1 1.0)
		(begin
		  (displayln (format "N1=~a" (car n1-l)))
		  (when n1-l
			(begin
			  (set-trans m0 d1 (gap-ratio (car n1-l)))
			  (set-trans m0 m1 (- 1.0 (gap-ratio (car n1-l))))))))
	    (begin
	      (displayln (format "I0=~a" i0))
	      #f
	      ))
	))
    (makeS)
    (hash-map *transition* list)
    ;#t
    
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
(define (gap-ratio chars)
  (let ((len (length chars)))
    (* 1.0 (/ (gap-count chars) len))))

(define (decideMI chars dnacnt theta)
  (if (= 0 (gap-count chars))
      "M"
      (if (< (gap-ratio chars) theta)
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

;; 
(define (sizeI charlistlist)
  (apply +
	 (map (lambda(x)(apply bitwise-ior x))
	      (apply zip (map notgap1 charlistlist)))))


;;(sizeI tr3)
;;(sym-count-m tr3 sigma0)
