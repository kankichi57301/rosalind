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
    (displayln (format "main count=~a" main-count)) 
    ;; 存在しいなものも含め全部のstateを作る
    (define (all-state0)
      	       (map (lambda(x)(format "~a~a" (cadr x)(car x)))
		    (cartesian-product (iota main-count 1)'("M" "D" "I"))))
    ;(displayln (format "all-state0=~a" (all-state0)))
    
    (set! all-state `("S" "I0" ,@(all-state0) "E"))
    

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
    
    (define (make-profile)
    
     
      (for-each (lambda(i)
		  (makeM i)
		  (makeI i)
		  (when (> i 0)
		  	(makeD i)))
		(iota  (+ 1 main-count)))) 
     
    
    
        
    ;; n列目のたての並びを出力
    (define (find-chars state)
      (let ((it (filter (lambda(x)(string=? state (car x))) step-alist)))
	(if (null? it)
	    #f
	    (map cadr it))))

    (set! step-alist (zip states tr-align))
    (map displayln step-alist)

 
    ;;transition table に遷移確率をセット
    ;;m0はSに書き換え
    (define (set-trans from to ratio)
      (when (> ratio 0.0)
	    (hash-set! *transition* (list (if (string=? from "M0")
					      "S" from)
					  (if (string=? to (format "M~a" (+ 1 main-count)))
					      "E" to))
		       ratio)))
    
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
	;(displayln (format "main=~a" (or m1-l n1-l)))
	(if (not i0-l)  ;; Iがない場合
	    (if m1-l    ;; 次stepに全くgapがない
		(set-trans m0 m1 1.0)   ;;100% 次stepMに遷移する
		(if n1-l
		    (begin  ;;次stepのgapの割合によってD,Mに分岐
		      (set-trans m0 d1 (gap-ratio (car n1-l)))
		      (set-trans m0 m1 (- 1.0 (gap-ratio (car n1-l)))))
		    (set-trans m0 m1 1.0)))
		
	    (begin     ;;Iがある場合　--*-- TBC
	      ;(displayln (format "I0=~a" i0-l))
	      (if m1-l ;;Iを全てskipした次stepに全くgapがない
		  (let ((next-ins-ratio (* 1.0 (/ (sizeI i0-l) seqcnt))))
		    (set-trans m0 i0 next-ins-ratio)
		    (set-trans m0 m1 (- 1.0 next-ins-ratio))
		  )
		  #f   ;;Iを全てskipした次stepに全くgapがない
	      ))
	)))
;; I state からの遷移
    (define (makeI n)
      (let* ((m1 (format "M~a" (+ n 1)))
	     (n1 (format "N~a" (+ n 1)))
	     (d1 (format "D~a" (+ n 1)))
	     (i0 (format "I~a" n))
	     (i0-l (find-chars i0))
	     (m1-l (find-chars m1))
	     (n1-l (find-chars n1))
	     )
	(when i0-l
	      (let ((siz (sizeI i0-l))
		    (all (all-nogap-count i0-l)))
		(when (> all siz)
		      (set-trans i0 i0 (- 1.0 (/ (- all siz) all))))
		(set-trans i0 m1 (* 1.0 (/ siz all)))
	      ))
	))
    
    (define (makeD n)
      (let* ((n0 (format "N~a" n))
	     
	     (m1 (format "M~a" (+ n 1)))
	     (n1 (format "N~a" (+ n 1)))
	     (d0 (format "D~a" n))
	     (d1 (format "D~a" (+ n 1)))
	     (i0 (format "I~a" n))
	     (m1-l (find-chars m1))
	     (n0-l (find-chars n0))
	     (n1-l (find-chars n1))
	     (i0-l (find-chars i0))
	     )
	(when n0-l
	;(displayln (format "main=~a" (or m1-l n1-l)))
	      (if (not i0-l)  ;; Iがない場合
		  (if m1-l    ;; 次stepに全くgapがない
		      (set-trans d0 m1 1.0)   ;;100% 次stepMに遷移する
		      (if n1-l
			  (begin  ;;次stepのgapの割合によってD,Mに分岐
			    (set-trans d0 d1 (gap-ratio (car n1-l)))
			    (set-trans d0 m1 (- 1.0 (gap-ratio (car n1-l)))))
			  (set-trans d0 m1 1.0)))
		
		  (begin     ;;Iがある場合　--*-- TBC
					;(displayln (format "I0=~a" i0-l))
		    (if m1-l ;;Iを全てskipした次stepに全くgapがない
			(let ((next-ins-ratio (* 1.0 (/ (sizeI i0-l) seqcnt))))
			  (set-trans d0 i0 next-ins-ratio)
			  (set-trans d0 m1 (- 1.0 next-ins-ratio))
			  )
			#f   ;;Iを全てskipした次stepに全くgapがない
			))
		 ))))
    
    (define (outAlltrans)
      (sort
       (hash-map *transition* list)
       (lambda(x y)
	 (< (index-of all-state (caar x))
	    (index-of all-state (caar y)))))
       )
    #|
    (makeS)
    (makeM 1)
    (makeM 2)
    (makeM 3)
    |#
    (make-profile)
    
    (outAlltrans)
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

    

(include "mkprof-testdata.ss")
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


;; GAP=>0 それ以外は1に変換
(define (notgap1 charlist)
 (map (lambda(c)(if (equal? #\- c) 0 1)) charlist))

;;
;; ((#\A #\- #\- #\- #\- )(#\B #\B #\- #\- #\-)) => 2 
;; ((#\A #\- #\- #\- #\- )(#\- #\B #\B #\- #\-)) => 3
;;
;; 
(define (sizeI charlistlist)
  (apply +
	 (map (lambda(x)(apply bitwise-ior x))
	      (apply zip (map notgap1 charlistlist)))))


(define (all-nogap-count charlistlist)
  (apply + (append-map notgap1 charlistlist)))

;;
;;(all-nogap-count (take tr1 3))
;;
;;(ros_ba10e 5)
