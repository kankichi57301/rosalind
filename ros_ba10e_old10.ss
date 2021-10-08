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
	 (theta (string->number (car data)))   ;; threshold
	 (sigma (map (lambda(x)(string-ref x 0))  ;;出力記号
		     (regexp-match* #rx"[A-Za-z]" (third data))))
	 (sigmas (cons #\- sigma))
	 (sym-cnt (length sigma))   ;;出力記号の種類
	 (aligns (map (lambda(y)
			(map (lambda(x)(string-ref x 0))
			     (regexp-match* #rx"[A-Za-z\\-]" y)))
		      (drop data 4)))
	 (seqlen (length (car aligns)))    	 ;;アライメント後の長さ（共通）
	 (seqcnt (length aligns))              	 ;;アライメントの本数
	 (tr-align (transpose aligns))	         ;;転置したアライメント
	 (st '())
	 (states '())
	 (all-state '()) ;;全てのstate (S,E,I0を含む)
	 (main-count 0)
	 (step-alist '())
	 )

    (include "init_ba10e.ss")

    #|
    (define (make-profile)
      (for-each (lambda(i)
		  (makeM i)
		  (makeI i)
		  (when (> i 0)
		  	(makeD i)))
		(iota  (+ 1 main-count)))) 
    |#
    
    (set! step-alist (zip states tr-align))

    (for-each displayln step-alist)

    #|
    (define (make-profile)
      (for-each
       (filter (lambda

  
    (make-profile)
    |#

    
    ;(outAlltrans)
    ;#t
    ;;遷移表を１行分出力する。
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

#|
(define (decideMI chars dnacnt theta)
  (if (= 0 (gap-count chars))
      "M"
      (if (< (gap-ratio chars) theta)
	  "N"   ;; means M & D 2021/09/22
	  "I")))
|#
(define (decideMI chars dnacnt theta)
      (if (< (gap-ratio chars) theta)
	  "M"   
	  "I"))
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
