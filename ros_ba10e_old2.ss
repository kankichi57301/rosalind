#lang racket
;; rosalind
;; 
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


(define *sigma* '())
(define *aligns* '())
(define *tr-align* '())
(define matA #f)
(define matB #f)
(define *result* '())
(define ins-lines '())
(define match-lines '())
(define *ins0* '())
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

	 (all-state '()) ;;全てのstate (S,E,I0を含む)
	 (main-count 0)
	 )

    (set! *tr* tr-align)
    
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

 
    
#t
    
 ))


;;--*-- --*--

;;(define (decide-MDI chars prevstate thres dnacnt)  
(define (gap-ratio chars dnacnt)
  (/ (gap-count chars) dnacnt))

(define (decideMI chars dnacnt theta)
  (if (= 0 (gap-count chars))
      "M"
      (if (< (gap-ratio chars dnacnt) theta)
	  "MD"
	  "I")))

;;(map (lambda(x)(decideMI x (length x) 0.25)) *tr*)

(define (solve-ba10e tr-align theta)
  (let* ((states0 (map (lambda(x)(decideMI x (length x) theta)) tr-align))
	 (states (zip states0 (number-states states0)))
	)
    states0

    ))
    

;; --*--
(ros_ba10e 1)

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
