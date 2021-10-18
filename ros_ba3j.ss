#lang racket
;; rosalind
;; Reconstruct a String from its Paired Composition
;; [BA3J] 2021/10/05
;; 2012/10/17 C
(require srfi/1)
(require srfi/13)
(require srfi/19)
(require "readfileA.ss")
(require "roslibA.ss")
(require "roslibB.ss")
(require "roslibC.ss")

(define *time* #f)
(define *ba3j_out* "data\\ba3j_out.txt")

(define (ros_ba3j . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba3j.txt"
		    (format "data\\rs_ba3j~a.txt" (car n)))))
	 (1st-line (string-tokenize(car data)))
	 (k (string->number (car 1st-line)))
	 (d (string->number (cadr 1st-line)))
	 (kdks (map (lambda(s)(regexp-replace #rx"[|]" s (make-string d #\-)))(cdr data)))
	 (ans "")
	 )
    
    
    (set! *time* (current-time))
    (set! ans (merge-all kdks))
    (displayln (format "elapsed =~a sec" (time-second (time-difference (current-time) *time*))))
    
    (call-with-output-file *ba3j_out*
      (lambda(out)
	(displayln ans out))
      #:exists 'truncate/replace)
    ans
))

(define aaa '(
"GTGG--GTGA"
"TGGT--TGAG"
"GGTC--GAGA"
"GTCG--AGAT"
"TCGT--GATG"
"CGTG--ATGT"
"GTGA--TGTT"
"TGAG--GTTG"
"GAGA--TTGA"))
;;; ���s��
(define aa2 
'("GAGA--TTGA"
  "TCGT--GATG"
  "CGTG--ATGT"
  "TGGT--TGAG"
  "GTGA--TGTT"
  "GTGG--GTGA"
  "TGAG--GTTG"
  "GGTC--GAGA"
  "GTCG--AGAT"))

(define (match-gap c1 c2)
  (or (equal? #\- c1)
      (equal? #\- c2)
      (equal? c1  c2)))
(define *x* '())
(define *n* 0)
(define (str-shift-match basestr addstr n)
  (let ((addlen (string-length addstr)))
    (if (negative? n)	
	(begin
	  #|
	  (displayln (format "~a:~a"
			     (string-drop addstr (- n))
			     (string-take basestr (+ addlen n))))
	  |#
	  (string-equal-with-gap?
	   (string-drop addstr (- n))
	   (string-take basestr (+ addlen n))))
	(begin
	  (string-equal-with-gap?
	   (string-drop-right addstr n)
	   (string-take-right basestr (- addlen n)))))))

(define (string-equal-with-gap? str1 str2)
  (andmap match-gap (string->list str1)(string->list str2)))

;;
(define (merge-gap-c char1 char2)
  (if (equal? char1 #\-)
      char2
      char1))
(define (merge-gap-str str1 str2)
  (apply string (map merge-gap-c (string->list str1)(string->list str2))))


(define (merge-gapped0 base added n)
  ;;; check args
  (when (str-shift-match base added n)
	;;(displayln "\x1b[43mmerge arg ok\x1b[0m")
	
	(let ((addlen (string-length added)))
	  (if (negative? n)
	      (format "~a~a~a"
		      (string-take added (- n))
		      (merge-gap-str (string-take base (+ addlen n))
				     (string-drop added (- n)))
		      (string-drop base (+ addlen n)))
	      (begin
		(format "~a|~a|~a"
			(string-drop-right base (- addlen n))
			(merge-gap-str (string-take-right base  (- addlen n))
				       (string-take       added (- addlen n)))
			(string-take-right added n))
		
		(format "~a~a~a"
			(string-drop-right base (- addlen n))
			(merge-gap-str (string-take-right base  (- addlen n))
				       (string-take       added (- addlen n)))
			(string-take-right added n)))

	      ))))

(define (merge-all strlist)
  (merge-all0 (car strlist)(cdr strlist) 1 0))

(define (merge-all0 accstr reststrlist cnt dir)
    (if (null? reststrlist)
	accstr
	(if (= dir 0) 
	    (let ((next (find-first (lambda(x)(str-shift-match accstr x 1)) reststrlist)))
	      (if next
		  (begin
		    (displayln (format "next+ =[~a] acc=[~a]" cnt (string-length accstr)))
		    (merge-all0 (merge-gapped0 accstr next 1)(delete-once next reststrlist)(+ 1 cnt) 0))
		  (merge-all0 accstr reststrlist cnt 1)))
	    (let ((next2 (find-first (lambda(x)(str-shift-match accstr x -1)) reststrlist)))
	      (when next2
		    (displayln (format "next- =[~a] acc=[~a]" cnt (string-length accstr)))
		    (merge-all0 (merge-gapped0 accstr next2 -1)(delete-once next2 reststrlist)(+ 1 cnt) 1))))))
	      
;;(ros_ba3j 2)
	    
