#lang racket
;; rosalind
;; Finding Disjoint Motifs in a Gene 
;; (ITWV)
;; 2021/10/14 AC
(require srfi/13)
(require srfi/1)

(require "readfileA.ss")


(define (ros_itwv . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_itwv.txt"
		    (format "data\\rs_itwv~a.txt" (car n)))))
	 (s-dna (car data))
	 (rest-dna (cdr data))
	 )
    (display-matrix
     (matrix-string-interweave? s-dna rest-dna))
   ))

;; check interweave for list s t u 

(define (interweave0? s t u)

  ;;(displayln (format "[0]~s::~s:~s" s t u))
  
  (let ( 
	(tlen (length t))
	(ulen (length u)))

  (if (null? t)
      (if (null? u)
	  #t
	  (if (equal? (take s ulen) u)
	      #t
	      #f))
      (if (null? u)
	  (if (equal? (take s tlen) t)
	      #t
	      #f)
;;---
	  (if(equal? (car s)(car t))
	     (if (equal? (car s)(car u))
		  (or (interweave0? (cdr s)(cdr t) u)
		      (interweave0? (cdr s) t (cdr u)))
		  (interweave0? (cdr s)(cdr t) u))
	     (if (equal? (car s)(car u))
		 (interweave0? (cdr s) t (cdr u))
		 #f))))))
		  
(define (interweave2? s t u)

  ;;(displayln (format "[2]~s::~s:~s" s t u))
  
  (if (= (length s)(+ (length t)(length u)))
      (interweave0? s t u)
      #f))
      
      
(define (interweave3? s t u s-len t-u-sumlen)

  ;;(displayln (format "[3]~s::~s:~s" s t u))
  
  (if (< s-len t-u-sumlen)
      #f
      (ormap
       (lambda(x)
	 (interweave2? 
	  (take (drop s x) t-u-sumlen) t u))
       (iota (+ 1 (- s-len t-u-sumlen))))))

(define (interweave4? s t u)

  ;;(displayln (format "[4]~s::~s:~s" s t u))
  
  (interweave3? s t u (length s)(+ (length t)(length u))))

(define (string-interweave? sstr tstr ustr)

  ;;(displayln (format "[5]~s::~s:~s" sstr tstr ustr))
  (if
   (interweave4? (string->list sstr)
		 (string->list tstr)
		 (string->list ustr))
   1 0))

(define (map-string-interweave? sstr tstr ustrlist)
  (map (lambda(x)(string-interweave? sstr tstr x))
       ustrlist))

(define (matrix-string-interweave? sstr ustrlist)
  (map (lambda(y)(map-string-interweave? sstr y ustrlist))
       ustrlist))

(define (display-matrix numlistlist)
  (for-each
   (lambda(y) (display (format "~a\n"
			       (apply string-append
				      (map (lambda(x)(format "~a " x)) y)))))
	   numlistlist))
