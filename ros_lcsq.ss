#lang racket
;; rosalind
;; Finding a Shared Spliced Motif 
;; [LCSQ] 2021/10/12 AC
(require srfi/13)

(require "readfileA.ss")
(define *lcsq_out* "data\\lcsq_out.txt")
;;
;;lcs.ss
;;

(define *my-hash* (make-hash))

(define (lcs s1 s2)
  (if (= 1 (length s1))
      (if (member (car s1) s2)
	  s1
	  '())
      (if (= 1 (length s2))
	  (if (member (car s2) s1)
	      s2
	      '())
	  (if (equal? (car s1)(car s2))
	      (cons (car s1)(lcs-a (cdr s1)(cdr s2)))
	      (let ((r1 (lcs-a (cdr s1) s2))
		    (r2 (lcs-a s1 (cdr s2))))
		(if (> (length r1)(length r2))
		    r1
		    r2))))))

(define x1 '(A B C 1 2 3 D 4 E X))
(define x2 '(A B 1 C 3 D 2 E 4 Y X))

(define (lcs-a s1 s2)
  (let ((r (hash-ref *my-hash* (list s1 s2)(lambda() #f))))
    (if r
	r
	(let ((r1 (lcs s1 s2)))
	  (hash-set! *my-hash* (list s1 s2) r1)
	  r1))))

(define (lcs-b s1 s2)
  (set! *my-hash* (make-hash))
  (lcs-a s1 s2))
  

(define (my-lcs str1 str2)
  (apply string (lcs-b (string->list str1)(string->list str2))))
;;;
;;; shortest common supersequence 
;;;
(define (find-pos lst item)
  (if (null? lst)
      9999
      (if (equal? (car lst) item)
	  1
	  (+ 1 (find-pos (cdr lst) item)))))



(define (s-c-sseq-0 s1 s2 lcs)
  (if (null? lcs)
      (append s1 s2)
      (let* ((fst (car lcs))
	     (p1  (find-pos s1 fst))
	     (p2  (find-pos s2 fst)))
	(append (take s1 (- p1 1))
		(take s2 (- p2 1))
		(list fst)
		(s-c-sseq-0 (drop s1 p1)(drop s2 p2)(cdr lcs))))))

		
(define (string-shortest-common-subsequence str1 str2)
  (let ((seq1 (string->list str1))
	(seq2 (string->list str2)))
    (apply string
	   (s-c-sseq-0 seq1 seq2 (lcs-b seq1 seq2)))))
		    

(define (ros_lcsq . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_lcsq.txt"
		    (format "data\\rs_lcsq~a.txt" (car n)))))
	 (fasta-data (edit-fasta data))
	 (res "")
	 )

    (set! res
	  (apply my-lcs fasta-data))
    
    (call-with-output-file *lcsq_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)
    res
   ))
