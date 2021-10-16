#lang racket
;; rosalind
;; Sorting by Reversals
;;[SORT]
;;2021/10/15 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(require "roslibB.ss")

(define history-result '())


(define (ros_sort . n)
  (let* ((data (filter (lambda(x)(not(string= "" x)))
		       (read-file*
			(if (null? n)
			    "data\\rosalind_sort.txt"
			    (format "data\\rs_sort~a.txt" (car n))))))
	 (data2 (map string-tokenize data))
	 (data3 (map (lambda(x)(map string->number x)) data2)))

    (displayln (apply reversal-distance2 data3))
    (for-each (lambda(x)(displayln (format "~s ~s" (+ 1 (car x))(cadr x))))
	      (reverse (car history-result)))
    
   ))


(define (find-breakpoint-list0 seq prev n)
  (if (< (length seq) 2)
      (if (not (= 1 (abs (- prev (car seq)))))
	  (cons n
		(if (= (car seq) (+ n 1))
		    '()
		    (list (+ n 1))))
	  (if (= (car seq) (+ n 1))
	      '()
	      (list (+ n 1))))
	  
      (if (not (= 1 (abs (- prev (car seq)))))
	  (cons n (find-breakpoint-list0 (cdr seq)(car seq)(+ 1 n)))
	  (find-breakpoint-list0 (cdr seq)(car seq)(+ 1 n))
	  )))

(define (find-breakpoint-list seq)
  (find-breakpoint-list0 seq 0 0))

(define (reverse-part seq from to)
  (append (take seq from)
	  (reverse (take (drop seq from)(- to from)))
	  (drop seq to)))

(define (reverse-part2 seq-history-list from to)
  (list (reverse-part (car seq-history-list) from to)
	(append (cadr seq-history-list)(list (list from to)))))
  
;;------------------------------------------------------------
(define (all-reversals seq)
  (let ((brkpts (find-breakpoint-list seq)))
    (map (lambda(x)(reverse-part seq (car x)(cadr x)))
	 (filter (lambda(a)(< (car a)(cadr a)))(cartesian-product brkpts brkpts)))))

(define (all-reversals2 seq-history-list)
  (let ((brkpts (find-breakpoint-list (car seq-history-list))))
    (map (lambda(x)(reverse-part2 seq-history-list (car x)(cadr x)))
	 (filter (lambda(a)(< (car a)(cadr a)))(cartesian-product brkpts brkpts)))))


(define (reversal-distance2 src dst)
  (let ((dst2 (position-list src dst)))
    (solve72a2 (list (list dst2 '())))))


(define (solve72a2 seq-hist-list-list)

 ;;(displayln (format "arg=~s" seq-hist-list-list))
  (let ((it (find-first (lambda(x)(seq-123? (car x))) seq-hist-list-list)))
    (if it
	(begin 
	  (set! history-result (cdr it))
	  0
	)
	(let ((all-rev (min-revs2 (append-map all-reversals2 seq-hist-list-list))))
	  (+ 1 (solve72a2 all-rev))))))

(define (add-brkptn2 seq-hist-list-list)
  (map (lambda(x)(append x (list (length (find-breakpoint-list (car x))))))
       seq-hist-list-list))


(define (min-revs2 seq-hist-list-list)
  (let* ((aaa (add-brkptn2 seq-hist-list-list))
	 (minrev  (apply min (map caddr aaa)))
	 (minrevs (filter (lambda(x)(= minrev (caddr x))) aaa))
	)
					;(map (lambda(x)(take x 2)) minrevs)
    (map (lambda(x)(take x 2))minrevs)
  ))

(define (position-list org dst)
  (map (lambda(x)(+ 1 (index-of org x))) dst))

;; seq‚ª'(1 2 3 4 5...)‚Å‚ ‚é‚©
(define (seq-123? seq)
  (andmap = seq (iota (length seq) 1)))


(define t77 '(1 2 3 6 5 4 10 9 8 7))
(define t77-1 `(,t77 ()))
(define t77-10 `((,t77 ())))
(define t77-2 (all-reversals2 t77-1))

(define t77a '(8 7 2 10 1 5 3 4 9 6))
(define t77b '(5 9 8 6 2 3 1 7 10 4))

(define (reversal-part-times lst revlist)
  (if (null? revlist)
      lst
      (reversal-part-times
       (reverse-part lst (caar revlist)(cadar revlist))
       (cdr revlist)
       )))
       
