#lang racket
;; rosalind
;; Find a Cyclic Peptide with Theoretical Spectrum Matching an Ideal Spectrum
;; [BA4E] 2021/10/19 AC
;;
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "monoisotopicA.ss")
(require "roslibA.ss")
(require "roslibB.ss")
(define myhash #f)
(define *ba4e_out* "data\\ba4e_out.txt")
(define *myhash* #f)

(define (ros_ba4e . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba4e.txt"
		    (format "data\\rs_ba4e~a.txt" (car n)))))
	 (wlist (sort (map string->number (string-tokenize (car data))) <))
	 (res '())
	 )
	 
    (set! res (exec-ba4e wlist))
    
    (call-with-output-file *ba4e_out*
      (lambda(out)
	(for-each (lambda(wts)
		    (display (format "~a " (string-join (map number->string wts) "-")) out))
		  res))
      #:exists 'truncate/replace)
    res
))

(define (calc-amino-count len)
  (tri-num->num (/ (- len 2) 2)))

(define (runsum nlist)
  (if (null? nlist)
      '()
      (cons (car nlist)(map (lambda(x)(+ (car nlist) x))(runsum (cdr nlist))))))
  
;; nlistの左方にwtをつなぐことができるか。
;;　可能なら#t
;;　できないなら #f を返す
(define (can-extend-peptide nlist wt wttable)
  (let ((runs (cdr (runsum (cons wt nlist)))))
    (if (include? runs wttable)
	#t
	#f)))

;;--*--
(define w1 '(3 4 5 7))
(define wt '(5 9 14 16 18 20 21 22))
;;--*--

(define (solve-ba4e singles wlist)
  (solve-ba4e0 (list (car singles))(cdr singles) wlist)
  )

(define (solve-ba4e0 acc singles wlist)
  ;;(displayln (format "acc=~a single=~s wlist=~a" acc singles wlist))
  (if (null? singles)
      (list acc)
      ;;サイクルが閉じるための最後の１つ
      (if (= 1 (length singles))
	  (if (and (can-extend-peptide acc          (car singles) wlist)  ;;左でも右でもどちらにもつなぐことができるか
		   (can-extend-peptide (reverse acc)(car singles) wlist))
	      (begin
		;;(display   (format "comlt[1] =~a|" (cons (car singles) acc)))
		;;(displayln (format "comlt[2] =~a" (append acc (list(car singles)))))

		(list (cons (car singles) acc)(append acc (list(car singles))))) ;;答えは２通り
	      '())
	  (append 
	   (let ((next (filter (lambda(x)(can-extend-peptide acc x wlist)) singles)))
	     (append-map
	      (lambda(w)
		(solve-ba4e0 (cons w acc)(delete-once w singles)(exclude (cdr (runsum (cons w acc))) wlist)))
		next))
	   (let ((next2 (filter (lambda(x)(can-extend-peptide (reverse acc) x wlist)) singles)))
	     (append-map
	      (lambda(w)
		(solve-ba4e0 (append acc (list w))(delete-once w singles)(exclude (cdr (runsum (append acc (list w)))) wlist)))
		next2))
	  ))))
		    

;;--*-- make test data --*--
(define (make-ba4e-testdata0 dbl len times)
  (map (lambda(x)(apply + (take (drop dbl x) len)))
       (iota times)))

(define (make-ba4e-testdata nlist)
  (let ((dbl (append nlist nlist))
	(len (length nlist)))
    (cons 0
	  (append
	   (sort
	    (append-map (lambda(x)(make-ba4e-testdata0 dbl x len))
			(iota (- len 1) 1))
	    <)
	   (list (apply + nlist))))
  ))
;;--*-- test prog--*--
(define (exec-ba4e wtlist)
  (let* ((wt (cdr (sort wtlist <)))
	 (single (filter (lambda(x)(member x all-amino-weights)) wt))
	 (mult  (exclude single wt))
	 (amino-cnt (calc-amino-count (length wtlist))))
    (displayln (format "amino_c=~a" amino-cnt))
    (displayln (format "peptlen=~a" (length single)))
    (if (= amino-cnt (length single))
	(begin
	  (displayln "check OK")
	  (delete-duplicates
	   (solve-ba4e single mult))
	)
	  
	(begin ;;conflictあり
	  (displayln "conflict")
	  (if (> (length single) amino-cnt)
	      (let ((conflict (find-first (lambda(triple)(include? triple wtlist))conflict-diamino )))
		(delete-duplicates
		 (solve-ba4e (delete-once (caddr conflict) single)(cons (caddr conflict) mult))))
	      #t)))))
    
    
;;--*-- test data --*--
(define wt3 (make-ba4e-testdata '(71 99 113)))
(define wt4 (make-ba4e-testdata '(71 99 113 131)))
(define wt41 (make-ba4e-testdata '(57 71 99 131)))
(define wt5 (make-ba4e-testdata '(57 71 99 131 115)))
(define wt6 (make-ba4e-testdata '(57 71 99 131 115 103)))
(define wt7 (make-ba4e-testdata '(87 71 99 131 115 103 71)))
(define wt8 (make-ba4e-testdata '(99 97 103 137 71 131 114 113)))
(define wt9 (make-ba4e-testdata '(97 103 137 71 131 114 113 113 115)))
(define wt10 (make-ba4e-testdata '(99 97 103 137 71 131 114 113 113 115))) ;;; ???--*-- おかしい
(define wta  (make-ba4e-testdata '(99 97 103 137 71 131 114 113 113 115 113)))
;;(test-ba4e wt3)
