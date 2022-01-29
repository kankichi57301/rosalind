(module rosalind racket/base
	(provide (all-defined-out))
	(require (only-in racket/list index-of))
;;
;;bwtB.ss
;;
(require srfi/1)
(require srfi/13)
(require "roslibA.ss")

(define str6 "ACGTAGT$")
(define charlist6 (string->list str6))
(define str7 "TTGGCCTAACGTAGT$")
(define charlist7 (string->list str7))
(define str8 "GCGTGCGTGTGCA$")
(define charlist8 (string->list str8))
(define str9  "CAGTCAGTCAGCAGTAGCAGG$")
(define charlist9 (string->list str9))

(define (rotlist lst n)  
  (if (= n 0)
      lst
      (rotlist (append (cdr lst) (list (car lst))) (- n 1))))


(define (all-rot lst)
  (map (lambda(x)(rotlist lst x))(iota (length lst))))

(define (mybwt str)
  (sort
   (all-rot
    (zip (string->list str)(iota (string-length str) 1)))
   (lambda(x y)(string< (apply string (map car x))(apply string  (map car y))))))

(define (firstlast lst)
  (list (car lst)(car (reverse lst))))

(define (myFL str)
  (map firstlast (mybwt str)))

(define (make-bwt str)
  (map (lambda(x)(car (take-right x 1)))
       (sort
	(all-rot (string->list str))
	(lambda(x y)(string< (apply string x)(apply string y))))))

;;
;;function
;; search-2
;;nuc-count-list = list of (A,C,G,T) count
;;BWT-count-alist list of (nuc,count)
;;
(define (numbering-char charlist)
  (numbering-char0 charlist 0 0 0 0))

(define (numbering-char0 charlist a c g t)
  (if (null? charlist)
      '()
      (case (car charlist)
	[(#\$)(cons '(#\$ 0)          (numbering-char0 (cdr charlist) a c g t))]
	[(#\A)(cons `(#\A ,(+ 1 a)) (numbering-char0 (cdr charlist) (+ 1 a) c g t))]
	[(#\C)(cons `(#\C ,(+ 1 c)) (numbering-char0 (cdr charlist) a (+ 1 c) g t))]
	[(#\G)(cons `(#\G ,(+ 1 g)) (numbering-char0 (cdr charlist) a c (+ g 1) t))]
	[(#\T)(cons `(#\T ,(+ 1 t)) (numbering-char0 (cdr charlist) a c g (+ t 1)))])))


(define (count-nuc charlist)
  (count-nuc0 charlist 0 0 0 0))

(define (count-nuc0 charlist a c g t)
  (if (null? charlist)
      (list a c g t)
      (case (car charlist)
	[(#\A) (count-nuc0 (cdr charlist) (+ a 1) c g t)]
	[(#\C) (count-nuc0 (cdr charlist) a (+ c 1) g t)]
	[(#\G) (count-nuc0 (cdr charlist) a c (+ g 1) t)]
	[(#\T) (count-nuc0 (cdr charlist) a c g (+ t 1))]
	[else  (count-nuc0 (cdr charlist) a c g t      )]
	)))
      



(define (list-span lst from to)
  (take (drop lst (- from 1))(- to from -1)))

(define (count-line nuc-count nuc no)
  (+ no
     (case nuc
       [(#\A) 0]
       [(#\C) (car nuc-count)]
       [(#\G) (+ (car nuc-count)
		 (cadr nuc-count))]
       [(#\T) (+ (car nuc-count)
		 (cadr nuc-count)
		 (caddr nuc-count)
		 )])))
       

(define nuc-c-6 (count-nuc charlist6))
(define nuc-c-7 (count-nuc charlist7))
;;
;;function name:nuc->span
;;example: arg1 = (1 2 3 4) means $ACCGGGTTTT
;; include first '$' character
;;         arg2 = #\A       =>'(2 2)               
;;                #\C       =>'(3 4)
;;                #\G       =>'(5 7)
;;                #\T       =>'(8 11)
(define (nuc->span nuc-count nuc)
  (let ((pos (index-of all-enki nuc)))
    (list (+ 2 (apply + (take nuc-count pos)))
	  (+ 1 (apply + (take nuc-count (+ 1 pos))))
	  )
    ))
;;--*-- TBC--*--
(define (minmax alist)
  (let ((nums (map cadr alist)))
    (values (apply min nums)(apply max nums))))

(define (search-2 nuc-count BWT-with-count search from to)
  ;;(displayln nuc-count)
  ;;(displayln BWT-with-count)
  (let* ((span  (nuc->span nuc-count (car search)))
	 (matched0 (list-span BWT-with-count (car span)(cadr span)))
	 (matched  (if (or (= 0 from)(= 0 to))
		       matched0
		       (list-span matched0 from to)))
	 (matched2 (filter (lambda(x)(equal? (car x)(cadr search))) matched))
	 )
    (if (= 2 (length search))
	(length matched2)
	(if (null? matched2)
	    0
	    (let-values ([(from* to*)(minmax matched2)])
	      (search-2 nuc-count BWT-with-count (cdr search) from* to*))))))
       
    
;;--*-- test data --*--
(define bwt6 (make-bwt str6))
(define bwt9 (make-bwt str9))
;;--*--
(define (find-count bwt search)
  (let ((bwt-with-c (numbering-char bwt))
	(nuc-cnt (count-nuc bwt)))
    (search-2 nuc-cnt bwt-with-c search 0 0)
    ))

;;(find-count str9 '(#\G #\A))

(define (find-count-M bwt searchstrlist)
  (map (lambda(x)(find-count bwt x))(map (lambda(x)(reverse (string->list x))) searchstrlist)))

;;(find-count-M (make-bwt "ATGATGAACTTG$") '("AT" "ACT" "TT"))
  
(define (confirm-bwt bwt)
  (let ((bwt-with-c (numbering-char bwt))
	(firstline (sort bwt char<?)))
    (zip firstline bwt-with-c)))



)
