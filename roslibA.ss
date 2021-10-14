(module rosalind racket/base
	(provide (all-defined-out))
	(require (only-in racket/list index-of flatten cartesian-product empty? group-by))
	(require (only-in racket/function identity))
(require srfi/1)
(require srfi/13)
(require srfi/14)

(define (add-group groupset item pred)

  ;;(displayln (format "arg=~s ,~s" groupset item))
  
  (if (null? groupset)
      (list (list item))
      (let* ((fstgr (car groupset))
	     (fst   (car fstgr)))
	(if (pred item fst)
	    (cons (append fstgr(list item))(cdr groupset))
	    (cons fstgr (add-group (cdr groupset) item pred))))))
  
;; (add-group '((1 2 3)(4 5 6)) 7 (lambda(x y)(= (modulo x 2)(modulo y 2))))

(define (devide-group lst pred)
  (if (null? lst)
      '()
      (if (= 1 (length lst))
	  (list lst)
	  (let ((start (list (list (car lst)))))
	    (foldl (lambda(x y)(add-group y x pred))
				start (cdr lst)))
	  )
      )
  )

(define (mod3=? x y)
  (= (modulo x 3)(modulo y 3)))

(define (nucle-complement enki)
  (case enki
    [(#\A) #\T]
    [(#\T) #\A]
    [(#\G) #\C]
    [(#\C) #\G]))

(define (complement? nuc1 nuc2)
  (equal? nuc1 (nucle-complement nuc2)))				

(define (reverse-complement? str1 str2)
  (string=? (string-reverse str1)
	    (string-map nucle-complement str2)))

(define (make-reverse-complement str1)
  (string-map nucle-complement  (string-reverse str1)))
(define (m-rc str1)
  (make-reverse-complement str1))

(define (eq-or-rev-comple str1 str2)
  (or (string=? str1 str2)
      (reverse-complement? str1 str2)))

(define (max-item lst pred)
  (if (null? lst)
      '()
      (max-item0 lst pred)))
	
(define (max-item0 lst pred)
  (if (= 1 (length lst))
      (car lst)
      (let ((fst (car lst))
	    (lst (max-item0 (cdr lst) pred)))
	(if (> (pred fst)(pred lst))
	    fst
	    lst))))


(define (max-item2 lst pred)
  (let ((fst (car lst)))
    (if (= 1 (length lst))
	(list fst (pred fst))
	(let  ((lst(max-item2 (cdr lst) pred)))
	  (if (> (pred fst)(pred (car lst)))
	      (list fst (pred fst))
	      lst)))))

(define (min-item lst pred)
  (if (null? lst)
      '()
      (min-item0 lst pred)))

(define (min-item0 lst pred)
  (if (= 1 (length lst))
      (car lst)
      (let ((fst (car lst))
	    (lst (min-item0 (cdr lst) pred)))
	(if (< (pred fst)(pred lst))
	    fst
	    lst))))

(define test '((a 7)(b 8)(c 15)(e 4)(d 3)(f 11)))
;;(max-item2 '(a b c f)(lambda(x)(cadr (assoc x test))))

(define (max-index-of nlst func)
  (let ((lst (map func nlst)))
    (index-of lst (apply max lst))))

(define (min-index-of nlst func)
  (let ((lst (map func nlst)))
    (index-of lst (apply min lst))))


(define (apply-every-n func lis n)
  (if (or (null? lis)(< (length lis) n))
      '()
      (cons (apply func (take lis n))
	    (apply-every-n func (drop lis n) n))))


(define (string-section str from to)
  (string-take (string-drop str from)(- to from)))

(define (delete-once x lst)
  (if (null? lst)
      '()
      (if (equal? (car lst) x)
	  (cdr lst)
	  (cons (car lst)(delete-once x (cdr lst))))))

(define (exclude set all)
  (if (null? set)
      all
      (exclude (cdr set)(delete-once (car set) all))))


(define (near=? x y)
  (< (abs (- x y)) 0.01))

(define (group-per lst n)
  (if (<= (length lst) n)
      (list lst)
      (cons (take lst n)(group-per (drop lst n) n))))

(define (quad-lessp tri1 tri2)
  (if (= (car tri1)(car tri2))
      (triple-lessp (cdr tri1)(cdr tri2))
      (< (car tri1)(car tri2))))

(define (triple-lessp tri1 tri2)
  (if (= (car tri1)(car tri2))
      (double-lessp (cdr tri1)(cdr tri2))
      (< (car tri1)(car tri2))))

(define (double-lessp pair1 pair2)
  (if (=  (car pair1)(car pair2))
      (< (cadr pair1)(cadr pair2))
      (< (car  pair1)(car  pair2))))
;;補集合
(define (set-complement set all)
  (filter (lambda(x)(not (member x set))) all))
;;集合の比較
(define (set=? set1 set2)
  (= (length set1)(length set2)(length (filter (lambda(x)(member x set2)) set1))))


(define (all-number lst)
  (andmap number? lst))

(define (include? set all)
  (andmap (lambda(x)(member x all)) set))


(define (make-group lst n)
  (if (<= (length lst) n)
      (list lst)
      (cons (take lst n)(make-group (drop lst n) n))))
;;注　lst1に重複があっても全部出力する(setでないということ)
(define (set-intersect lst1 lst2)
  (filter (lambda(x)(member x lst2)) lst1))

;;直積のべき乗 2021/06/17
#|
(define (cartesian-expt0 lst exponent)
  (if (= 1 exponent)
      lst
      (map flatten (cartesian-product (cartesian-expt lst (- exponent 1)) lst))))
|#

(define (cartesian-expt lst exponent)
  (if (= 1 exponent)
      (map list lst)
      (map flatten (cartesian-product (cartesian-expt lst (- exponent 1)) lst))))

;;
;;転置　2021/06/29=>2021/09/23
;;
(define (transpose listlist)
  (apply zip listlist))

;;; 小数点以下三位で丸める
(define (roundp3 f)
  (/ (round (* 1000 f)) 1000.0))
;;
;;listを空白区切りで表示
;;
(define (displaylist lst out)
  (for-each (lambda(x)(display (format "~a " x) out))
	    lst)
  (display "\n" out)
  )

;;
(define (list-rep lst n)
  (if (= 0 n)
      '()
      (append lst (list-rep lst (- n 1)))))

(define (list-hamming-distance list1 list2)
  (if (or (empty? list1)(empty? list2))
      0
      (+ (if (char=? (car list1)(car list2))
	     0 1)
	 (list-hamming-distance (cdr list1)(cdr list2)))))

(define (hamming-distance str1 str2)
  (let ((list1 (string->list str1))
	(list2 (string->list str2)))
    (list-hamming-distance list1 list2)))

(define (k-mers dna k)
  (if (= (string-length dna) k)
      (list dna)
      (cons (string-take dna k)
	    (k-mers (string-drop dna 1) k))))

(define (inc-hash! hash key)
  (hash-set! hash key (+ 1 (hash-ref hash key 0))))

(define (inc-hash2! hash key d)
  (hash-set! hash key (+ d (hash-ref hash key 0))))

(define (all-dnas len)
  (map (lambda(x)(apply string x))(cartesian-expt '(#\A #\C #\G #\T) len)))
;;; 上記の char list版
(define (all-dnas-list len)
  (cartesian-expt '(#\A #\C #\G #\T) len))

(define (all-combi str d)
  (let (( myhash (make-hash)))
    (all-combi0 (string->list str) d '() myhash)
    (hash-map myhash (lambda(x y)  (apply string x)))))


(define all-enki '(#\A #\C #\G #\T))

(define (all-combi0 lst d acc hash)
  ;(displayln (format "arg=~a acc=~a" lst acc))
  (if (= 0 (length lst))
      (hash-set! hash acc #t)
      (if (= d 0)
	  (hash-set! hash (append acc lst) #t)
	  (begin
	    (all-combi0 (cdr lst) d (append acc (list (car lst))) hash)
	    (for-each (lambda(e)(all-combi0 (cdr lst)(- d 1)(append acc (list e)) hash))
		      (delete  (car lst) all-enki))))))
;;--*-- compare --*--
;;(length (delete-duplicates (filter (lambda(dna)(< (hamming-distance dna "AAAA") 3))(all-dnas 4))))
;;(length (all-combi "AAAA" 2))
;;
(define (sub-list lst from len)
  (if (= 0 from)
      (take lst len)
      (sub-list (cdr lst)(- from 1) len)))


(define (classify-count lst class)
  (map (lambda(x)(count (lambda(c)(equal? c x)) lst))
       class))

(define (nuc-count charlist)
  (classify-count charlist all-enki))


(define (nuc2num nuc)
  (case nuc
    [(#\A) 0]
    [(#\C) 1]
    [(#\G) 2]
    [(#\T) 3]))


(define (score-1-line charlist)
  (- (length charlist) (apply max (map length (group-by identity charlist)))))

;; charのリスト用
(define (score-motif-matrix motifs)
  (apply + (map score-1-line (transpose motifs))))
;; string用
(define (score-motif-matrix-str motifsstrlist)
  (score-motif-matrix (map string->list motifsstrlist)))
(define (nuc->countlist char)
  (case char
    [(#\A) '(1 0 0 0)]
    [(#\C) '(0 1 0 0)]
    [(#\G) '(0 0 1 0)]
    [(#\T) '(0 0 0 1)]))

(define (dna->countlist charlist)
  (map nuc->countlist charlist))

(define (kmer-prob dna problist)
  (apply * (map (lambda(nuc prob)(list-ref prob (nuc2num nuc)))
		dna
		problist
		)))

(define (list-add lis1 lis2)
  (map + lis1 lis2))
(define (list-list-add lislis1 lislis2)
  (map list-add lislis1 lislis2))

(define (list-add* . listlist)
  (fold list-add (car listlist)(cdr listlist)))
;;
;; function name:motifs->profile
;;
;; input : '((#\A #\C #\G #\T)(#\A #\A #\A #\A)(#\T #\T #\C #\C))
;; output: '((2 0 0 1) (1 1 0 1) (1 1 1 0) (1 1 0 1))
;;
(define (motifs->profile motifs)
  (map nuc-count (transpose motifs)))
  
(define (motifs->profile-pseudo-count motifs . pseudo-count)
  (let ((psc (if (null? pseudo-count)
		 0
		 (car pseudo-count))))
    (map (lambda(x)
	   (list-add (make-list 4 psc) x))
	 (motifs->profile motifs))))
		     

			     

(define (delete-nth lst n)
  (if (= 0 n)
      (cdr lst)
      (cons (car lst)
	    (delete-nth (cdr lst)(- n 1)))))

  

;; hash-append
(define (hash-append! hash key item)
  (hash-set! hash key (append (hash-ref hash key '()) (list item))))
;; triange-number => number
(define (tri-num->num n)
  (/ ( + 1 (sqrt (+ 1 (* 8 n)))) 2))
;; common lispのmaplist　ただし lstは１つだけ funcは１引数のみ
(define (maplist func lst)
  (if (= 1 (length lst))
      (list  (func (list (car lst))))
      (cons (func lst)(maplist func (cdr lst)))))

;;
;; parse "1->2,3" or "4->5"
;;

(define (parse-adj-list str)
  (let* ((parsed (regexp-match #rx"([0-9]+) -> ([0-9]+(,[0-9]+)*)" str))
	 (src (string->number (cadr parsed)))
	 (dsts (map string->number (string-tokenize (caddr parsed) char-set:digit)))
	 (arc (list src dsts))
	 (graphs (map (lambda(x)(list src x)) dsts))
	 )
    graphs
    ;arc
    ))

(define (list-span list from to)
  (take (drop list from) (+ 1(- to from))))

(define (string->numlist str)
  (map string->number (string-tokenize str)))

(define (runsum0 nlist acc)
  (if (null? nlist)
      '()
      (let ((sum (+ (car nlist) acc)))
	(cons sum (runsum0 (cdr nlist) sum)))))

(define (runsum nlist)
  (runsum0 nlist 0)
)

;; (mapsub0   identity '(1 2 3 4 5))
;; ==> '((1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5))
(define (mapsub0 func lst)
  (let ((r (reverse lst)))
    (reverse
     (maplist (lambda(x)(func (reverse x))) r))))

(define (log10 x)
  (/ (log x)(log 10)))

)
	 
