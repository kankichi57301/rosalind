(require srfi/1)
(require srfi/13)
;(displayln "roslib loaded")

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
#|
(define (transpose mat)
  (map (lambda(y)(map (lambda(x)(list-ref x y)) mat))
       (iota (length (car mat)))))
|#
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
;;��W��
(define (set-complement set all)
  (filter (lambda(x)(not (member x set))) all))
;;�W���̔�r
(define (set=? set1 set2)
  (= (length set1)(length set2)(length (filter (lambda(x)(member x set2)) set1))))


(define (all-number lst)
  (andmap number? lst))

(define (include? set all)
  (andmap (lambda(x)(member x all)) set))

(define (delete-nth lst pos)
  (if (= 0 pos)
      (cdr lst)
      (cons (car lst)
	    (delete-nth (cdr lst)(- pos 1)))))

(define (make-group lst n)
  (if (<= (length lst) n)
      (list lst)
      (cons (take lst n)(make-group (drop lst n) n))))
;;���@lst1�ɏd���������Ă��S���o�͂���(set�łȂ��Ƃ�������)
(define (set-intersect lst1 lst2)
  (filter (lambda(x)(member x lst2)) lst1))

;;���ςׂ̂��� 2021/06/17
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
;;�]�u�@2021/06/29
;;
(define (transpose listlist)
  #|
  (let ((len (length (car listlist))))
    (map (lambda(i)
	   (map (lambda(lis)(list-ref lis i))listlist)) 
	 (iota len))))
  |#
  (apply map list listlist))

;;; �����_�ȉ��O�ʂŊۂ߂�
(define (roundp3 f)
  (/ (round (* 1000 f)) 1000.0))
;;
;;list���󔒋�؂�ŕ\��
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
;;; ��L�� char list��
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

;; char�̃��X�g�p
(define (score-motif-matrix motifs)
  (apply + (map score-1-line (transpose motifs))))
;; string�p
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
  
(define (motifs->profile-pseudo-count motifs)
  (map (lambda(x)(list-add '(1 1 1 1) (nuc-count x))) (transpose motifs)))

 
;; charlist����prof�����ɍő�̊m����kmer��Ԃ�
(define (find-max-prob charlist profile k)
  (let* ((first-kmer (take charlist k)))
    (find-max-prob0 charlist profile first-kmer (kmer-prob first-kmer profile) k)))

(define (find-max-prob0 charlist profile ans maxval k)
  (if (< (length charlist) k)
      (begin ;(displayln (format "ans=~a prob=~a" ans maxval))
	     ans)
      (let* ((kmers (take charlist k))
	     (val (kmer-prob kmers profile)))
	(if (> val maxval)
	    (find-max-prob0 (cdr charlist) profile kmers val    k)      ;; update
	    (find-max-prob0 (cdr charlist) profile ans   maxval k)      ;; not update
	    ))))

;; hash-append
(define (hash-append! hash key item)
  (hash-set! hash key (append (hash-ref hash key '()) (list item))))
;; triange-number => number
(define (tri-num->num n)
  (/ ( + 1 (sqrt (+ 1 (* 8 n)))) 2))
;; common lisp��maplist�@������ lst�͂P���� func�͂P�����̂�
(define (maplist func lst)
  (if (null? lst)
      (list (func lst))
      (cons (func lst)(maplist func (cdr lst)))))

;;
;; parse "1->2,3" or "4->5"
;;

(define (parse-adj-list str)
  (let* ((parsed (regexp-match #rx"([0-9]+) -> ([0-9]+(,[0-9]+)*)" str))
	 (arc (list (string->number (cadr parsed))
		    (map string->number (string-tokenize (caddr parsed) char-set:digit)))))

    arc))

(define (list-span list from to)
  (take (drop list from) (+ 1(- to from))))
