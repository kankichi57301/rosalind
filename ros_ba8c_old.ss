#lang racket
;; rosalind
;; Implement the Lloyd Algorithm for k-Means Clustering
;; [BA8C] 2021/09/15 
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba8c_out* "ba9c_out.txt")

(define (ros_ba8c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba8c.txt"
		    (format "rs_ba8c~a.txt" (car n)))))
	 (firstline  (string->numlist (car data)))
	 (k (car firstline))
	 (m (cadr firstline))
	 (points  (map string->numlist (cdr data)))
	 (n (length points))
	 )
    points
    #|
    (call-with-output-file *ba8b_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))



;;
;;k-means Clustering
;;


(define centers '())
(define cluster '())  ;;n番目の点が属するクラスタ
(define *k* 0)          ;;クラスタの数
(define *m* 0)          ;;次元
(define *n* 2)

(define (k-means points k)
  (set! *n* (length points))
  (set! *k* k)
  (set! cluster (random-cluster points k))
  cluster
  )
;;
;;同じ長さ（次元）の点の集合の重心を計算する
;;
(define (calc-centroid points)
  (let ((n (length points))
	(tr (transpose points)))
    
    (map (lambda(x)(/ (apply + x) n)) tr)

  ))

;; n個の1~kまでの乱数を作る
(define (random-cluster n k)
  (shuffle
   (append (iota k 1)(map (lambda(x)(+ 1 (random k)))(iota (- n k))))))

(define (devide-cluster points cluster )
  (let* ((pasted (map list cluster points))
	 (grouped (group-by car pasted)))
    ;grouped
    (map (lambda(x)(map cadr x)) grouped)
))
;;
(define p1 '((2)(4)(6)(8)(10)(3)))
(define cl '(1 1 0 0 0 1))

(define (calc-centers points cluster)
  (map (lambda(x)(apply calc-centroid x))(devide-cluster points cluster)))

;;(calc-centroid p1 cl)
