#lang racket
;;
;; filename:7ctest.ss
;;rosalind ba7cのテストデータ作成用
;;--*-- test data--*--
(require srfi/1)
(require srfi/13)
(require "roslibA.ss")
(require "roslibB.ss")
;;隣接リスト　親->子の順番
(define adj-list0
  '((0 1 12)
    (0 2 8)
    (1 3 9)
    (1 4 17)
    (2 5 7)
    (2 6 5)
    (3 7 10)
    (3 8 5)
    (4 9 6)
    (4 10 14)
    (5 11 18)
    (5 12 11)
    (6 13 7)
    (6 14 9)
    ))

(define adj-list1
  (append adj-list0
	  '((7 15 12)
	    (7 16 8))))

(define  adj-list2
	 '((0 1 2)
	   (0 2 3)
	   (1 3 2)
	   (2 5 4)
	   (1 4 7)
	   (2 6 1)))

(define adj-list3
  (append adj-list2
	  '((6 7 3)
	    (6 8 1))))
	  
(define  adj-list4
	 '((0 1 2)
	   (0 2 3)
	   (1 3 5)
	   (2 4 3)
	   (1 5 4)
	   (2 6 4)))


(define (get-leaves adjlist)
  (exclude (delete-duplicates (map car  adjlist))
	   (delete-duplicates (map cadr adjlist))))

(define (path-to-root adjlist node)
  (reverse (path-to-root0 adjlist node '())))


(define (path-to-root0 adjlist node acc)
  ;;(displayln (format "node=~a" node))
  (let ((parent (find-first (lambda(x)(equal? (cadr x) node)) adjlist )))
    (if (not parent)        ;; root node
	(cons node acc)
	(path-to-root0 adjlist (car parent) (cons node acc)))))
    
(define (common-ancestor adjlist node1 node2)
  (find-first (lambda(x)(member x (path-to-root adjlist node1)))
	      (path-to-root adjlist node2)))
;;
;;　ancsがdescの親ノードであること。
(define (limb-len0 adjlist desc ances)
  (let ((it (find-first (lambda(x)(equal? (take x 2) `(,ances ,desc))) adjlist)))
    (if it
	(caddr it)
	#f)))

(define (limb-len adjlist desc ances)
  ;;(displayln (format "~a : ~a" desc ances))
  (if (equal? desc ances)
      0
      (let ((it (find-first (lambda(x)(equal? (cadr x) desc)) adjlist)))
	(if it
	    (+ (caddr it)(limb-len adjlist (car it) ances))
	    #f))))

(define (leaf-dist adjlist leaf1 leaf2)
  (let ((common (common-ancestor adjlist leaf1 leaf2)))
    ;;(displayln (format "common=~a" common))
    (if common
	(+ (limb-len adjlist leaf1 common)
	   (limb-len adjlist leaf2 common))
	#f)))

(define (adjlist->distMat adjlist)
  (let ((leaves (get-leaves adjlist)))
    (map (lambda(i)
	   (map (lambda(j)
		  (leaf-dist adjlist i j))
		leaves))
	 leaves)))

;;
;; (adjlist->distMat adj-list2)
;;
(define (make-ba7c-data adjlist . n0)
  
  (let ((res (adjlist->distMat adjlist))
	(n (if (null? n0)
		1
		(car n0)))
	)
    (call-with-output-file (format "data\\rs_ba7c~a.txt" n)
      (lambda(out)
	(displayln (length (car res)) out)
	(for-each (lambda(line)
		    (for-each (lambda(x)(display (format "~a " x) out)) line)
		    (display "\n" out))
		  res))
      #:exists 'truncate/replace)
    res
    ))
      
