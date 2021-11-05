#lang racket
;; rosalind
;; Implement Leaderboard Cyclopeptide Sequencing
;; [BA4G] 2021/11/05 AC
;; 参考: https://www.youtube.com/watch?v=Qbnvs1CfRYY&t=202s
(require srfi/1)
(require srfi/13)
(require srfi/19)
(require "readfileA.ss")
(require "monoisotopicA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba4g_out* "data\\ba4g_out.txt")
(define *leader-board* '())
(define *leader* '())

(define *spect* '())

(define *time* #f)


(define (ros_ba4g . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba4g.txt"
		    (format "data\\rs_ba4g~a.txt" (car n)))))
	 (k (string->number (car data)))
	 (spect (map string->number (string-tokenize(cadr data))))
	 (parent (car (take-right spect 1)))
	 (res '())
	 )
					;(set! *leader-board* '(()))
    ;(displayln spect)
    
    (set! *spect* spect)
    
    ;(displayln (format "k=~a" k))
    ;(displayln (format "parent=~a" parent))
    ;(displayln (format "spect count=~a" (length spect)))
    (set! *time* (current-time))
    (set! res (string-join (map number->string (leaderboard spect k parent)) "-"))
    (displayln (format "elapsed =~a sec" (time-second (time-difference (current-time) *time*))))
    
    (call-with-output-file *ba4g_out*
      (lambda(out)
	(displayln res out))
      #:exists 'truncate/replace)
    
    res
    
))

(define (add-18-amino nlistlist)
  (append-map (lambda(nlist)(map (lambda(amino)(cons amino nlist)) amw)) nlistlist))

;; itemsのそれぞれにfuncを適用した結果が上位k位以内になるitem返す
(define (top-n items k func)
  (let ((koujun (sort items (lambda(x y)(> (func x)(func y))))))
    (if (<= (length items) k)
	koujun
	(let* ((border (list-ref koujun (- k 1)))
	       (bds (filter (lambda(x)(= (func x)(func border))) koujun)))
	  ;(displayln (format "boader=~a" border))
	  ;(displayln (format "bds=~a" bds))
	  (if (= (length bds) 1)
	      (take koujun k)
	      (union* (take koujun k) bds))))))

;;--*--
;;(top-n '(1 2 3.6 3.3 3.4 3.5 4 5)) 3 floor)


(define (leaderboard spect k parent)

  (leaderboard-loop '(()) spect k parent 0)
  *leader*
  )

(define (leaderboard-loop board spect k parent cnt)
  ;(displayln (format "loop cnt=~a" cnt ))
  (if (> cnt 1000)
      #t
      (let ((next-leader '()))
	(if (= 0 (length board))
	    #t
	    (begin
	      (if (= cnt 0)
		  
		  (set! board (map (lambda(x)`((,x),(if (member x *spect*) 1 0))) amw))
		  
		  (begin
		    ;(displayln (format "len:=~a" (length board)))
		    ;(set! *x1* board)
		    (set! board (add-amino-score board spect))
		    ;(set! *x2* board)
		    (set! board (top-n board k (lambda(x)(cadr x))))
		    ;(set! *x3* board)
		    ))
	      ;(displayln (format "board len=~a" (length board)))
	      (flush-output)
	      
	      (let ((parent-match (filter (lambda(x)(= parent (apply + (car x)))) board))) ;; parent weightと合致したリストを取り出す
		(when (not (null? parent-match))
		      (begin
			(set! next-leader (car (max-item parent-match (lambda(x)(mass-score (car x) spect)))))
			(when (or (null? *leader*)(> (mass-score next-leader spect)(mass-score *leader* spect)))
			      (set! *leader* next-leader)))))
	      
	      (set! board (filter (lambda(x)(< (apply + (car x)) parent)) board))
	      (leaderboard-loop board spect k parent (+ 1 cnt)))
	      
	      ))))

(include "expandpeptide.ss") 	  
;;(ros_ba4g 1)		  


