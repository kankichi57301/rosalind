#lang racket
;; rosalind
;; Find the Most Frequent Words in a String
;; [BA1B] 2021/07/08 AC
#|
How to execute
>racket
>>(enter! "ros_ba1b.ss")
>>(ros_ba1b 1)
引数の1は　"rs_ba1b1.txt"を対象に操作することを指定する
(ros_ba1b)と引数を省略すると"rosalind_ba1b.txt"(本番用ダウロードファイル)が対象になる
結果は変数*ba1b_out*の値"ba1b_out.txt"で示すファイルに出力される。
|#

(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")

(define *ba1b_out* "data\\ba1b_out.txt")
(define myhash #f)


(define (ros_ba1b . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba1b.txt"
		    (format "data\\rs_ba1b~a.txt" (car n)))))
	 (dna (car data))
	 (k   (string->number (cadr data)))
	 (res '())
	 )
    (set! myhash (make-hash))
    ;(displayln dna)
    (set! res (solve-ba1b dna k))
    (call-with-output-file *ba1b_out*
      (lambda(out)
	(for-each (lambda(w)
		    (display (format "~a " w) out))
		  res))
      #:exists 'truncate/replace)
    res
    ))

(define (solve-ba1b dna k)
  (ba1b-hash dna k)
  (let* ((hash-res (hash-map myhash list))
	 (maxval (cadar (sort hash-res (lambda(x y)(> (cadr x)(cadr y)))))))
    
    (map car (filter (lambda(pair)(= maxval (cadr pair))) hash-res))))
  
  
(define (ba1b-hash str k)
  (when (>= (string-length str) k)
	(begin
	  (inc-hash! myhash (string-take str k))
	  (solve-ba1b (string-drop str 1) k))))
		    
