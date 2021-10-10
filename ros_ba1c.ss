#lang racket
;; rosalind
;; Find the Reverse Complement of a String
;; [BA1C] 2021/07/08  AC
#|
How to execute
>racket
>>(enter! "ros_ba1c.ss")
>>(ros_ba1c 1)
引数の1は　"data\\rs_ba1c1.txt"を対象に操作することを指定する
(ros_ba1c)と引数を省略すると"data\\rosalind_ba1c.txt"(本番用ダウロードファイル)が対象になる
結果は変数*ba1c_out*の値"data\\ba1c_out.txt"で示すファイルに出力される。
|#

(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(define *ba1c_out* "data\\ba1c_out.txt")

(define (ros_ba1c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba1c.txt"
		    (format "data\\rs_ba1c~a.txt" (car n)))))
	 (res (make-reverse-complement (car data)))
       )
    (call-with-output-file *ba1c_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)
   res 
))

	 
				

