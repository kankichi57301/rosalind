-\*-rosalind 解答練習 by @kankichi57301-\*-  
内容  
＊各問題を解くソースファイル
ros_[問題の略号2\~4文字].ss (racket source)  
ros_[問題の略号2\~4文字].c  (C source)
~A.ssは上記からrequireされるものです。
ファイルの最初のほうのコメントに日付けと"AC"の文字が書いてあるソースはrosalindのダウソしたデータ
で動作確認済み（処理結果をウプしたらcorrectになったもの）
＊データファイル
data/rs_[問題の略号2\~4文字][連番].txt データファイル  
data/rosalind_[問題の略号2\~4文字].txt ダウンロードしたデータファイル  
data/[問題の略号2\~4文字]_out.txt 出力  
環境 win7(で作ったけどLinuxでもいけると思う。たぶん)　racketをインスコすること  
使い方
\>cd <ssファイルを置いたディレクトリ>
\>racket  
\>\>(enter! "ros_[問題の略号2~4文字].ss")  
\>\>\>(ros_[問題の略号2\~4文字] [引数])  
引数は省略可　そのときはdata/rosalind_[問題の略号2\~4文字].txtが対象  
指定したときはdata/rs_[問題の略号2\~4文字][連番].txt　を処理する  
#lang racketで始まっていない一部のソースはenter!じゃなくてloadすること  
(順次#lang racket　追加予定)  