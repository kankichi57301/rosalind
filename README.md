rosalind 解答練習 by @kankichi57301
内容  
ros_[問題の略号2~4文字].ss (racket source)
ros_[問題の略号2~4文字].c  (C source)
data/rs_[問題の略号2~4文字][連番].txt データファイル
data/rosalind_[問題の略号2~4文字].txt ダウンロードしたデータファイル
data/[問題の略号2~4文字]_out.txt 出力
環境 win10(で作ったけどLinuxでもいけると思う。たぶん)　racketをインスコすること
使い方
>racket
>>(enter! "ros_[問題の略号2~4文字].ss")
>>>(ros_[問題の略号2~4文字] [引数])
引数は省略可　そのときはdata/rosalind_[問題の略号2~4文字].txtが対象
指定したときはdata/rs_[問題の略号2~4文字][連番].txt　を処理する
#lang racketで始まっていない一部のソースはenter!じゃなくてloadすること
(順次#lang racket　追加予定)