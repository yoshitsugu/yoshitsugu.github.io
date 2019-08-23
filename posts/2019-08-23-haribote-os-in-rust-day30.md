---
title: 「30日でできる！OS自作入門」をRustで。30日目
tags: OS自作入門, OS, Rust
---

[「30日でできる！OS自作入門 」](https://book.mynavi.jp/supportsite/detail/4839919844.html)のC言語の部分をできるだけRustですすめてみる。今回で最後となる。
<!--more-->

アプリケーションを作っていく。特にソースを載せてもあまり意味がなさそうなので、ソースは載せず、概要と実行結果だけにとどめる。

## 数値演算をする

簡単な数値演算プログラムを作る。  
ソースは[こちら](https://github.com/yoshitsugu/hariboteos_in_rust/blob/master/apps/calc/src/lib.rs)を参照のこと。  
本の内容と同様、パーズ結果の中間表現のようなものは挟まず、パーズした木の端にあたるところからそのまま評価していく形の実装となる。

### 実行結果

<img src="/images/20190823/calc.png" class="blog-img img-responsive" alt="数値演算" title="数値演算"/> 


## テキストビューワを作る

スクロールつきでテキストを表示できるビューワを作る。  
ここで、コンソール以外のウィンドウでは日本語対応していないことに気づいたので、日本語対応部分をコンソール内から外に切り出した。(本のコードでは元々そうなっていた。)


### 実行結果

<img src="/images/20190823/tview.gif" class="blog-img img-responsive" alt="テキストビューワ" title="テキストビューワ"/> 


## 画像ビューワを作る

本では音楽再生のアプリも作っているが、QEMUだと音がならないため、とばして画像ビューワを作る。  
ソースは[こちら]()を参照のこと。

### 実行結果

<img src="/images/20190823/gview.gif" class="blog-img img-responsive" alt="画像ビューワ" title="画像ビューワ"/> 


30日目は以上となる。ここまでの内容のコードは[yoshitsugu/hariboteos_in_rustのday30](https://github.com/yoshitsugu/hariboteos_in_rust/tree/day30)としてタグを打ってある。


## 感想など

ここまでで本の内容で実装したいものは実装できたので一旦終了とする。 
折角なので、感想も書いておく。  
  
自分はあまりOSなどいわゆる低レイヤーと呼ばれるような部分について詳しくなく、もう少し詳しくなりたい、という思いからこの本を読みはじめた。そういう意味では、かなり勉強になったと思う。今の最新のOSはそもそもブートがUEFIになっていたり、メモリ管理にはセグメンテーションではなくページングが必須になっていたり[^1]とかなり違うようだが、エッセンスを学ぶにはよかったように思う。  
  
また、勉強中のRustで書いたため、余計時間がかかった部分もあったが、デバッグ時のエラーメッセージや、所有権の仕組みによるメモリ汚染の防止など、Rustの洗練されている部分の恩恵もかなり受けられたのではないかと思う。  
とはいえ、本のCのコードをそのまま翻訳したような`unsafe`なコードばかり書いてしまったのでそこはもう少しやり方を変えてもよかったかもしれない。  
  
最後に、3ヶ月近くかかってしまったが、楽しくすすめられたのでよかったと思う。  

<img src="/images/20190823/haribote.png" class="blog-img img-responsive" />


[^1]: [こちらのスライド](https://docs.google.com/presentation/d/1mbLk70RKi-ExLzb78WosCu5DujpQN2K7B9iSbUGRXq4/mobilepresent) が現代のOSの差分を知るのにちょうどよかった。
