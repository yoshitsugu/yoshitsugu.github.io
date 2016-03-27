---
title: Haskell Servant, HRR, ElmでSPAを作っている
tags: haskell-servant, haskell-relational-query, Elm
---

<a href="http://rundis.github.io/blog/2015/haskell_elm_spa_part1.html" target="_blank">Typed up CRUD SPA with Haskell and Elm</a>の記事に触発される形で最近SPAを作りはじめた。<!--more-->

## ソース
自分用のチュートリアルコードなので、晒すのもためらわれるが、載せておく。
<a href="https://github.com/yoshitsugu/hagemai" target="_blank">https://github.com/yoshitsugu/hagemai</a>

## 作りたいもの 
<a href="http://www.daifukuya.com/kagemai/" target="_blank">影舞</a>をご存知だろうか。シンプルだが結構つかいやすいバグトラッキングシステムである。今回はこれを目標に作りはじめてみた。  
おおまかな機能として以下のようなものがある。

* バグを登録できる。
* バグに関連するやりとりを追加できる。
    - やりとりメッセージの追加時にバグ自体の状態も変更できる。
* バグの状態管理(受付中、確認中、など)ができる。
    - 影舞では状態を動的に増やすことができる。

尚、Haskellでつくる影舞、ということでHagemaiという名前にしたが、特に毛髪関連の他意はない。

## 使用する技術要素
シンプルなシステムにはオーバーエンジニアリングなのは重々承知だが、勉強のため、SPAで作る。

* バックエンドはHaskell
    - APIサーバには<a href="https://haskell-servant.github.io/" target="_blank">haskell-servant</a>を使う。
        - APIを型で表現できる。
    - DBとのやりとりには<a href="https://khibino.github.io/haskell-relational-record/" target="_blank">haskell-relational-record</a>(HRR)を使う
        - 仕組みがまだよくわかっていないのだが、SQLに型をつけることでコンパイル時に正しいSQL文なのかチェックできる。


* フロントエンドは<a href="http://elm-lang.org/" target="_blank">Elm</a>
    - Haskellライクなシンタックスを持つAltJSで、FRPのフレームワークをあつかえる。

## 進捗
とりあえすバグの登録とコメントの追加はできた。

* 見た目は<a href="https://nkmr6194.github.io/Umi/" target="_blank">Umi</a>をつかっている。
* レイアウトは現在のところ結構適当だ。

### バグの登録
<img src="/images/hagemai_create.gif" class="blog-img img-responsive" >

### コメントの追加
影舞と同じように、タイトル、優先度などバグ自体もここで更新できる。
<img src="/images/hagemai_create_comment.gif" class="blog-img img-responsive" >



## ここまで作った感想など

### 影舞意外と高機能
シンプルだと思っていたけど、意外といろいろやっていることが細かく見ていくとわかった。

- そもそも状態数を任意に増やせる設計にするのは割と面倒。
- 各コメントにページ内リンクつけてる
- captca機能がある？未確認


### HDBC-MySQLの問題？

```haskell
Exception: SqlError {seState = "", seNativeError = 2014,
seErrorMsg = "Commands out of sync; you can't run this command now"}
```
というエラーがでてハマった。

  - <a href="http://blog.xaxxi.net/2014/02/08/hdbc-mysql%E3%82%92%E4%BD%BF%E3%81%86%E3%81%AE%E3%81%8C%E9%9B%A3%E3%81%97%E3%81%84/" target="_blank">HDBC-mysqlを使うのが難しい</a>
  - <a href="http://stackoverflow.com/questions/8027948/hdbc-mysql-command-out-of-sync" target="_blank">HDBC-mysql “command out of sync”</a>

このあたりの問題っぽいけど自分のtransactionの使い方が間違っている気もする。


### Haskell <-> Elm の差
同じっぽいけど微妙に違うシンタックスなので、HaskellとElmのコードを行き来していると結構イラっとする。

### 何はともあれ楽しい。
コンパイル通った時の安心感はやはりHaskell, Elmのメリットだとおもう。あと、リファクタリングとかも型でどこを直せばいいかわかりやすいので、やりやすいような気がする。

## 今後
SPAは仕事ではほとんど作ったことがなく、せっかくなので知見をためておきたい。  
とりあえず飽きるまではやる。そもそもがオーバーエンジニアリングなやつなので、今のところ特にちゃんとした製品にしようとは考えていない。
