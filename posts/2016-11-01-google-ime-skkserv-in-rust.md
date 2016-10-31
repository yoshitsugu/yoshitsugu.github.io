---
title: RustでGoogle IME skkserv を作った
tags: Rust, SKK
---

Rustの勉強がてら、Google IME skkservを作った。<!--more-->

## GitHubリポジトリ
[https://github.com/yoshitsugu/google-ime-skkserv-rs](https://github.com/yoshitsugu/google-ime-skkserv-rs)

## これは何か
SKKという日本語入力のIMEがある。SKKサーバーというサーバーをたてることで、文字変換の際に単に辞書を参考するだけでなく、サーバーから受け取る形にすることができる。  
一方Google IMEには「Google CGI API for Japanese Input」というAPIがある。これは変換したい文字を投げてやると変換後の文字列が返ってくるものである。  
そこでGoogle CGI API for Japanese Inputに中継してくれるSKKサーバーをたてればSKKユーザーでもGoogle IMEでの変換を活用することができる。このツール自体は既にあり(ページ最下部「参考にしたページ」参照)、今回は必要に迫られた、というよりはRust勉強のために作った、という側面が強い。

## 使い方
1. Rustの環境を準備しておく。自分は[rustup](https://github.com/rust-lang-nursery/rustup.rs)を使っている。
2. 上のリポジトリをclone
3. `cargo install`
4. `gskkserv` で 55100ポートにskkservが待ち受けている状態になる。
    - `-h` オプションでlistenするhost, `-p` オプションでlistenするportをそれぞれ指定できるようにしている。

## 使っている様子
### Before
<img src="/images/20161101/before.gif" class="blog-img img-responsive">  
標準のSKK辞書では変換できない。

### After
<img src="/images/20161101/after.gif" class="blog-img img-responsive">  
一発で変換できる。

## 難しかった点
- skkservに渡される文字列がEUC_JPで、Google IMEのAPIを使うにはUTF-8に変換しなければならず、Rustでやると結構面倒だった。
- まだあまりownershipまわりの扱い方がしっくりきていないのでもうちょっと経験が必要そう。

## 今後
さすがにネットワークI/Oがあると一瞬待つので、変換結果をキャッシュできるようにしたい。  
また、今回Rustチュートリアルなどをさらっと見ながらつくっただけなので、コードの書き方についてももう少し洗練させていきたい。

## 参考にしたページ
- [Google IME SKK サーバー 作った - hitode909のダイアリー](http://blog.sushi.money/entry/20110421/1303274561)
- [Lightweight skkserv implementation for golang](https://github.com/akiym/go-skkserv)
