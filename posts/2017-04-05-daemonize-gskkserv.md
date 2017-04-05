---
title: Rust版Google IME skkserv を daemon化できるようにした
tags: Rust, SKK
---

[以前の記事](/posts/2016-11-01-google-ime-skkserv-in-rust.html)でRustでGoogle IMEのskkservを作ったことを紹介した。これをdaemon化できるオプションを追加した<!--more-->  

## 説明
とは言ってもそれだけなので、記事にするほどのものでもないが。。。  
[https://github.com/yoshitsugu/google-ime-skkserv-rs](https://github.com/yoshitsugu/google-ime-skkserv-rs) をcloneし、 `cargo build --release` したあと、 `./target/release/gskkserv -d ` するとdaemon化した状態で起動する。  

## 実装について
この実装には [http://knsd.github.io/daemonize/daemonize/index.html](http://knsd.github.io/daemonize/daemonize/index.html) のcrateを使用した。  
サクっとdaemon化できて便利だった。  
  
ついでに `try!` の代わりに `?` を使用するようにしたり、ちょこちょこと修正した。
