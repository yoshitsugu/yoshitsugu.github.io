---
title: Slack Slashコマンド + Rust で遊んだ
tags: Slack, Rust
---

[弊社ブログ](http://sikmi.com/blog/sikmi_kaizen/slack-slash-command-pomodoro)に書いたが、Slack Slashコマンドでポモドーロ数を測れるようにした。  
[https://github.com/yoshitsugu/slash-pomo](https://github.com/yoshitsugu/slash-pomo)  
背景などは弊社ブログに書いた通りなので、ここでは実装面について書く。<!--more-->  

## 使った言語、ライブラリなど

### [Rust](https://www.rust-lang.org)
個人的に今勉強中なのでRustで書いてみた。  
このツール自体は社内で使ってもらっているが、実装は業務外の時間にやった&別になくても困らないものなので、趣味優先にした。

### [Rocket](https://rocket.rs/)
ついでにWebフレームワークも最近でたRocketを使っている。  
RustのWebフレームワークとしては[nickel](http://nickel.rs/)とか[Iron](http://ironframework.io/)などがあるようだ。  
Rocketは最近v0.2.0がでたが、まだ追従できていない。(現在0.1.6)

### [Redis](https://redis.io/)
カウントの記憶にはRedisを使った。  
普段手軽に使っているものなので使ったが、他にも選択肢はいろいろあったかもしれない。あまり調べられてはいない。  
RustからRedisを呼び出すのには[redis-rs](https://github.com/mitsuhiko/redis-rs)を使った。

## 実行環境
### [IBM Bluemix](https://www.ibm.com/cloud-computing/jp/ja/bluemix/)
Webサーバを動かす環境としてはIBMのBluemixを使った。  
HerokuではないのはSlashコマンドの仕様として3秒以内にレスポンスしないとタイムアウトしてしまう[^1]ため、しばらくするとsleepしてしまうHerokuだと厳しかったから。  
Redisは[Redis Cloud](https://console.ng.bluemix.net/catalog/services/redis-cloud)を使った。30MBのプランなら無料なので便利だ。   
[公式wiki？](https://github.com/cloudfoundry-community/cf-docs-contrib/wiki/Buildpacks)には[heroku-buildpack-rust](https://github.com/emk/heroku-buildpack-rust)がそのまま使えると書いてあったが使えなかった。[仕方なくforkしてちょっと修正した。](https://github.com/yoshitsugu/heroku-buildpack-rust)

## 感想
とりあえず動くものができたので追ってリファクタリングや自動テストなどやりたい。  
多分慣れている人が見たらまだまだツッコミどころ満載のコードだと思うが個人的には大分Rustの感覚が養われてきたように思う。この段階で再度基礎の勉強をしてもいいかもしれないと思っている。  
また、こういうオモチャみたいなものでも、動くものを作るのはやはり楽しい。

[^1]: [https://api.slack.com/slash-commands](https://api.slack.com/slash-commands) には一応3秒以内に返せない場合の解決策もあるがシンプルにやりたかった。

