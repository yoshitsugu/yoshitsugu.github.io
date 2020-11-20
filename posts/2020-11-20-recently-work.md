---
title: 最近やった/やっていること
tags: Rust, Zig, C Compiler, GraphQL
---

気づけば前回のブログから 1 年近くたってしまったので、最近やった/やっていることを書く。

<!--more-->

## Rust で Text Editor

- [yoshitsugu/kilo_rust](https://github.com/yoshitsugu/kilo_rust)
- [Build Your Own Text Editor](https://viewsourcecode.org/snaptoken/kilo/) を Rust でやった。
- [rhysd/kiro-editor](https://github.com/rhysd/kiro-editor) という偉大な先達があり、多いに参考にさせていただいた。
- エスケープシーケンスを使ってカーソルや文字色の制御で遊ぶのは楽しかった。

## Zig で C Compiler

- [yoshitsugu/zugcc](https://github.com/yoshitsugu/zugcc)
- Rui さんの [chibicc](https://github.com/rui314/chibicc) を Zig 言語でクローンしている。
- chibicc 自体がインクリメンタルな開発をしているので、非常に参考になる。
- 途中までだが [低レイヤを知りたい人のための C コンパイラ作成入門](https://www.sigbus.info/compilerbook) も参考になる。

## その他近況メモ

- 社内で「お茶会」と称して最近やっていることをゆるく共有する会があるので、こういうことをブログに書く前に満足してしまう。
- 仕事では Web フロントエンドをさわっていることが多い。最近は専ら Vue / Nuxt。
  - 個人的には React の hooks が好きなので React を使いたい。
- [Rust で GraphQL サーバ + React SPA の Web アプリケーション](https://github.com/yoshitsugu/holacircle)なども作ったが、GraphQL はよかった。
  - GraphQL で作りたいインターフェースを記述すれば、あとは Rust の型に導かれるままに、自動的に API ができていく感じがあり、開発体験がとてもよかった。
