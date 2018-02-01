---
title: ブログ(Hakyll)のデプロイをDockerを使って高速化した
tags: Haskell,TravisCI,Hakyll,Docker
---
このブログのデプロイが遅いのが苦痛だったので対策した。<!--more-->

## 問題点
このブログはTravisCIでデプロイしているのだが、一ヶ月に1回くらいしか更新しないためか、キャッシュもあまり効かず、毎回30分くらいはかかってしまう。(キャッシュが効くときは3分くらいでおわることもある。)  
TravisCIで行っているデプロイ手順としては以下のようになっている。

1. Stackの準備
2. GHCの準備
3. Hakyllとブログ用Haskellコードのコンパイル
4. ブログ用のMarkdownをHTMLに変換
5. GitHub Pagesにデプロイ

このうち、1-3まではほぼ毎回変わらないにも関わらず、この時間が大きな部分を占めていた。

## 解決方法
上記1-3までを行ったDocker imageをあらかじめ用意して、TravisCI上は

1. Docker imageのpull
2. ブログ用のMarkdownをHTMLに変換
3. GitHub Pagesにデプロイ

のみで済むようにした

## デプロイ用Docker imageの作成
- ライブラリをいろいろインストールするのが面倒だったので `buildpack-deps:jessie` から作るようにした
- リポジトリを一個作って、Docker HubでAutomated Buildされるようにした。
  [yoshitsugu/yoshitsugu.github.io-deployer-docker](https://github.com/yoshitsugu/yoshitsugu.github.io-deployer-docker)


## .travis.yml の修正

不要なライブラリを消去してinstall部分も以下のように修正した。  

- 旧バージョン

```yaml
install:
  - stack --no-terminal setup
  - stack exec -- ghc --version
  - stack --no-terminal build
  - stack exec -- blog build
```

- 新バージョン

```yaml
install:
  - docker pull ${DEPLOYER_NAME}
  - docker run -v $(pwd):/yoshitsugu.github.io --rm ${DEPLOYER_NAME} bash -c "cd /yoshitsugu.github.io && LC_ALL=C.UTF-8 stack exec -- blog build"
```
 

## 結果
- 以前
<div>![以前](/images/20180201/old.png)</div>
- 現在
<div>![現在](/images/20180201/new.png)</div>
速くなった
