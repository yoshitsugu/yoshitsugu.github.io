---
title: Stateモナドについてメモ
tags: Monad, Haskell
---

Stateモナドについて、自分なりにまとめる。自分用のメモなので詳しくしりたい人は下の参考リンクなどをみるほうがよいと思う。
<!--more-->

## モナドとは
モナドについては箱にたとえる方法や、関数型言語で副作用をあつかうための仕組み、などいろいろな説明がある[^1]。使ってみて感覚をつかむのがいいと思う。自分としてはコンテキストをあつかう、という説明がしっくりきた。そのうち自分の言葉で説明するエントリも書く予定。  
モナドのインスタンスとするには ``return`` と ``bind (>>=)``を定義する必要がある。  



## Stateモナドとは
変数の書き換えを行わない純粋な関数型言語で状態を扱うための仕組み。  
どうやって副作用をともなわずに状態をあつかっているのかというと、基本的には「前の状態」を入力として「次の状態」を出力とする(多分厳密には違う)  
モナドを使うことで手続的に状態を変更するように書くことができる。  
``State s a``のような型となり、``s``は状態の型、``a``は最終的な値の型となる。この順番をいつも忘れてしまうのだが、型があるのでわかりやすいともいえる。
  
現状のmtlパッケージの実装とは違うが、Monadのインスタンス化はだいたい以下のようにして行える。

```haskell
newtype State state a = State { runState :: state -> (a, state) }
instance Monad (State state) where
  return a = State (\s -> (a, s))
  State x >>= f = State (\s -> let (a, s') = x s in runState (f a) s')
```  


## 例
<a href="https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html" target="_blank">公式ドキュメント</a>から引用
``n + x``をStateモナドを使って実装している。

```haskell
tick :: State Int Int
tick = do n <- get
          put (n+1)
          return n

plusOne :: Int -> Int
plusOne n = execState tick n

plus :: Int -> Int -> Int
plus n x = execState (sequence $ replicate n tick) x
```

## 基本的な操作
### get
Stateから状態をとりだしてくる

### put
Stateの状態を更新する

### modify
get + put

## 実行系の操作
Monad系はよく``runXXX``のような関数をもち、手続的な書き方で処理を積んでおき、``runXXX``で実際に実行、という流れになる。Stateモナドは状態と値をもつので最終的にそれぞれを返す操作もある。

### runState
(値,状態)のタプルを返す。

```haskell
runState :: (State s a) -> s -> (a, s)
```

### execState
値はすてて状態を返す。

```haskell
execState :: (State s a) -> s -> s
```

### evalState
状態はすてて値を返す。

```haskell
evalState :: (State s a) -> s -> a
```

## 無限の猿定理

最近流行っているっぽい？無限の猿定理[^2]の一種、「ズン」と「ドコ」をランダムに繰り返し、「ズンズンズンズンドコ」になったら停止して「キヨシ」と表示するプログラムもStateモナドを使って書ける。
「ズン」の回数をカウントするのにStateモナドを使った。

```haskell
module Main where

import           Control.Monad.State
import           System.Random

data ZundokoCount = Start | Zun1 | Zun2 | Zun3 | Zun4 | Doko deriving (Show)

count :: String -> ZundokoCount -> ZundokoCount
count "ズン" Start = Zun1
count "ズン" Zun1 = Zun2
count "ズン" Zun2 = Zun3
count "ズン" Zun3 = Zun4
count "ドコ" Zun4 = Doko
count _ Doko = Doko
count _ _ = Start

zundoko :: StdGen -> StateT ZundokoCount IO ZundokoCount
zundoko gen = do
  z <- get             -- 状態の取り出し
  let (r, genN) = randomR (0,1) gen :: (Int, StdGen)
      str = if r == 0 then "ズン" else "ドコ"
      z' = count str z -- 次の状態
  liftIO $ putStr str
  put z'               -- 状態の更新
  case z' of
    Doko -> return z   -- Dokoだったらloop終了
    _ -> zundoko genN

main :: IO ()
main = do
  gen0 <- newStdGen
  runStateT (zundoko gen0) Start
  putStrLn " "
  putStrLn "キヨシ"
```


```bash
$ stack runghc Zundoko.hs 
ズンズンズンズンズンドコズンズンドコドコズンズンズンズンズンドコズンドコ
ドコズンズンドコドコドコズンズンドコズンドコドコドコズンドコドコドコズン
ドコズンズンドコズンドコズンズンズンズンドコ 
キヨシ
```

## 参考
* <a href="https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html" target="_blank">Control.Monad.State.Lazyドキュメント</a>
* <a href="https://wiki.haskell.org/State_Monad" target="_blank">Haskell Wiki - State Monad</a>
* <a href="http://qiita.com/7shi/items/2e9bff5d88302de1a9e9" target="_blank">Haskell 状態系モナド 超入門</a>
* <a href="http://qiita.com/lotz/items/503ef04b03433d29f77c" target="_blank">Stateモナドが便利に使えた！</a>
* <a href="http://gihyo.jp/book/2014/978-4-7741-6926-2" target="_blank">関数プログラミング実践入門</a>

[^1]: <a href="http://qiita.com/hiruberuto/items/8bbc0343bf794c368287" target="_blank">ポケモンにたとえるなんてものもある(!?)</a>
[^2]: <a href="https://ja.wikipedia.org/wiki/%E7%84%A1%E9%99%90%E3%81%AE%E7%8C%BF%E5%AE%9A%E7%90%86" target="_blank">Wikipedia</a>
