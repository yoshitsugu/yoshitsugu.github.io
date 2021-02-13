---
title: HaskellのLinearTypes言語拡張について少し調べた
tags: Linear Haskell, Haskell, LinearTypes
---

最近リリースされた [GHC 9.0.1](https://www.haskell.org/ghc/blog/20210204-ghc-9.0.1-released.html) から使えるようになった `LinearTypes` 言語拡張について気になったので調べた。

<!--more-->

## LinearTypes言語拡張とは

GHC9.0.1から使えるようになった言語拡張で、Linear Typeを導入できる。ただ、上記リリースノートに `a first cut` とある通り、まだ実験的な機能としてリリースされた段階のようだ。通常のGHCの言語拡張のように

```haskell
{-# LANGUAGE LinearTypes #-}
```

とすることで使えるようになる

## Linear Typeとは

そもそもLinearTypesで使えるようになるLinear Typeとはどのような概念なのか、簡単に説明すると、「関数のある引数がちょうど1度だけ評価される、という条件を指定できるもの」のようだ。  
具体例を挙げる。

```haskell
{-# LANGUAGE LinearTypes #-}

module Main where

-- a %1 -> b でaがLinear Typeであることを指定できる
tuple :: Int %1 -> Int %1 -> (Int, Int)
tuple a b = (a, b)

tuple2 :: Int %1 -> Int %1 -> (Int, Int)
tuple2 a b = (a, a) 
```

としてコンパイルすると以下のようなエラーがでる

```bash
$ cabal build
# ...
Main.hs:10:8: error:
    • Couldn't match type ‘'Many’ with ‘'One’
        arising from multiplicity of ‘a’
    • In an equation for ‘tuple2’: tuple2 a b = (a, a)
   |
10 | tuple2 a b = (a, a)
   |        ^

Main.hs:10:10: error:
    • Couldn't match type ‘'Many’ with ‘'One’
        arising from multiplicity of ‘b’
    • In an equation for ‘tuple2’: tuple2 a b = (a, a)
   |
10 | tuple2 a b = (a, a)
   |          ^
```

tupleはコンパイル可能だが、tuple2は2箇所でエラーとなる。
エラー内容はそれぞれ

- a はLinear Typeなのに2回評価されているのでエラー
- b はLinear Typeなのに1回も評価されていないのでエラー

となる。このように「ただ1度だけ評価される」という制約を導入できるのがLinear Typeである。Linear type systemsというのは前から理論的には知られていたものの、なかなか実装までは至らず、今回Haskellで晴れて実装までこぎつけたようだ。  
proposalは [こちら](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst) にある。  
また、論文も [こちら](https://arxiv.org/abs/1710.09756) で公開されていたため、かいつまんで読んでみた。

## Linear Typeが何の役に立つのか

論文によると、下記2点にフォーカスして実装をすすめたそうだ。

- 値の変更の安全性
- 外部リソース(ファイルやネットワークなど)にアクセスする際の安全性

Linear Typeがこの2点にどのように対応できるのか、論文の例を挙げつつ見ていく。  

### 値の変更の安全性

以下のようなArrayに関する変更を安全にすることを考える

```haskell
array :: Int -> [(Int, a)] -> Array a
array size pairs = runST $ do
  ma <- newMArray size
  forM_ pairs (write ma)
  unsafeFreeze ma

-- Arrayまわりの関数の型が以下のようになっているとする
type MArray s a
type Array a
newMArray :: Int -> ST s (MArray s a)
read :: MArray s a -> Int -> ST s a
write :: MArray s a -> (Int, a) -> ST s ()
unsafeFreeze :: MArray s a -> ST s (Array a)
forM_ :: Monad m => [a] -> (a -> m()) -> m()
runST :: (forall s. ST s a) -> a
```

上記 `array` という関数はarrayの長さを第1引数に、インデックスと値のタプルのリストを第2引数に受けとり、immutableなArrayを生成する。ここで `unsafeFreeze` は、freeze後の挙動については制約をかけることができず、状況によってはfreeze後にwriteされる可能性もあり得る。  
これをLinear Typeを使って以下のように定義し直す。

```haskell
array :: Int -> [(Int, a)] -> Array a
array size pairs = newMArray size (\ma -> freeze (foldl write ma pairs))

type MArray a
type Array a
newMArray :: Int -> (MArray a %1 -> Unrestricted b) %1 -> b
read :: MArray a %1 -> Int -> (MArray a, Unrestricted a)
write :: MArray a %1 -> (Int, a) -> MArray a
freeze :: MArray a %1 -> Unrestricted (Array a)
```

ここで、 `read` や `write` はLinear Typeになっているので1度しかMArrayを評価できない。ただ、返り値としてMArrayを返すので、論理的には新しい `MArray` ということになり、 `read` と `write` をつなげて書くことができる。  
`freeze` は引数を消費して、 `Unrestricted (Array a)` としてimmutableなArrayを返している。そのため、これ以上 `MArray` として `read` や `write` ができない。
`Unrestricted` というのはLinear Typeの制限を受けない値として扱うことを意味する。つまり何度でも評価されるかもしれないし、1度も評価されないかもしれない。 `Array` 自体はimmutableなので `Unrestricted` でも安全、ということだろう。  
この __「関数は引数を消費する。その後の関数でも続けて消費できる場合は論理的に新しい値として返す」__ という流れがLinear Typeでの安全性保証の肝になっているようだ。

### 外部リソースアクセスの安全性

Linear Typeを使ったファイルアクセスについても見ておく。  

```haskell
firstLine :: FilePath -> IO_L ByteString
firstLine fp = do
  f <- openFile fp
  (f, Unrestricted bs) <- readLine f
  closeFile f
  return bs

type File
openFile :: FilePath -> IO_L FileHandler
readLine :: FileHandler %1 -> IO_L (FileHandler, Unrestricted ByteString)
closeFile :: FileHandler %1 -> IO_L ()
```

`readLine` は `FileHandler` を消費し、返り値として新しい `FileHandler` を返す。一方 `closeFile` は `FileHandler` を消費し、新しい `FileHandler` は返さない。  
通常のIOと異なり、 `IO_L` では返り値をLinear Typeで指定できる。これにより返り値の `FileHandler` をただ1度だけ消費する、という制限をかける。  
上記により、close忘れやclose後の呼出を防ぐことができる。

## 後方互換性

論文によると、Linear Typeの設計として、Linear Typeについての型注釈がない場合はmultiplicity(linearかどうか)をmany(linearじゃない状態)にする、という設計にしたそうだ。これにより言語拡張を使ってもLinear Typeを明示的に指定しないところは通常のHaskellとして書ける。後方互換性を大事にしている旨が何度か言及されていた。

## Rustとの比較

Linear Typeの挙動や目的について、Rustのmoveやborrowに似てるな、と思ったが、Rustのborrowについても論文内で言及があった。  
borrowシステムはその性質上、あるvが関数fによってborrowされていたら、fが処理を終えるまでvを保持しないといけない。そのため、tail-call eliminationができない。関数型言語ではtail-call eliminationは必須のため導入できない、という言及だった。  
tail-call elimination(いわゆる末尾再帰最適化)ができないという制約があるとは知らなかったため、あとで調べてみようかと思う。

## パフォーマンス

詳しくは読めていないが、Linear TypeによってUnboxed valueを使った最適化なども安全に行うことができ、性能向上にもつながった、というようなことが書いてあるようだ。

## Future work

future workとしては以下のような内容があげられていた。

- プログラム最適化
  - 最適なインライン化を行うためにカーディナリティの宣言にこのmultiplicityの型注釈を使うのはどうか、という話
- multiplicitiesを拡張
  - 今回は引数をちょうど1度呼びだすかそれ以外か、という話だったが、もっと拡張はできる、という話
- Streaming I/OやProgramming foreign heapsなどの実際の例への適用
  - 引数が評価される回数の制約がつけられるなら、引数が使いおわった時点で開放すればいいので、GCがいらなくなるためメモリ管理も容易になる

## 感想

引数の評価される回数を型で表現する、というのがなかなか興味深いなと思って調べはじめた。  
調べてみると、Rustのmoveやborrowのようにリソースが使われている状態、もう使えなくなった状態をうまく扱うために設計されたものということが理解できた。  
また、 [こちら](http://syocy.hatenablog.com/entry/try-linear-types) に言及されている通り、HaskellからWebAssemblyへのコンパイラを開発しているところでもあるので、WASMでGCどうするのか問題を解決する案としても意義があるのかなと思う。

## 参考
- [ghc-proposals/0111-linear-types.rst](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst#motivation)
- [Linear Haskell: practical linearity in a higher-order polymorphic language](https://arxiv.org/abs/1710.09756)
- [linear types · Wiki · Glasgow Haskell Compiler / GHC](https://gitlab.haskell.org/ghc/ghc/-/wikis/linear-types)
- [GHCの線形型プロトタイプを試すだけ - syocy’s diary](http://syocy.hatenablog.com/entry/try-linear-types)
- [haskell/linear-type.md at master · lotz84/haskell](https://github.com/lotz84/haskell/blob/master/docs/linear-type.md)