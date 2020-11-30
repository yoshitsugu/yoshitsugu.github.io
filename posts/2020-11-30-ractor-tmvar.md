---
title: Ractor::TMVarを作った
tags: Ractor, Ractor::TMVar, Ractor::TVar, STM
---

[Ractor::TVar](https://github.com/ko1/ractor-tvar) に対応する [TMVar](https://hackage.haskell.org/package/stm-2.5.0.0/docs/Control-Concurrent-STM-TMVar.html) がほしくなったので作った

<!--more-->

## TMVar

先日、[Ractor で「食事する哲学者の問題」を解く](/posts/2020-11-21-ractor-dining-philosophers-problem.html) という記事を書いた。その後、やはり TMVar に相当するものがほしい、という気持ちになった。  
元々参考にしていた、[STM で解く「食事する哲学者の問題」 - あどけない話](https://kazu-yamamoto.hatenablog.jp/entry/20120704/1341378177) では TVar をそのまま使うのではなく、TMVar のほうを使っており、それに倣った形となる。  
現状、ractor-tvar には `retry` に相当するものとしては、 `raise Ractor::RetryTransaction` するしかなく、例外処理で通常の分岐処理を行うのはあまり気持ちよくない、と感じたことも一因である。

## takeTMVar, putTMVar

今回はとりあえず `takeTMVar` および `putTMVar` に相当する機能があれば十分と考えた。  
[これらのソース](https://hackage.haskell.org/package/stm-2.5.0.0/docs/src/Control.Concurrent.STM.TMVar.html)を参考にしながら実装をすすめる。

### takeTMVar

実装は以下の通り。

```haskell
takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> do writeTVar t Nothing; return a
```

TVar の値を読み込み、値が `Nothing` だったら `retry`, `Just a` だったら TVar を空にしてから a を返している。

### putTMVar

実装は以下の通り。

```haskell
putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do writeTVar t (Just a); return ()
    Just _  -> retry
```

今度は TVar の値が `Nothing` だったら値の書き込み、`Just _` だったら `retry` となっている。

## 実装

Ruby での実装は以下のようにした

```ruby
# frozen_string_literal: true

require "ractor/tvar"

class Ractor
  class TMVar

    # Ractor::TMVarの値が空であることを示すマーク代わり
    BLANK = :RACTOR_TMVAR_BLANK

    def initialize(tvar = nil)
      @tvar = Ractor::TVar.new(tvar)
    end

    # takeTMVarに相当する
    def value
      v = @tvar.value
      raise Ractor::RetryTransaction if v == BLANK

      # Ractor::TVarはatomicallyの中でしか代入できないので、atomicallyで包んでいる
      Ractor.atomically do
        @tvar.value = BLANK
      end
      v
    end

    # putTMVarに相当する
    def value=(new_value)
      raise Ractor::RetryTransaction if @tvar.value != BLANK

      @tvar.value = new_value
    end
  end
end
```

`takeTMVar` を `value` 、 `putTMVar` を `value=` として、それぞれ同等の仕組みを実装した。  
Haskell の TMVar では Maybe 型を使っているが、Ruby では使えない。そこで `:RACTOR_TMVAR_BLANK` というシンボルを用意して、それを `Nothing` 相当として動くようにした。 `nil` を `Nothing` 代わりにすることも考えたが、Ruby では `nil` を値として取得したいパターンもあるかもしれないという仮定の下、特別なシンボルを用意した。  
こちらの実装を gem 化したものが [ractor-tmvar](https://rubygems.org/gems/ractor-tmvar) となる。

## 「食事する賢者の問題」を Ractor::TMVar で解く

Ractor::TVar で解いた「食事する賢者の問題」を上記で実装した Ractor::TMVar で解くことにする。

```ruby
# frozen_string_literal: true

require 'securerandom'
require 'ractor/tmvar'

NUM_OF_PHILOSOPHERS = 5

class Philosopher
  def initialize(name, left, right)
    @name = name.freeze
    @left = left
    @right = right
  end

  def eat
    puts "#{@name} eating..."
    sleep SecureRandom.random_number * 5
  end

  def think
    puts "#{@name} thinking..."
    sleep SecureRandom.random_number * 5
  end

  def take_forks
    Ractor.atomically do
      @left.value
      @right.value
    end
  end

  def put_forks
    Ractor.atomically do
      @right.value = nil
      @left.value = nil
    end
  end

  def start
    loop do
      take_forks
      eat
      put_forks
      think
    end
  end
end

forks = NUM_OF_PHILOSOPHERS.times.map do
  Ractor::TMVar.new
end

rs = NUM_OF_PHILOSOPHERS.times.map do |i|
  Ractor.new("philosopher #{i + 1}", forks[i], forks[(i + 1) % NUM_OF_PHILOSOPHERS]) do |n, l, r|
    Philosopher.new(n, l, r).start
  end
end
Ractor.select(*rs)
```

Ractor::TVar で解くため、ループでのロック取得や、raise Ractor::RetryTransaction を使った retry で行っていた制御を Ractor::TMVar に隠蔽でき、スッキリしたコードになったと思う。

## まとめ

Haskell の TMVar を参考にした、Ractor::TMVar を作った。  
ソースコードは [yoshitsugu/ractor-tmvar](https://github.com/yoshitsugu/ractor-tmvar) にある。  
また、[「食事する賢者の問題」を解いたリポジトリ](https://github.com/yoshitsugu/dining_philosophers_problem_ractor)も更新した。
