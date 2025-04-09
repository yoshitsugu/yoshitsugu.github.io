---
title: Rustのメモリオーダリングについて
tags: Rust, Memory Ordering
---

最近業務でRustを使っており、再入門のため、[Rust for Rustaceans](https://rust-for-rustaceans.com/) を読んでいる。メモリオーダリングまわりについて自分用にまとめておく。

<!--more-->

## メモリオーダリングとは何か

実行速度の最適化のために、CPUでの実行時にはコードに書かれた順番で実行されないことがある。
例として、Rust for Rustaceansに載っているものとして以下[^1]がある。

```rust
static X: AtomicBool = AtomicBool::new(false);
static Y: AtomicBool = AtomicBool::new(false);

let t1 = spawn(|| {
  let r1 = Y.load(Ordering::Relaxed);
  X.store(r1, Ordering::Relaxed);
});

let t2 = spawn(|| {
  let r2 = X.load(Ordering::Relaxed); // 1
  Y.store(true, Ordering::Relaxed);   // 2
});
```

ここでの `r2` はtrueになることがなさそうに見えるが、CPUの最適化により、1より2のほうが先に実行されることがあり、その結果、`r2` がtrueになり得る、とのことだ。<br>
このような人間の直感とは違う実行順になると、シングルスレッドの素直なプログラミングなら問題ないものの、マルチスレッドで値を共有するような場合は問題になる(ことがある)。そのため、ある場面ではCPUが実行する順序を制御したくなる。これがメモリオーダリングの役割だ。上記コード中の `Ordering::Relaxed` もメモリオーダリングの一種であり、「何も制御しない」メモリオーダリングをあらわす。つまり、CPUが実行に最適と判断したら、コード上の見掛けの実行順を無視してよい、ということだ。

## Rustにおけるメモリオーダリング

Rustにおいて、メモリオーダリングは以下のenumとして定義されている。[^2]

```rust
#[non_exhaustive]
pub enum Ordering {
    Relaxed,
    Release,
    Acquire,
    AcqRel,
    SeqCst,
}
```

`Relaxed` は上記の通り、何も制御しない。

## Relase, Acquire, AcqRel

- `Ordering::Release`
   - storeと共に使われ、これより後に書かれたloadやstoreがこれより先に移動されることはないようになる
- `Ordering::Acquire`
    - loadと共に使われ、これより先に書かれたloadやstoreがこれより後に移動されることはないようになる
- `Ordering::AcqReq`
    - `Ordering::Release`と `Ordering::Acquire`の効果を同時に得る

つまり、先ほどの例を


```rust
static X: AtomicBool = AtomicBool::new(false);
static Y: AtomicBool = AtomicBool::new(false);

let t1 = spawn(|| {
  let r1 = Y.load(Ordering::Acquire);
  X.store(r1, Ordering::Release);
});

let t2 = spawn(|| {
  let r2 = X.load(Ordering::Acquire);
  Y.store(true, Ordering::Release);
});
```

のように書き換えれば、前述の問題は起きないことになる。

## SeqCst

これで安心かと思いきや、まだ問題がある。あまり本にある内容を繰り返してもしょうがないので、詳しくはRust for Rustaceansを読んでもらえればと思うが、おおまかには以下の通りである。

- 4つのスレッドが同時に動くとする
- 最初の2つで書き込み(スレッドA, Bとする)、後の2つで読み込み(スレッドC, Dとする)をする
- CではAのあとBが実行されたように見え、DではBのあとAが実行されたように見える、という状況がありえる

というものだ。 `Ordering::SeqCst` (Sequentially Consistent Ordering)はこれを常に一貫した順序で見えるように制御する。

## Rust以外でのメモリオーダリング

ところで、Rust以外でも同等の仕組みはある。上述の `enum Ordering` のドキュメントにも

> Rust’s memory orderings are the same as those of C++20.

との記載がある通り、C++でも同じようなものがあるようだ。<br>
また、[低レベルプログラミング](https://www.amazon.co.jp/dp/4798155039) という書籍によると、これらを「メモリバリア」と呼び、GCCでの実装例を紹介している。書籍内での説明[^3]では、

> **acquire操作**<br>
> これは、acquireセマンティクスと呼ばれるプロパティを持つ処理のことだ。ある処理が共有メモリからの読み込みを実行するとき、ソースコードでそれに続くリードとライトとの順序が変更されない保証があれば、その処理は、このプロパティを持つ。
> 言い換えると、この操作に続くコードが、これより前に実行されるように並び替えられることはない、という点で、全般的なメモリバリアと似ている

とあり、`Ordering::Acquire` と同じような説明がなされていることがわかる。同様にrelease操作もある。

## atomic typesが提供する他のメソッド

Rustの話にもどす。<br>
Rustのatomic typesが提供するメソッドは他にもある。ここでは詳しくは触れないが、

- `compare_exchange` : 条件つきで値を入れ替える操作をatomicにできる
- `fetch_add` : 数値のインクリメントをatomicにできる

などがある。並行、並列処理で共有する値を扱うときはこれらのメソッドが必要ないかも検討するとよさそうだ。

## いつこれらのメモリオーダリングやメソッドを使うか

速度向上のメリットをとって実行順序が直感的にならないリスクをとるか、意図しない値が書き込まれたり読み込まれたりするリスクを減らし、その分実行速度が落ちることを許容するか、という点はトレードオフがある。
Rust for Rustaceansにはどのように考えるべきかも書かれている。具体的には、以下の3つの点があげられている。

1. シンプルにスタートする(Start Simple)

2. ストレステストを書く(Write Stress Test)
3. 並行テストのためのツールを使う(Use Concurrency Testing Tools)

1について補足すると、 `Ordering::SeqCst` などの「シンプルな」(おそらくここではコードを読んで理解する素朴な実行順序に近いものがシンプルと表現されている)ものをまず使うようにする。その後計測してボトルネックとなるところを最適化していく。という流れが推奨されている。

自分の業務でここまでのことを考えることがあるかどうかは不明だが、このあたりは頭に起きつつ、実装をすすめていきたい。

[^1]: Rust for Rustaceans, Chapter 10, Listing 10-2
[^2]: [https://doc.rust-lang.org/std/sync/atomic/enum.Ordering.html](https://doc.rust-lang.org/std/sync/atomic/enum.Ordering.html)
[^3]: 低レベルプログラミング, 17章マルチスレッド, 17.7 メモリバリア
