---
title: パーフェクトRuby第2版を読んだ
tags: Ruby, Book Review
---
[パーフェクトRuby第2版](http://gihyo.jp/book/2017/978-4-7741-8977-2)を読んだので、軽く感想を書いておく<!--more-->  

## 総評
さすがに結構知っていることが多かったものの、細かいところは勉強になった。

## 章ごとの感想

### Part1 Ruby overview, Part2 Ruby言語仕様
だいたい知っている内容だった。
細かいところだが、caseのwhenにProcを使えるのは知らなかった。  
FizzBuzzも
```ruby
def fizz_buzz(n)
  case n
  when -> (n) { n % 15 == 0 }
    'FizzBuzz'
  when -> (n) { n % 3 == 0 }
    'Fizz'
  when -> (n) { n % 5 == 0 }
    'Buzz'
  else
    n
  end
end
```
のように書ける。こんな風に書くことはないだろうが。
  
Thread, Fiberあたりの並行処理の話は今まであまり必要になったことがないので、勉強になった。  
次の版では話題のGuildの話もはいってくるのだろうか。

### Part3 メタプログラミング
恥ずかしながら(なのかどうかはよくわからないが)[メタプログラミングRuby](https://www.oreilly.co.jp/books/9784873117430/)は読んだことがなく、メタプログラミングは普段あまりやらないので、結構知らないこと、もしくは今まであまり理解できていなかったことが書いてあった。ClassやModuleの構造を理解する上では非常に勉強になった。  
そういえば、以下Animalモジュールのような動的なメソッド生成を行っているmoduleを見たことがある。DogではAnimalをextendして、`define_animal_methods` を呼べばメソッドが展開されるという寸法である。

```ruby
module Animal
  def define_animal_methods
    define_method :name do |&block|
      puts 'animal_name'
      block.call if block
    end

    # define_methodが続く...
  end
end

class Dog
  extend Animal
  define_animal_methods
end

Dog.new.name
#=> animal_name
```

ここで、展開先のClassにおいて、Animalの `name` をoverrideするようなイメージで、 `block.call` しているところにblockを渡して新しい `name` メソッドにしたい。どうしたらいいだろうか。  
自分は結果的には以下のように `instance_method` で一旦退避させた上で、 `define_method` で定義するような荒技を使った。

```ruby
class Cat
  extend Animal
  define_animal_methods

  _name = instance_method(:name)

  define_method :name do
    _name.bind(self).() do
      puts 'cat_name'
    end
  end
end

Cat.new.name
# => animal_name
#    cat_name
```

もっと簡単な方法があるような気もするが、どうだろうか。  
また、関係ないが、 `method_missing` を使った[こういうアプローチ](http://qiita.com/pink_bangbi/items/274c8227a92826a269cb)は面白いと思う。自分でも何か考えてみたい。

### Part4 標準添付ライブラリ
こうしてみるとRubyはやはり標準の機能がかなり豊富だなと思う。  
fileutilsなどのあまり使わないライブラリまわりは、覚えられないので必要になったら都度調べている。今回復習できたものの、また忘れたころに必要になるだろうなと思う。Setもごく稀に使うが、Arrayで事足りることが多いイメージがある。

### Part5 実践プログラミング
gemを一度は作ったことがあり、なんとなく流れは知っているので、流し読み程度。  
Ruby版Power Assertについても触れられているが、どのくらいユーザー数いるんだろうか。

<br />

## 最近読んだ他の本
最後に、パーフェクトRuby以外の本で最近読んで気になったものをあげておく。  

- [実践AWS Lambda](http://amzn.to/2swsaZF)
    - AWS Lambdaのチュートリアル的な本。
    - SAMや周辺サービスにも言及している。
    - 総ページ数に対してほぼスクリーンショットのページが多くて少し残念だった。
- [炎立つ](http://amzn.to/2sWR81T)
    - 技術書ではない。奥州藤原氏の話で、岩手に引っ越したので読んでいる。
    - まだ3巻の途中。
    - 影響されてしまったのか、「感心しました」と言いたい場面で「感服仕った」が先に出てきて困った。
