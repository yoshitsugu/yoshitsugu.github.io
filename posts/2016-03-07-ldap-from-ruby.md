---
title: RubyからLDAPをつかう
tags: Ruby,LDAP
---
所用でRubyからLDAPを使う必要があったので調べた。
<!--more-->

## そもそもLDAPとは
RDBのテーブルのようなものを想定しておけばいいようだ。  
ただし、各レコードが構造をもち、各レコードに階層構造のインデックスがついている。  
yoshitsugu.hogehoge.ne.jp をひっぱってくれば jp -> ne -> hogehoge -> yoshitsugu とインデックスをたどっていき、たどった先にある電話番号とかメールアドレスとかがずるずるっとひけるイメージ。  
詳しくはRFC4510などを参考にするとよいと思う。

## LDAP用のgem
* <a href="http://ruby-ldap.sourceforge.net/" target="_blank">ruby-ldap</a>  
  メンテされてないのか安定しているのかわからないが、2009/4/21からupdateされていない。  
  一部C実装なので下のnet-ldapよりはやいらしい。

* <a href="https://github.com/ruby-ldap/ruby-net-ldap" target="_blank">net-ldap</a>  
  Pure Rubyのライブラリ。  
  Pure Rubyなので、ruby-ldapより遅いが、ポータビリティが高い。
    
<a href="https://www.ruby-toolbox.com/search?q=ldap" target="_blank">Ruby ToolBox</a>
を見ると
<a href="https://github.com/cschiewek/devise_ldap_authenticatable" target="_blank">Devise LDAP Authenticalble</a>
など他にもいくつかあるようだ。

## Net::LDAP
上記のうち、net-ldapを使った。理由は一番メンテされてそうなことと、ポータビリティが高いから。  
使い方は簡単で例えば、以下のようにすればfilterした結果を取得できる。

```ruby
require 'net/ldap'

PORT   = 636 # ldapは389, ldapsは636
DOMAIN = 'hogehoge.ne.jp'
SERVER = "ldap.#{DOMAIN}"
BASE   = 'DC=hogehoge,DC=ne,DC=jp'

ldap = Net::LDAP.new { 
  host: SERVER,
  port: PORT,
  base: BASE,
  encryption: :simple_tls, # ldaps じゃなければいらない
  auth: {
    username: 'cn=ldapuser,dc=hogehoge,dc=ne,dc=jp',
    password: 'PASSWORD',
    method: :simple
  }
}

raise 'bind failed' unless ldap.bind

entries = {}

ldap.open { |conn|
  filter1 = Net::LDAP::Filter.eq('sAMAccountName', 'foo')
  conn.search(filter: filter1) do |entry|
    entry.each do |field, value|
      entries[field] = value
    end
  end
}

p entries 
```
お手軽。

## 参考
* <a href="https://thinkit.co.jp/free/tech/18/1/1.html" target="_blank">LDAPとは何をするもの？</a>
* <a href="http://www.slideshare.net/tasheeen/ruby-ldap-5107901" target="_blank">Ruby で扱う LDAP のススメ</a>
* <a href="http://d.hatena.ne.jp/dayflower/20100302/1267509137" target="_blank">Ruby から Net::LDAP で ActiveDirectory にアクセスする</a>

