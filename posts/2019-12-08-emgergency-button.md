---
title: AWS IoT ButtonとRaspberry Piで子どもの緊急連絡ができるようにする
tags: AWS IoT Button, Raspberry Pi, Rust, IoT
---

幼児の娘が緊急事態を伝えられるようにした。
<!--more-->

## 背景

私には幼児の娘がおり、妻の仕事の関係上、2人きりで生活することが多い。  
もし2人きりのときに自分が倒れた場合、携帯電話などを持たない娘は連絡の手段がない。そこで、AWS IoT Buttonを押すだけで、家庭用Slackに通知することで、少なくとも妻には連絡がいくようにした。  
尚、近所の人に助けを求める、など、他の解決策も娘とは話をしており、今回の対策はミッションクリティカルなものではなく、少しでも娘の生存確率を上げる保険的なものとなる。

## 要件

- AWS IoT Buttonを押すと、家庭用Slackに緊急事態を伝えるメッセージが投稿されること。
- いたずらで押すなど、誤通知があった場合にすぐフォローできるようにするため、Slack通知とともに音を出して気づけるようにすること。
    - Slack通知だけであればAWS IoT ButtonとAWS Lambdaを組み合わせればできるが、こちらの音を出す要件によりRaspberry Piも使うことにした。

## 環境

- AWS IoT Buttonは [こちら](https://www.amazon.co.jp/dp/B075FPHHGG) から購入した
- Raspberry Piは昔購入したもの。 `/proc/cpuinfo` をみると `Raspberry Pi Model B Rev 2` となっていた

## 手順

今回は、全体像として以下のような流れで処理することにした。

1. AWS IoT Buttonを押す
2. IoT ButtonにひもづけられているAWS Lambdaの関数が実行される
3. Lambda関数はRaspberry PiがsubscrubeしているMQTTのトピックをpublishする
4. Raspberry PiがMQTTのpublishを検知し、Slackへの投稿、および音を鳴らす処理を行う

### AWS IoT Buttonを接続する

[AWS IoT 1Click](https://aws.amazon.com/jp/iot-1-click/) を使えるようにする。セットアップ方法は調べればすぐでてくる[^1]ので、ここには詳しく書かない。

### Raspberry PiをAWS IoT Coreで接続する

[AWS IoT Core](https://aws.amazon.com/jp/iot-core/) を使って、Raspberry Piを管理する。セットアップ方法はこちらも調べればすぐでてくる[^2]ので、ここには詳しく書かない。  
ポリシーの設定の理解に少し時間がかかった。

### AWS IoT Buttonで連動するAWS Lambda関数を作る

Raspberry PiがAWS IoT Coreで接続できるようになったので、MQTTのトピックをpublishするような関数を作る。今回は趣味でRustで作った。

```rust
use lambda_runtime::{error::HandlerError, lambda, Context};
use serde_json::Value;
use rusoto_core::region::Region::ApNortheast1;
use rusoto_iot_data::{IotDataClient, PublishRequest};
use rusoto_iot_data::IotData;
use bytes::Bytes;

fn main() {
    lambda!(handler)
}

fn handler(
    event: Value,
    _: Context,
) -> Result<Value, HandlerError> {
    let client = IotDataClient::new(ApNortheast1);
    let payload = PublishRequest {
        payload: Some(Bytes::from(&b"{ \"message\": \"panic button is pushed\" }"[..])),
        qos: None,
        topic: "panic_button".to_string()
    };
    client.publish(payload).sync().unwrap();
    Ok(event)
}
```

これをserverless frameworkのrustプラグインを使ってデプロイできるようにした。

### Raspberry PiでMQTTのサブスクライブをする

上記の通知をRaspberry Piで受けとり、Slack通知と音の再生をする。こちらも趣味でRustのコードで動かした。Slack通知には[slack-hook](https://github.com/frostly/rust-slack)、音の再生は`aplay`コマンドをそのまま実行する形にした。また、MQTTのサブスクライブは[rumqtt](https://github.com/AtherEnergy/rumqtt)を利用した。  

```rust
use rumqtt::{client::Notification::Publish, MqttClient, MqttOptions, QoS};
use slack_hook::SlackTextContent::{Text, User};
use slack_hook::{PayloadBuilder, Slack, SlackUserLink};
use std::env;
use std::process::Command;

fn main() {
    // MQTTまわりの設定
    let client_id = env::var("CLIENT_ID").expect("CLIENT_ID is invalid");
    let ca = include_bytes!("../tlsfiles/ca.crt").to_vec();
    let client_cert = include_bytes!("../tlsfiles/cert.pem").to_vec();
    let client_key = include_bytes!("../tlsfiles/private.key").to_vec();

    let mqtt_host = env::var("MQTT_HOST").expect("MQTT_HOST is invalid");

    // SlackのWebhook URL
    let slack_hook_url = env::var("SLACK_HOOK_URL").expect("SLACK_HOOK_URL is invalid");

    // 音楽ファイルの場所
    let music_file = env::var("MUSIC_FILE_PATH").expect("MUSIC_FILE_PATH is invalid");

    let mqtt_options = MqttOptions::new(client_id, mqtt_host, 8883)
        .set_ca(ca)
        .set_client_auth(client_cert, client_key)
        .set_keep_alive(10);

    let (mut mqtt_client, notifications) = MqttClient::start(mqtt_options).unwrap();
    mqtt_client
        .subscribe("panic_button", QoS::AtLeastOnce)
        .unwrap();

    for notification in notifications {
        match notification {
            Publish(_) => {
                // publishをうけたとった場合だけ以下を実行
                let slack = Slack::new(&*slack_hook_url).unwrap();
                let p = PayloadBuilder::new()
                        .text(vec![
                            User(SlackUserLink::new("!everyone")),
                            Text(":rotating_light: *緊急ボタンが押されました！* :rotating_light:\nすぐに連絡をとってください".into())
                        ].as_slice())
                        .channel("#general")
                        .username("自宅")
                        .icon_emoji(":house:")
                        .build()
                        .unwrap();
                slack.send(&p).expect("Cannot send message to slack");
                // 音量の調整
                Command::new("amixer")
                    .arg("cset")
                    .arg("numid=1")
                    .arg("90%")
                    .output()
                    .expect("Cannot change volume");
                for _ in 0..5 {
                    // aplayで音楽を再生
                    Command::new("aplay")
                        .arg(&*music_file)
                        .output()
                        .expect("Cannot play music");
                }
            }
            _ => println!("{:?}", notification),
        }
    }
}
```

Raspberry Pi向けにコンパイルするのに [cross](https://github.com/rust-embedded/cross) を使った。

コードの全体は [https://github.com/yoshitsugu/panic_button](https://github.com/yoshitsugu/panic_button) にて公開している。

## 実行結果

AWS IoT Buttonを押すことで、Sackに投稿され、同時に音が鳴る様子を撮影した。

<iframe width="600" height="337" src="https://www.youtube.com/embed/oHe0b-iSPB4" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

## 感想

AWS IoT ButtonとRaspberry Piを使うことで比較的簡単に実現できた。
AWS IoT Coreは他にもいろいろと機能がありそうで、そのうち触ってみたい。  
また同じような仕組みで、Slack通知とともにカメラで室内写真を撮っておくったり、できることはまだありそうだ。娘に実際に使ってもらいつつ、改善をすすめていきたい。

[^1]: [【国内販売開始】AWS IoT Enterprise Button試してみたらホンマに簡単にLambda関数を実行できた ｜ Developers.IO](https://dev.classmethod.jp/cloud/aws/aws-iot-enterprise-button/) など
[^2]: [Raspberry PiでAWS IoT Coreと接続し、GPIO制御をしてみた - Qiita](https://qiita.com/gnk263/items/a7937259746c81a6b052) など

