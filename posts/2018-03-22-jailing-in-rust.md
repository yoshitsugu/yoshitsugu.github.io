---
title: chrootベースのjailをRustで作ってみる
tags: Rust, jailing
---

[jailing](https://github.com/kazuho/jailing) をRustに置き換えながらjail構築の処理を追ってみた。<!--more-->

## 目的
[自作Linuxコンテナの時代](http://blog.yuuk.io/entry/diy-container)というy_uuk1さんが書いたブログ記事を最近読み直すことがあった。その中で触れられていたkazuhoさんの[jailing](https://github.com/kazuho/jailing)のコードを眺めていたところ、これをRustで書いたらRustの勉強+jailingの勉強にもなってよいのではないか、と思った。

## やってみる

### 処理の概要
perlはなじみがないが、まず雰囲気をつかむ。

1. /etc や /tmp などのディレクトリを準備する
2. /bin や /usr/bin などをbindでmountする
3. /dev を準備する
4. chrootする
5. capabilityを制限する
6. コマンドの実行

という流れになっている。

### 1. /etc や /tmp などのディレクトリを準備する
まずはこここを実装する。
空ディレクトリの生成、権限の変更(777)、ファイルのコピーをする

```rust
const NEW_DIRS: &'static [&'static str] = &["etc", "run", "usr", "var/log"];
const TMP_DIRS: &'static [&'static str] = &["tmp", "run/lock", "var/tmp"];
const COPY_FILES: &'static [&'static str] =
    &["etc/group", "etc/passwd", "etc/resolv.conf", "etc/hosts"];

fn create_dirs(root: &str) {
    let mut dirs = NEW_DIRS.to_vec();
    dirs.extend(TMP_DIRS.iter());
    for d in dirs {
        fs::create_dir_all(format!("{}/{}", root, d)).expect(&format!("Failed to create dir {}", d));
    }
}

fn change_dir_permissions(root: &str) {
    for d in TMP_DIRS.iter() {
        let metadata = fs::metadata(format!("{}/{}", root, d)).expect(&format!("Failed to get metadata {}", d));
        let mut permissions = metadata.permissions();
        permissions.set_mode(0o777);
    }
}

fn copy_files(root: &str) {
    for f in COPY_FILES {
        fs::copy(format!("/{}", f), format!("{}/{}", root, f)).expect(&format!("Failed to copy file {}", f));
    }
}

```

### 2. /bin や /usr/bin などをbindでmountする
必要なコマンドやライブラリがはいっているディレクトリをbindでmountする。  
mountはコマンドをそのまま実行してもよいが、今回は[libmount](https://github.com/tailhook/libmount)というcrateを使ってみた。


```rust
const BIND_DIRS: &'static [&'static str] = &[
    "bin",
    "etc/alternatives",
    "etc/pki/tls/certs",
    "etc/pki/ca-trust",
    "etc/ssl/certs",
    "lib",
    "lib64",
    "sbin",
    "usr/bin",
    "usr/include",
    "usr/lib",
    "usr/lib64",
    "usr/libexec",
    "usr/sbin",
    "usr/share",
];

fn bind_mount(root: &str) {
    for d in BIND_DIRS {
        let sd = format!("/{}", d);
        let rd = format!("{}/{}", root, d);

        if !Path::new(&sd).exists() {
            continue;
        }
        let sdm = fs::metadata(&sd).expect(&format!("Failed to get metadata {}", &sd));
        if sdm.file_type().is_symlink() {
            let rdm = fs::metadata(&rd).expect(&format!("Failed to get metadata {}", &rd));
            if !rdm.file_type().is_symlink() {
                fs::create_dir_all(
                    Path::new(&rd)
                        .parent()
                        .expect(&format!("Failed to get parent dir path {}", &rd)),
                ).expect(&format!("Failed to create parent dir {}", &rd));
            }
            let sdl = fs::read_link(&sd).expect(&format!("Failed to read symlink {}", &sd));
            std::os::unix::fs::symlink(sdl, &rd)
                .expect(&format!("Failed to create symlink {}", &rd));
        } else {
            fs::create_dir_all(&rd).expect(&format!("Failed to create dir {}", &rd));
            if fs::read_dir(&rd).unwrap().count() == 0 {
                libmount::BindMount::new(Path::new(&sd), Path::new(&rd))
                    .recursive(false)
                    .readonly(true)
                    .mount()
                    .expect(&format!("Failed to mount {} to {}", &sd, &rd));
            }
        }
    }
}
```

### 3. /dev を準備する
`/dev/null`, `/dev/zero`, `/dev/random`, `/dev/urandom` を準備する。  
元々のjailingでは`mknod` というコマンドで準備している。今回は `libc`の[mknod](https://doc.rust-lang.org/libc/x86_64-unknown-linux-gnu/libc/fn.mknod.html)を使った。  
このmknodなどのlibcのライブラリの使い方は[coreutils](https://github.com/uutils/coreutils)の実装が非常に参考になった。(というか一部そのまま使っている。)
```rust
fn make_devices(root: &str) {
    fs::create_dir_all(format!("{}/dev", root)).expect("Cannot create /dev dir");
    make_device_if_not_exists(format!("{}/dev/null", root), 0o666, makedev(1, 3));
    make_device_if_not_exists(format!("{}/dev/zero", root), 0o666, makedev(1, 5));
    for r in &["random", "urandom"] {
        make_device_if_not_exists(format!("{}/dev/{}", root, r), 0o444, makedev(1, 9));
    }
}

fn make_device_if_not_exists(path: String, mode: mode_t, dev: dev_t) {
    if !Path::new(&path).exists() {
        let err = makenod(&path, S_IFCHR | mode, dev);
        handle_os_error(err, format!("{}", path));
    }
}

fn makenod(path: &String, mode: mode_t, dev: dev_t) -> i32 {
    unsafe {
        mknod(
            CString::new(path.as_bytes())
                .expect("Error in construct CString")
                .as_bytes_with_nul()
                .as_ptr() as *const libc::c_char,
            mode,
            dev,
        )
    }
}

fn makedev(maj: u64, min: u64) -> dev_t {
    // pick up from <sys/sysmacros.h>
    ((min & 0xff) | ((maj & 0xfff) << 8) | (((min & !0xff)) << 12) | (((maj & !0xfff)) << 32))
        as dev_t
}

fn handle_os_error<T: std::fmt::Display>(err: i32, action: T) {
    if err != 0 {
        panic!(
            "Error: {{action: {}, code: {}, msg: {} }}",
            action,
            err,
            Error::last_os_error()
        );
    }
}
```

### 4. chrootする
chrootもコマンドを呼び出してもいいのだが、折角なので、`libc`の[chroot](https://doc.rust-lang.org/libc/x86_64-unknown-linux-gnu/libc/fn.chroot.html)を使った。  


```rust
fn exec_chroot(root: &str) {
    std::env::set_current_dir(&root).expect(&format!("Cannot change current dir to {}", &root));
    let err = unsafe {
        chroot(CString::new(".".as_bytes())
            .expect("Error in construct CString")
            .as_bytes_with_nul()
            .as_ptr() as *const libc::c_char)
    };
    handle_os_error(err, "chroot");
}
```

### 5. capabilityを制限する
capabilityについてあまり理解できていなかったため、manや[Linux Capability - ケーパビリティについての整理](http://udzura.hatenablog.jp/entry/2016/06/24/181852)を参考にした。  
また、capabilityの操作は[caps-rs](https://github.com/lucab/caps-rs)を使用した。(結構有用そうなのにスター数が少なくて悲しい。)
```rust
fn drop_capabilities() {
    let allowed_caps = vec![
        Capability::CAP_SETGID,
        Capability::CAP_SETGID,
        Capability::CAP_NET_BIND_SERVICE,
    ];
    let cur = caps::read(None, CapSet::Bounding).expect("Cannot read capabilities");
    for c in cur {
        if allowed_caps.contains(&c) {
            continue;
        }
        caps::drop(None, CapSet::Bounding, c).expect(&format!("Cannot drop capability {}", c));
    }
}
```

### 6. コマンドの実行
最後に、chroot環境で、コマンドを実行する。 
```rust
fn exec_command<'a>(commands: Option<clap::Values>) {
    let cmds = match commands {
        Some(vs) => vs.map(|v| v).collect::<Vec<&str>>(),
        None => vec!["bash", "-i"],
    };
    let _ = Command::new(cmds[0])
        .args(&cmds[1..])
        .status()
        .unwrap_or_else(|e| panic!("Cannot exec: {}", e));
}
```

## コマンド化
上記をベースに、`jl`という名前でコマンドを作った。  
コードは[https://github.com/yoshitsugu/jl](https://github.com/yoshitsugu/jl)に置いてある。

## 動かす
`/opt/jail`というディレクトリを作ってそこで実験してみる
```bash
$ sudo mkdir /opt/jail
```

`jl`経由で`bash`起動
```bash
$ sudo jl --root bash

bash-4.4#

```

`ls`とか`pwd`をためしてみる
```bash
bash-4.4# ls
bin  dev  etc  lib  lib64  run  sbin  tmp  usr  var
bash-4.4# pwd
/
```

capability上許可されていないはずの`kill`や`chown`をためしてみる
```bash
bash-4.4# kill 26559
bash: kill: (26559) - 許可されていない操作です
bash-4.4# touch /var/log/hoge
bash-4.4# chown 1000 /var/log/hoge
chown: 'var/log/hoge' の所有者を変更中: 許可されていない操作です

```

ちなみに、`jl`コマンドにそのままコマンドを渡すこともできる
```bash
$ sudo jl --root /opt/jail ls
bin  dev  etc  lib  lib64  run  sbin  tmp  usr  var
$ sudo jl --root /opt/jail -- ruby -e "puts 'hello'"
hello
```

## まとめ
- [jailing](https://github.com/kazuho/jailing)のコードを読みながらRustでchrootベースのjail構築ができるようにした。
    - [https://github.com/yoshitsugu/jl](https://github.com/yoshitsugu/jl)
- custom bindingなど元のjailingにあるもので実装していないものもあるので、その辺はまた今度。
- Linux capabilityやRustでlibcを使う方法など勉強になった。
