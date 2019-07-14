---
title: 「30日でできる！OS自作入門」をRustで。19日目
tags: OS自作入門, OS, Rust
---

[「30日でできる！OS自作入門 」](https://book.mynavi.jp/supportsite/detail/4839919844.html)のC言語の部分をできるだけRustですすめてみる。今回は19日目の内容。
<!--more-->

## cat(type)コマンドを追加する

前回に引き続きコマンド追加となる。  
今回は`cat`に相当する機能ということで、例によって本では`type`になっているがここでは`cat`として実装する。
  
前回、コマンド文字列を抽出処理を関数として書いたが、Rustの`split`でも書けそうなことに気づいたので、`split`を使う方針に変更する。

```rust
// console.rs
fn exec_cmd(
    cmdline: [u8; 30],
    cursor_y: isize,
    sheet_manager: &mut SheetManager,
    sheet_index: usize,
    memtotal: usize,
) -> isize {
    let sheet = sheet_manager.sheets_data[sheet_index];
    let mut cursor_y = cursor_y;
    macro_rules! display_error {
        ($error: tt, $cursor_y: tt) => {
            write_with_bg!(
                sheet_manager,
                sheet_index,
                sheet.width,
                sheet.height,
                8,
                $cursor_y,
                Color::White,
                Color::Black,
                30,
                $error
            );
            cursor_y = newline($cursor_y, sheet_manager, sheet_index);
            return newline(cursor_y, sheet_manager, sheet_index);
        };
    }
    let cmdline_strs = cmdline.split(|s| *s == 0 || *s == b' ');
    let mut cmdline_strs = cmdline_strs.skip_while(|cmd| cmd.len() == 0);
    let cmd = cmdline_strs.next();
    if cmd.is_none() {
        display_error!("Bad Command", cursor_y);
    }
    let cmd = from_utf8(&cmd.unwrap()).unwrap();
    if cmd == "mem" {
    // 省略
```

あわせて、`display_error`という文字列を表示するだけのマクロを用意してエラメッセージ表示を完結に記述できるようにした。  
  
ファイルの中身を表示するにあたり、`FileInfo`のメソッドとしてファイルの中身の先頭番地を返せるようにしておく。

```rust
// file.rs
impl FileInfo {
    pub fn content_addr(&self) -> usize {
        self.clustno as usize * 512 + 0x003e00 + ADR_DISKIMG
    }
}
```

これを使って、表示ロジックを書いていく。

```rust
// console.rs
fn exec_cmd(
    // 省略
) -> isize {
    // 省略
        } else if cmd == "cat" {
        // ファイル名となるところを抽出
        let mut filename = cmdline_strs.skip_while(|strs| strs.len() == 0);
        let filename = filename.next();
        if filename.is_none() {
            display_error!("File Not Found", cursor_y);
        }
        let filename = filename.unwrap();
        // 拡張子の前後でわける
        let mut filename = filename.split(|c| *c == b'.');
        let basename = filename.next();
        let extname = filename.next();
        let mut b = [b' '; 8];
        let mut e = [b' '; 3];
        if let Some(basename) = basename {
            for fi in 0..b.len() {
                if basename.len() <= fi {
                    break;
                }
                if b'a' <= basename[fi] && basename[fi] <= b'z' {
                    // 小文字は大文字で正規化しておく
                    b[fi] = basename[fi] - 0x20;
                } else {
                    b[fi] = basename[fi];
                }
            }
        } else {
            display_error!("File Not Found", cursor_y);
        }
        if let Some(extname) = extname {
            for fi in 0..e.len() {
                if extname.len() <= fi {
                    break;
                }
                if b'a' <= extname[fi] && extname[fi] <= b'z' {
                    e[fi] = extname[fi] - 0x20;
                } else {
                    e[fi] = extname[fi];
                }
            }
        }
        let mut target_finfo: Option<FileInfo> = None;
        for findex in 0..MAX_FILE_INFO {
            let finfo = unsafe {
                *((ADR_DISKIMG + ADR_FILE_OFFSET + findex * core::mem::size_of::<FileInfo>())
                    as *const FileInfo)
            };
            if finfo.name[0] == 0x00 {
                break;
            }
            if finfo.name[0] != 0xe5 {
                if (finfo.ftype & 0x18) == 0 {
                    let mut filename_equal = true;
                    for y in 0..finfo.name.len() {
                        if finfo.name[y] != b[y] {
                            filename_equal = false;
                            break;
                        }
                    }
                    for y in 0..finfo.ext.len() {
                        if finfo.ext[y] != e[y] {
                            filename_equal = false;
                            break;
                        }
                    }
                    if filename_equal {
                        target_finfo = Some(finfo);
                        break;
                    }
                }
            }
        }
        if let Some(finfo) = target_finfo {
            let content_length = finfo.size;
            let mut cursor_x = 8;
            for x in 0..content_length {
                let chr = unsafe { *((finfo.content_addr() + x as usize) as *const u8) };
                if chr == 0x09 {
                    // タブ
                    loop {
                        write_with_bg!(
                            sheet_manager,
                            sheet_index,
                            sheet.width,
                            sheet.height,
                            cursor_x,
                            cursor_y,
                            Color::White,
                            Color::Black,
                            1,
                            " "
                        );
                        cursor_x += 8;
                        if cursor_x == MAX_CURSOR_X {
                            cursor_x = 8;
                            cursor_y = newline(cursor_y, sheet_manager, sheet_index);
                        }
                        if (cursor_x - 8) & 0x1f == 0 {
                            // 32で割り切れたらbreak
                            break;
                        }
                    }
                } else if chr == 0x0a {
                    // 改行
                    cursor_x = 8;
                    cursor_y = newline(cursor_y, sheet_manager, sheet_index);
                } else if chr == 0x0d {
                    // 復帰
                    // 何もしない
                } else {
                    write_with_bg!(
                        sheet_manager,
                        sheet_index,
                        sheet.width,
                        sheet.height,
                        cursor_x,
                        cursor_y,
                        Color::White,
                        Color::Black,
                        1,
                        "{}",
                        chr as char
                    );
                    cursor_x += 8;
                    if cursor_x == MAX_CURSOR_X {
                        cursor_x = 8;
                        cursor_y = newline(cursor_y, sheet_manager, sheet_index);
                    }
                }
            }
            cursor_y = newline(cursor_y, sheet_manager, sheet_index);
        } else {
            display_error!("File Not Found", cursor_y);
        }
    } else {
```

やたらと長くなってしまったが、やっていることは割と素直で、ファイルを探し、あれば中身を読み込んで表示、ということをしている。  
タブや改行はそれぞれ別に処理するようにしている。

### 実行結果

以下の内容のテキストファイルをimgに読み込ませておく

```default
aiueo
  tab tab
  tab tab
```

`cat`を実行してみると、ファイルの中身が正しく表示された。

<img src="/images/20190714/cat.png" class="blog-img img-responsive" alt="catコマンド" title="catコマンド"/>  


## File Allocation Table (FAT)を参照するようにする

上記の`cat`だと512バイトを超すファイルの場合に、うまく表示されないことがある。  
その場合、FATを参照にして次の512バイトがどこに配置されているかを知ることができる。  

まずは準備する。  

```rust
// file.rs
impl FileInfo {
    // FATを参考にしてファイルをロードする
    pub fn load_file(&self, buf_addr: usize, fat: &[u32; MAX_FAT], img_addr: usize) {
        let mut size = self.size as usize;
        let mut buf_addr = buf_addr as usize;
        let mut clustno = self.clustno as usize;
        loop {
            if size <= 512 {
                for i in 0..size {
                    let buf = unsafe { &mut *((buf_addr + i) as *mut u8) };
                    *buf = unsafe { *((img_addr + clustno * 512 + i) as *const u8) };
                }
                break;
            }
            for i in 0..512 {
                let buf = unsafe { &mut *((buf_addr + i) as *mut u8) };
                *buf = unsafe { *((img_addr + clustno * 512 + i) as *const u8) };
            }
            size -= 512;
            buf_addr += 512;
            clustno = fat[clustno] as usize;
        }
    }
}

// FAT情報を復号化
pub fn read_fat(fat: &mut [u32; MAX_FAT], img: [u8; MAX_FAT * 4]) {
    let mut j = 0;
    for i in (0..MAX_FAT).step_by(2) {
        fat[i + 0] = ((img[j + 0] as u32) | (img[j + 1] as u32) << 8) & 0xfff;
        fat[i + 1] = ((img[j + 1] as u32) >> 4 | (img[j + 2] as u32) << 4) & 0xfff;
        j += 3;
    }
}
```

これを使って、先程のファイルの中身のロード部分を書き換える。  

```rust
// console.rs
pub extern "C" fn console_task(sheet_index: usize) {
    // 省略
    let fat_addr = memman.alloc_4k(4 * MAX_FAT as u32).unwrap();
    let fat = unsafe { &mut *(fat_addr as *mut [u32; (MAX_FAT)]) };
    read_fat(fat, unsafe {
        *((ADR_DISKIMG + 0x000200) as *const [u8; (MAX_FAT * 4)])
    });
    // 省略
        // 引数にfatを追加
        exec_cmd(cmdline, cursor_y, sheet_manager, sheet_index, memtotal, fat);
// 省略

fn exec_cmd(
    cmdline: [u8; 30],
    cursor_y: isize,
    sheet_manager: &mut SheetManager,
    sheet_index: usize,
    memtotal: usize,
    fat: &[u32; MAX_FAT],
) -> isize {
    // 省略
        if let Some(finfo) = target_finfo {
            let content_addr = memman.alloc_4k(finfo.size).unwrap() as usize;
            finfo.load_file(content_addr, fat, ADR_DISKIMG + 0x003e00);
            let mut cursor_x = 8;
            for x in 0..finfo.size {
                let chr = unsafe { *((content_addr + x as usize) as *const u8) };
                if chr == 0x09 {
    // 省略
```

実行結果の画面は以前と変わらないので省略する。

## アプリケーションの起動

ここまでで、OS上でアプリケーションを起動できる準備が整った。  
簡単なアプリケーションとして以下のHLTするだけのアプリケーションを起動する。

```asm
fin:
  HLT
  JMP fin
```

流れとしては、まずは`cat`と同様、ファイルを探す。見つかった場合、今度は`cat`とは異なり、GDTでセグメントを登録し、farjmpでアプリケーションを起動する。  
`cat`と共通処理である、ファイルを探す部分を関数として抽出しておく。

```rust
// console.rs
fn search_file(filename: &[u8]) -> Option<FileInfo> {
    let mut target_finfo = None;
    // 拡張子の前後でわける
    let mut filename = filename.split(|c| *c == b'.');
    let basename = filename.next();
    let extname = filename.next();
    let mut b = [b' '; 8];
    let mut e = [b' '; 3];
    if let Some(basename) = basename {
        for fi in 0..b.len() {
            if basename.len() <= fi {
                break;
            }
            if b'a' <= basename[fi] && basename[fi] <= b'z' {
                // 小文字は大文字で正規化しておく
                b[fi] = basename[fi] - 0x20;
            } else {
                b[fi] = basename[fi];
            }
        }
    } else {
        return None;
    }
    if let Some(extname) = extname {
        for fi in 0..e.len() {
            if extname.len() <= fi {
                break;
            }
            if b'a' <= extname[fi] && extname[fi] <= b'z' {
                e[fi] = extname[fi] - 0x20;
            } else {
                e[fi] = extname[fi];
            }
        }
    }
    for findex in 0..MAX_FILE_INFO {
        let finfo = unsafe {
            *((ADR_DISKIMG + ADR_FILE_OFFSET + findex * core::mem::size_of::<FileInfo>())
                as *const FileInfo)
        };
        if finfo.name[0] == 0x00 {
            break;
        }
        if finfo.name[0] != 0xe5 {
            if (finfo.ftype & 0x18) == 0 {
                let mut filename_equal = true;
                for y in 0..finfo.name.len() {
                    if finfo.name[y] != b[y] {
                        filename_equal = false;
                        break;
                    }
                }
                for y in 0..finfo.ext.len() {
                    if finfo.ext[y] != e[y] {
                        filename_equal = false;
                        break;
                    }
                }
                if filename_equal {
                    target_finfo = Some(finfo);
                    break;
                }
            }
        }
    }
    target_finfo
}
```

`hlt`コマンドとして先ほどの`HLT`するだけのasmファイルをアセンブルした`hlt.bin`を探して起動するようにする。

```rust
// console.rs
    } else if cmd == "hlt" {
        let finfo = search_file(b"hlt.bin");
        if finfo.is_none() {
            display_error!("File Not Found", cursor_y);
        }
        let finfo = finfo.unwrap();
        let content_addr = memman.alloc_4k(finfo.size).unwrap() as usize;
        finfo.load_file(content_addr, fat, ADR_DISKIMG + 0x003e00);
        let gdt_offset = 1003; // 1,2,3はdesciptor_table.rsで、1002まではmt.rsで使用済
        let gdt = unsafe { &mut *((ADR_GDT + gdt_offset * 8) as *mut SegmentDescriptor) };
        *gdt = SegmentDescriptor::new(finfo.size - 1, content_addr as i32, AR_CODE32_ER);
        farjmp(0, gdt_offset * 8);
        memman.free_4k(content_addr as u32, finfo.size).unwrap();
```

### 実行結果

`hlt`と入力してエンターキーを押すと、以下のような画面のままコンソール画面が反応しなくなり、`HLT`されていそうなことがわかる。尚、入力フォームウィンドウは問題なく反応する。

<img src="/images/20190714/hlt.png" class="blog-img img-responsive" alt="hlt" title="hlt"/>  

また、`hlt.asm`の内容を以下の通り変更すると、意図通り全体がフリーズするようになる。

```asm
CLI
fin:
  HLT
  JMP fin
```


19日目は以上となる。ここまでの内容のコードは[yoshitsugu/hariboteos_in_rustのday19](https://github.com/yoshitsugu/hariboteos_in_rust/tree/day19)としてタグを打ってある。