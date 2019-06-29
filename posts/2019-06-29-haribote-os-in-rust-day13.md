---
title: 「30日でできる！OS自作入門」をRustで。13日目
tags: OS自作入門, OS, Rust
---

[「30日でできる！OS自作入門 」](https://book.mynavi.jp/supportsite/detail/4839919844.html)のC言語の部分をできるだけRustですすめてみる。今回は13日目の内容。
<!--more-->

## `write_with_bg!` マクロの導入

本では矩形を描画し、その上に文字を表示する部分をまとめて関数にしていた。  
たしかに、Rustのコードでも、矩形と文字描画の部分で、サイズ指定などが重複しており煩わしかったので、マクロでまとめてみる。

```rust
// vga.rs
#[macro_export]
macro_rules! write_with_bg {
    ($sheet_manager: expr, $sheet_addr: expr, $dst: expr, $width: expr, $height: expr, $x: expr, $y: expr, $fg: expr, $bg: expr, $length: expr, $($arg: tt)* ) => {{
        boxfill($dst, $width, $bg, $x, $y, $x + 8 * $length - 1, $y + 15);
        let mut writer = ScreenWriter::new(
                    Some($dst),
                    $fg,
                    $x,
                    $y,
                    $width as usize,
                    $height as usize);
        use core::fmt::Write;
        write!(writer, $($arg)*).unwrap();
        $sheet_manager.refresh($sheet_addr, $x, $y, $x + $length * 8, $y + 16);
    }}
}
```

引数が長大になってしまったので、あまり見た目はよくないが、少なくともrefreshと矩形領域のサイズのミスは防ぐことができるようになったのでよしとする。  

差分は[GitHubのdiff](https://github.com/yoshitsugu/hariboteos_in_rust/commit/11bd1b2ccda8b7028aab6fc03e23f582870ea131?diff=split)を見てもらうのがわかりやすいかと思う。


## タイマ用のFIFOをまとめる

現状、タイマごとに別のFIFOを使っていたが、まとめるようにする。

```rust
//lib.rs
#[no_mangle]
#[start]
pub extern "C" fn haribote_os() {
    // 省略
    let timer_buf = Fifo::new(8); // <- 一つに統合

    // FIFOを統一して、キューにつめるデータで区別できるようにする
    let timer_index1 = TIMER_MANAGER.lock().alloc().unwrap();
    TIMER_MANAGER
        .lock()
        .init_timer(timer_index1, &timer_buf, 10);
    TIMER_MANAGER.lock().set_time(timer_index1, 1000);
    let timer_index2 = TIMER_MANAGER.lock().alloc().unwrap();
    TIMER_MANAGER.lock().init_timer(timer_index2, &timer_buf, 3);
    TIMER_MANAGER.lock().set_time(timer_index2, 300);
    let timer_index3 = TIMER_MANAGER.lock().alloc().unwrap();
    TIMER_MANAGER
        .lock()
        .init_timer(timer_index3, &timer_buf, 1);
    TIMER_MANAGER.lock().set_time(timer_index3, 50);

    // 省略
    loop {
        // 省略
        } else if timer_buf.status() != 0 {
            // timer_bufで判定したあとに、データで判定
            let i = timer_buf.get().unwrap();
            sti();
            if i == 10 {
                write_with_bg!(
                    sheet_manager,
                    shi_bg,
                    buf_bg_addr,
                    *SCREEN_WIDTH as isize,
                    *SCREEN_HEIGHT as isize,
                    0,
                    64,
                    Color::White,
                    Color::DarkCyan,
                    7,
                    "10[sec]"
                );
            } else if i == 3 {
                // ...
            } else {
                if i != 0 {
                    // ...
                } else {
                    // ...
                }
            }
    // 省略
```

## ベンチマークの導入

性能を計測するために、ベンチマークを導入する。  
ここでは、エントリポイントとなる関数のloopにカウントアップする変数を仕込み、起動後3秒-10秒で何回ループが発生したかを計測することで、ベンチマークとする。

```rust
// lib.rs
#[no_mangle]
#[start]
pub extern "C" fn haribote_os() {
    // 省略
    let mut count = 0;
    let mut count_done = false;
    loop {
        count += 1;
        // 省略
            } else if i == 10 {
                write_with_bg!(sheet_manager, shi_bg, buf_bg_addr, *SCREEN_WIDTH as isize, *SCREEN_HEIGHT as isize,
                                0, 64, Color::White, Color::DarkCyan, 7, "10[sec]");
                if !count_done {
                    // ウィンドウ内にループ数を表示
                    write_with_bg!(sheet_manager, shi_win, buf_win_addr, 160, 52, 
                                   40, 28, Color::Black,  Color::LightGray, 10, "{:>010}", count);
                    count_done = true;
                }
            } else if i == 3 {
                write_with_bg!(sheet_manager, shi_bg, buf_bg_addr, *SCREEN_WIDTH as isize, *SCREEN_HEIGHT as isize,
                              0, 80, Color::White, Color::DarkCyan, 6, "3[sec]");
                // 起動直後から測定すると誤差が大きいのでここから測定
                count = 0;
            }
            // 省略
```        

### 実行結果

5回実行してみて、結果は以下の通りだった。

```default
1回目: 50507701
2回目: 47596192
3回目: 39071813
4回目: 43176467
5回目: 47981996
```

結構バラつきがあるが、QEMU上での確認なのでホストマシンの状況にも影響をうけそうだ。

## FIFOキューを統一

性能向上のため、割り込みデータを保持するのに使っているFIFOキューをキーボード用、マウス用、タイマ用でわけず、すべて同じFIFOキューを使うようにする。キューにつめこむデータの値の範囲で区別できるようにする。 
以下、細かい差分が多いので、おおまかなところの差分だけ載せる。

```rust
// fifo.rs
pub struct Fifo {
    pub buf: RefCell<[u32; 128]>, // 値をカバーできるようにu8 -> u32にする
    pub p: Cell<u32>,
    pub q: Cell<u32>,
    pub free: Cell<u32>,
    pub flags: Cell<u32>,
    pub size: u32,
}

lazy_static! {
    // 割り込み全般で使うように汎用的なFIFOキューを定義しておく。
    pub static ref FIFO_BUF: Mutex<Fifo> = Mutex::new(Fifo::new(128));
}
```

```rust
// interrupt.rs
const KEYBOARD_OFFSET: u32 = 256;
const MOUSE_OFFSET: u32 = 512;

pub extern "C" fn inthandler21() {
    out8(PIC0_OCW2, 0x61); // IRQ-01 受付終了
    let key = in8(PORT_KEYDAT);
    FIFO_BUF.lock().put(key as u32 + KEYBOARD_OFFSET).unwrap();
}

pub extern "C" fn inthandler2c() {
    out8(PIC1_OCW2, 0x64); // IRQ-12受付完了をPIC1に通知
    out8(PIC0_OCW2, 0x62); // IRQ-02受付完了をPIC0に通知
    let data = in8(PORT_KEYDAT);
    FIFO_BUF.lock().put(data as u32 + MOUSE_OFFSET).unwrap();
}
```

```rust
// lib.rs
#[no_mangle]
#[start]
pub extern "C" fn haribote_os() {
    // 省略
        if FIFO_BUF.lock().status() != 0 { // FIFO_BUFで判定
            let i = FIFO_BUF.lock().get().unwrap();
            sti();
            if 256 <= i && i <= 511 {
                // キーボード用の処理
            } else if 512 <= i && i <= 767 {
                // マウス用の処理
            } else if // タイマ用の処理 省略
```

### 実行結果

FIFOキューを統一した結果の性能向上を測るため、再度ベンチマークで計測してみた

```default
1回目: 70267378
2回目: 73037165
3回目: 69773835
4回目: 69880820
5回目: 70381397
```

1.5倍ほど向上してそうだ。

## タイマの順番を線形リスト形式で保持するようにする

現状、タイマの順番は配列形式でもっているが、これは先頭がタイムアウトになると後ろを詰めなおす処理がいるので非効率だ。(配列の長さをnとするとO(n)かかる。)  
線形リストのように、次の要素のインデックスを要素自身にもたせることで効率化を図る。

```rust
//timer.rs
#[derive(Debug, Clone, Copy)]
pub struct Timer {
    pub timeout: u32,
    pub flag: TimerFlag,
    pub data: u8,
    pub next: usize, // 追加 次の要素のインデックスをもつ
}

impl Timer {
    pub fn new() -> Timer {
        Timer {
            timeout: 0,
            flag: TimerFlag::AVAILABLE,
            data: 0,
            next: 0,
        }
    }
}

pub struct TimerManager {
    pub count: u32,
    pub next_tick: u32, // <- 元々nextだったが、紛らわしいので名前変更
    pub counting: u32,
    pub t0: usize,
    pub timers_data: [Timer; MAX_TIMER],
}
```

各関数も変更する。

```rust
//timer.rs
pub extern "C" fn inthandler20() {
    out8(PIC0_OCW2, 0x60); // IRQ-00受付完了をPICに通知
    let mut tm = TIMER_MANAGER.lock();
    tm.count += 1;
    if tm.next_tick > tm.count {
        return;
    }
    let mut timer_index = tm.t0;
    let mut timeout_count = 0;
    for i in 0..tm.counting {
        timeout_count = i;
        if tm.timers_data[timer_index].timeout > tm.count {
            break;
        }
        {
            let mut t_mut = &mut tm.timers_data[timer_index];
            t_mut.flag = TimerFlag::USED;
        }
        {
            let timer = &tm.timers_data[timer_index];
            FIFO_BUF.lock().put(timer.data as u32).unwrap();
            timer_index = timer.next;
        }
    }
    tm.counting -= timeout_count;
    tm.t0 = timer_index;

    if tm.counting > 0 {
        tm.next_tick = tm.timers_data[tm.t0].timeout;
    } else {
        tm.next_tick = 0xffffffff;
    }
}

impl TimerManager {
    // 省略
        pub fn set_time(&mut self, timer_index: usize, timeout: u32) {
        {
            let mut timer = &mut self.timers_data[timer_index];
            timer.timeout = timeout + self.count;
            timer.flag = TimerFlag::COUNTING;
        }
        let eflags = load_eflags();
        cli();
        self.counting += 1;
        if self.counting == 1 {
            let mut timer = &mut self.timers_data[timer_index];
            self.t0 = timer_index;
            timer.next = 0;
            self.next_tick = timer.timeout;
            store_eflags(eflags);
            return;
        }
        let mut t_index = self.t0;
        if &self.timers_data[timer_index].timeout <= &self.timers_data[t_index].timeout {
            // 先頭に入れる
            let mut timer = &mut self.timers_data[timer_index];
            self.t0 = timer_index;
            timer.next = t_index;
            self.next_tick = timer.timeout;
            store_eflags(eflags);
            return;
        }
        let mut old_t_index = t_index;
        // 間に入るところを探す
        loop {
            old_t_index = t_index;
            t_index = self.timers_data[t_index].next;
            if t_index == 0 {
                break;
            }
            if self.timers_data[timer_index].timeout <= self.timers_data[t_index].timeout {
                {
                    let mut s = &mut self.timers_data[old_t_index];
                    s.next = timer_index;
                }
                {
                    let mut timer = &mut self.timers_data[timer_index];
                    timer.next = t_index;
                }
                store_eflags(eflags);
                return;
            }
        }
        // 入るところが見つからなかったので、最後に入れる
        {
            let mut s = &mut self.timers_data[old_t_index];
            s.next = timer_index;
        }
        {
            let mut timer = &mut self.timers_data[timer_index];
            timer.next = 0;
        }

        store_eflags(eflags);
    }
    // 省略
```

所有権の問題回避でカッコが多いが、やっていることはそこまで難しくない。

### 実行結果

本の通り、これだけではあまり速くならなかった。

```default
1回目: 71964683
2回目: 68172741
3回目: 68513039
4回目: 71347232
5回目: 77214783
```

## 長さではなく番兵で判定するようにする

上記に加えて、有効なタイマの長さベースで管理するのではなく、最初からタイマの順番用配列に番兵となるようなデータを入れておくことで判定ロジックを簡単にする

```rust
// timer.rs

impl TimerManager {
    pub fn new() -> TimerManager {
        let mut tm = TimerManager {
            count: 0,
            next_tick: 0xffffffff,
            t0: Some(MAX_TIMER - 1),
            timers_data: [Timer::new(); MAX_TIMER],
        };
        // 番兵
        tm.timers_data[MAX_TIMER - 1] = Timer {
            timeout: 0xffffffff,
            flag: TimerFlag::COUNTING,
            data: 0,
            next: None,
        };
        tm
    }

    pub fn set_time(&mut self, timer_index: usize, timeout: u32) {
        {
            let mut timer = &mut self.timers_data[timer_index];
            timer.timeout = timeout + self.count;
            timer.flag = TimerFlag::COUNTING;
        }
        if self.t0.is_none() {
            return;
        }
        let eflags = load_eflags();
        cli();
        let mut t_index = self.t0.unwrap();
        if &self.timers_data[timer_index].timeout <= &self.timers_data[t_index].timeout {
            // 先頭に入れる
            let mut timer = &mut self.timers_data[timer_index];
            self.t0 = Some(timer_index);
            timer.next = Some(t_index);
            self.next_tick = timer.timeout;
            store_eflags(eflags);
            return;
        }
        let mut old_t_index: usize;
        // 挿入できるインデックスをさがす
        loop {
            old_t_index = t_index;
            if self.timers_data[t_index].next.is_none() {
                store_eflags(eflags);
                break;
            }
            t_index = self.timers_data[t_index].next.unwrap();
            if self.timers_data[timer_index].timeout <= self.timers_data[t_index].timeout {
                {
                    let mut s = &mut self.timers_data[old_t_index];
                    s.next = Some(timer_index);
                }
                {
                    let mut timer = &mut self.timers_data[timer_index];
                    timer.next = Some(t_index);
                }
                store_eflags(eflags);
                return;
            }
        }
    }
    // 省略
}

pub extern "C" fn inthandler20() {
    out8(PIC0_OCW2, 0x60); // IRQ-00受付完了をPICに通知
    let mut tm = TIMER_MANAGER.lock();
    tm.count += 1;
    if tm.next_tick > tm.count {
        return;
    }
    let mut timer_index = tm.t0;
    loop {
        if timer_index.is_none() {
            return;
        }
        let t_index = timer_index.unwrap();
        if tm.timers_data[t_index].timeout > tm.count {
            break;
        }
        let mut timer = &mut tm.timers_data[t_index];
        timer.flag = TimerFlag::USED;
        FIFO_BUF.lock().put(timer.data as u32).unwrap();
        timer_index = timer.next;
    }
    tm.t0 = timer_index;
    if let Some(t_index) = timer_index {
        tm.next_tick = tm.timers_data[t_index].timeout;
    }
}
```

### 実行結果

```default
1回目: 67180042
2回目: 75023138
3回目: 71575761
4回目: 76029007
5回目: 76825781
```

少し速くなったようだ。  

13日目は以上となる。ここまでの内容のコードは[yoshitsugu/hariboteos_in_rustのday13](https://github.com/yoshitsugu/hariboteos_in_rust/tree/day13)としてタグを打ってある。