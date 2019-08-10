---
title: 「30日でできる！OS自作入門」をRustで。26日目
tags: OS自作入門, OS, Rust
---

[「30日でできる！OS自作入門 」](https://book.mynavi.jp/supportsite/detail/4839919844.html)のC言語の部分をできるだけRustですすめてみる。今回は26日目の内容。
<!--more-->

## Sheetの移動を高速化する

本ではSheetの移動がQEMUだと遅いので高速化する、ということになっている。  
QEMUが改善されたのか、そもそもそこまで気にならなかったが、本の通りにすすめていく。  
まずは1バイトずつチェックしていたところを4バイトずつ埋めるように修正する。

```rust
// sheet.rs
impl SheetManager {
    pub fn refresh_map(&self, x0: i32, y0: i32, x1: i32, y1: i32, z0: i32) {
        if self.z_max.is_none() {
            return;
        }
        let x0 = max(0, x0);
        let y0 = max(0, y0);
        let x1 = min(x1, *SCREEN_WIDTH as i32);
        let y1 = min(y1, *SCREEN_HEIGHT as i32);
        for h in (z0 as usize)..=self.z_max.unwrap() {
            let si = self.sheets[h as usize];
            let sheet = &self.sheets_data[si];
            let bx0 = if x0 > sheet.x { x0 - sheet.x } else { 0 } as usize;
            let by0 = if y0 > sheet.y { y0 - sheet.y } else { 0 } as usize;
            let bx1 = if x1 > sheet.x {
                min(x1 - sheet.x, sheet.width)
            } else {
                0
            } as usize;
            let by1 = if y1 > sheet.y {
                min(y1 - sheet.y, sheet.height)
            } else {
                0
            } as usize;
            if let Some(t) = sheet.transparent {
                for by in by0..by1 {
                    let vy = (sheet.y + by as i32) as usize;
                    for bx in bx0..bx1 {
                        let vx = (sheet.x + bx as i32) as usize;
                        let width = sheet.width as usize;
                        let c = unsafe { *((sheet.buf_addr + by * width + bx) as *const Color) };
                        if c != t {
                            let ptr = unsafe {
                                &mut *((self.map_addr as *mut u8)
                                    .offset(vy as isize * *SCREEN_WIDTH as isize + vx as isize))
                            };
                            *ptr = si as u8;
                        }
                    }
                }
            } else {
                if (sheet.x & 3) == 0 && (x0 & 3) == 0 && (x1 & 3) == 0 {
                    // 4バイトずつ処理
                    let bx1: isize = (bx1 as isize - bx0 as isize) / 4;
                    let si4 = si | si << 8 | si << 16 | si << 24;
                    for by in by0..by1 {
                        let vy = (sheet.y + by as i32) as usize;
                        let vx = (sheet.x + bx0 as i32) as usize;
                        for bx in 0..bx1 {
                            let ptr = unsafe {
                                &mut *((self.map_addr as isize
                                    + vy as isize * *SCREEN_WIDTH as isize
                                    + vx as isize
                                    + bx as isize * 4)
                                    as usize as *mut u32)
                            };
                            *ptr = si4 as u32;
                        }
                    }
                } else {
                    for by in by0..by1 {
                        let vy = (sheet.y + by as i32) as usize;
                        for bx in bx0..bx1 {
                            let vx = (sheet.x + bx as i32) as usize;
                            let ptr = unsafe {
                                &mut *((self.map_addr as *mut u8)
                                    .offset(vy as isize * *SCREEN_WIDTH as isize + vx as isize))
                            };
                            *ptr = si as u8;
                        }
                    }
                }
            }
        }
    }

    pub fn refresh_part(&self, x0: i32, y0: i32, x1: i32, y1: i32, z0: i32, z1: i32) {
        if self.z_max.is_none() {
            return;
        }
        let x0 = max(0, x0);
        let y0 = max(0, y0);
        let x1 = min(x1, *SCREEN_WIDTH as i32);
        let y1 = min(y1, *SCREEN_HEIGHT as i32);

        for h in (z0 as usize)..=(z1 as usize) {
            let si = self.sheets[h as usize];
            let sheet = &self.sheets_data[si];
            let width = sheet.width as usize;
            let bx0 = if x0 > sheet.x { x0 - sheet.x } else { 0 } as usize;
            let by0 = if y0 > sheet.y { y0 - sheet.y } else { 0 } as usize;
            let bx1 = if x1 > sheet.x {
                min(x1 - sheet.x, sheet.width)
            } else {
                0
            } as usize;
            let by1 = if y1 > sheet.y {
                min(y1 - sheet.y, sheet.height)
            } else {
                0
            } as usize;
            if (sheet.x & 3) == 0 {
                // 4バイトずつ処理
                let i = (bx0 + 3) / 4;
                let i1 = max((bx1 / 4) as isize - i as isize, 0) as usize;
                let si4 = si | si << 8 | si << 16 | si << 24;
                for by in by0..by1 {
                    let vy = (sheet.y + by as i32) as usize;
                    let mut bx = bx0;
                    while bx < bx1 && (bx & 3) != 0 {
                        let vx = (sheet.x + bx as i32) as usize;
                        let map_si = unsafe {
                            *((self.map_addr as isize
                                + vy as isize * *SCREEN_WIDTH as isize
                                + vx as isize) as *const u8)
                        };
                        if si as u8 == map_si {
                            let c =
                                unsafe { *((sheet.buf_addr + by * width + bx) as *const Color) };
                            let ptr = unsafe {
                                &mut *((*VRAM_ADDR as *mut u8)
                                    .offset(vy as isize * *SCREEN_WIDTH as isize + vx as isize))
                            };
                            *ptr = c as u8;
                        }
                        bx += 1;
                    }
                    let vx = (sheet.x + bx as i32) as usize;
                    let p_base =
                        self.map_addr as isize + vy as isize * *SCREEN_WIDTH as isize + vx as isize;
                    let q_base =
                        *VRAM_ADDR as isize + vy as isize * *SCREEN_WIDTH as isize + vx as isize;
                    let r_base =
                        sheet.buf_addr as isize + by as isize * sheet.width as isize + bx as isize;
                    for i in 0..i1 {
                        let p = unsafe { &mut *((p_base + i as isize * 4) as *mut usize) };
                        let q = unsafe { &mut *((q_base + i as isize * 4) as *mut usize) };
                        let r = unsafe { &mut *((r_base + i as isize * 4) as *mut usize) };
                        if *p == si4 {
                            *q = *r;
                        } else {
                            let bx2 = bx + i as usize * 4;
                            let vx = sheet.x + bx2 as i32;
                            let p_base = self.map_addr as isize
                                + vy as isize * *SCREEN_WIDTH as isize
                                + vx as isize;
                            let q_base = *VRAM_ADDR as isize
                                + vy as isize * *SCREEN_WIDTH as isize
                                + vx as isize;
                            let r_base = sheet.buf_addr as isize
                                + by as isize * sheet.width as isize
                                + bx2 as isize;
                            for offset in 0..(4 as isize) {
                                let p = unsafe { &mut *((p_base + offset) as *mut u8) };
                                let q = unsafe { &mut *((q_base + offset) as *mut u8) };
                                let r = unsafe { &mut *((r_base + offset) as *mut u8) };
                                if *p == si as u8 {
                                    *q = *r
                                }
                            }
                        }
                    }
                    bx += i1 * 4;
                    while bx < bx1 {
                        let vx = (sheet.x + bx as i32) as usize;
                        let map_si = unsafe {
                            *((self.map_addr as isize
                                + vy as isize * *SCREEN_WIDTH as isize
                                + vx as isize) as *const u8)
                        };
                        if si as u8 == map_si {
                            let c =
                                unsafe { *((sheet.buf_addr + by * width + bx) as *const Color) };
                            let ptr = unsafe {
                                &mut *((*VRAM_ADDR as *mut u8)
                                    .offset(vy as isize * *SCREEN_WIDTH as isize + vx as isize))
                            };
                            *ptr = c as u8;
                        }
                        bx += 1;
                    }
                }
            } else {
                for by in by0..by1 {
                    let vy = (sheet.y + by as i32) as usize;
                    for bx in bx0..bx1 {
                        let vx = (sheet.x + bx as i32) as usize;
                        let width = sheet.width as usize;
                        let map_si = unsafe {
                            *((self.map_addr as isize
                                + vy as isize * *SCREEN_WIDTH as isize
                                + vx as isize) as *const u8)
                        };
                        if si as u8 == map_si {
                            let c =
                                unsafe { *((sheet.buf_addr + by * width + bx) as *const Color) };
                            let ptr = unsafe {
                                &mut *((*VRAM_ADDR as *mut u8)
                                    .offset(vy as isize * *SCREEN_WIDTH as isize + vx as isize))
                            };
                            *ptr = c as u8;
                        }
                    }
                }
            }
        }
    }
}
```

かなり長くなってしまったが、基本的にはSheetの位置が4で割り切れるときは4バイトずつ処理するように、という方向性で修正している。
体感的には速くなったのか微妙なところだが、一応動いた。

## 描画を省略する

描画を真面目に割り込みの度にやるのではなく、FIFOを全て消化するまでは位置の記憶だけで描画しないようにする。

```rust
// lib.rs
#[no_mangle]
#[start]
pub extern "C" fn hrmain() {
    // ウィンドウの移動
    let mut moving = false;
    let mut mouse_move_x = 0;
    let mut mouse_move_y = 0;
    let mut tmp_sheet_x = 0;
    // 追加
    let mut new_mx = -1;
    let mut new_my = 0;
    let mut new_wx = 0x7fffffff;
    let mut new_wy = 0;
    // 省略
            } else if 512 <= i && i <= 767 {
                if mouse_dec.decode((i - 512) as u8).is_some() {
                    let (new_x, new_y) = sheet_manager.get_new_point(
                        shi_mouse,
                        mouse_dec.x.get(),
                        mouse_dec.y.get(),
                    );
                    // sheet slideしていたところを位置の記憶だけにする
                    new_mx = new_x; 
                    new_my = new_y;
                    // 左クリックをおしていた場合
                    if (mouse_dec.btn.get() & 0x01) != 0 {
                        if moving {
                            let x = new_x - mouse_move_x;
                            let y = new_y - mouse_move_y;
                            // sheet slideしていたところを位置の記憶だけにする
                            new_wx = (x + tmp_sheet_x + 2) & !3;
                            new_wy = new_wy + y;
                            // 省略
                                            if 3 <= x && x < sheet.width - 3 && 3 <= y && y < 21 {
                                                // ウィンドウ移動モードへ
                                                moving = true;
                                                mouse_move_x = new_x;
                                                mouse_move_y = new_y;
                                                tmp_sheet_x = sheet.x;
                                                new_wy = sheet.y;
                                            }
                            // 省略
                    } else {
                        // 左クリックを押してなかったらウィンドウ移動モードからもどす
                        moving = false;
                        if new_wx != 0x7fffffff {
                            sheet_manager.slide(target_sheet_index, new_wx, new_wy);
                            new_wx = 0x7fffffff;
                        }
                    }
                }
            }
        } else {
            if new_mx >= 0 {
                sti();
                sheet_manager.slide(shi_mouse, new_mx, new_my);
                new_mx = -1;
            } else if new_wx != 0x7fffffff {
                sti();
                sheet_manager.slide(target_sheet_index, new_wx, new_wy);
                new_wx = 0x7fffffff;
            } else {
                task_manager.sleep(task_a_index);
                sti();
            }
        }
    }
}
```

体感的に少しスムーズになったような気がする。

## コンソールを手動で起動できるようにする

OS起動時のコンソールは1つとし、コンソールを手動で複数起動できるようにする。

```rust
// lib.rs
// lib.rs
#[no_mangle]
#[start]
pub extern "C" fn hrmain() {
    // 省略
    let mut active_window = open_console(sheet_manager, task_manager, memtotal);

    sheet_manager.slide(shi_mouse, mx, my);
    sheet_manager.slide(active_window, 56, 6);
    sheet_manager.updown(shi_bg, Some(0));
    sheet_manager.updown(active_window, Some(1));
    sheet_manager.updown(shi_mouse, Some(2));
    // 省略
                // Shift + F2 でコンソール起動
                if key == 0x3c
                    && (key_shift.0 == true || key_shift.1 == true)
                {
                    window_off(sheet_manager, task_manager, active_window);
                    active_window = open_console(sheet_manager, task_manager, memtotal);
                    sheet_manager.slide(active_window, 32, 4);
                    sheet_manager.updown(active_window, sheet_manager.z_max);
                    window_on(sheet_manager, task_manager, active_window);
                }
    // 省略
}

fn open_console(
    sheet_manager: &mut SheetManager,
    task_manager: &mut TaskManager,
    memtotal: u32,
) -> usize {
    let console_sheet = sheet_manager.alloc().unwrap();
    let memman = unsafe { &mut *(MEMMAN_ADDR as *mut MemMan) };
    let console_buf = memman
        .alloc_4k((CONSOLE_WIDTH * CONSOLE_HEIGHT) as u32)
        .unwrap() as usize;
    sheet_manager.set_buf(
        console_sheet,
        console_buf,
        CONSOLE_WIDTH as i32,
        CONSOLE_HEIGHT as i32,
        None,
    );
    make_window(
        console_buf,
        CONSOLE_WIDTH as isize,
        CONSOLE_HEIGHT as isize,
        "console",
        false,
    );
    make_textbox(
        console_buf,
        CONSOLE_WIDTH as isize,
        8,
        28,
        240,
        128,
        Color::Black,
    );
    let console_task_index = task_manager.alloc().unwrap();
    let mut console_task_mut = &mut task_manager.tasks_data[console_task_index];

    let console_fifo_addr = memman.alloc_4k(128 * 4).unwrap() as usize;
    let console_fifo = unsafe { &mut *(console_fifo_addr as *mut Fifo) };
    *console_fifo = Fifo::new(128, Some(console_task_index));
    console_task_mut.fifo_addr = console_fifo_addr;

    let console_esp = memman.alloc_4k(64 * 1024).unwrap() + 64 * 1024 - 12;
    console_task_mut.tss.esp = console_esp as i32;
    console_task_mut.tss.eip = console_task as i32;
    console_task_mut.tss.es = 1 * 8;
    console_task_mut.tss.cs = 2 * 8;
    console_task_mut.tss.ss = 1 * 8;
    console_task_mut.tss.ds = 1 * 8;
    console_task_mut.tss.fs = 1 * 8;
    console_task_mut.tss.gs = 1 * 8;

    let ptr = unsafe { &mut *((console_task_mut.tss.esp + 4) as *mut usize) };
    *ptr = console_sheet;
    let ptr = unsafe { &mut *((console_task_mut.tss.esp + 8) as *mut usize) };
    *ptr = memtotal as usize;
    task_manager.run(console_task_index, 2, 2);
    {
        let mut sheet_console = &mut sheet_manager.sheets_data[console_sheet];
        sheet_console.task_index = console_task_index;
        sheet_console.cursor = true;
    }
    console_sheet
}
```

コンソール関連の情報を配列でもっていたのを、`active_window`のみ残すようにして、その他はもたないようにした。

### 実行結果

`Shift+F2`で以下の通りコンソールを増やすことができた。
今回からいよいよウィンドウが増えて画面が狭くなったきたので、解像度を640x480にしている。

<img src="/images/20190810/multiple_console.png" class="blog-img img-responsive" alt="コンソールの手動起動" title="コンソールの手動起動"/> 


## exitコマンドの実装

コンソールを増やせるようになったので、今度は`exit`で終了できるようにする。

```rust
// console.rs
impl Console {
    fn run_cmd(&mut self, cmdline: [u8; MAX_CMD], memtotal: usize, fat: &[u32; MAX_FAT]) {
        self.cursor_x = 8;
        let cmdline_strs = cmdline.split(|s| *s == 0 || *s == b' ');
        let mut cmdline_strs = cmdline_strs.skip_while(|cmd| cmd.len() == 0);
        let cmd = cmdline_strs.next();
        if cmd.is_none() {
            self.display_error("Bad Command");
            return;
        }
        let cmd = cmd.unwrap();
        let cmd_str = from_utf8(&cmd).unwrap();
        if cmd_str == "mem" {
            self.cmd_mem(memtotal);
        } else if cmd_str == "clear" {
            self.cmd_clear();
        } else if cmd_str == "ls" {
            self.cmd_ls();
        } else if cmd_str == "cat" {
            self.cmd_cat(cmdline_strs, fat);
        } else if cmd_str == "exit" { // 追加
            self.cmd_exit(fat);
        } else {
            self.cmd_app(&cmd, fat);
        }
    }
    // 省略
 
    pub fn cmd_exit(&mut self, fat: &[u32; MAX_FAT]) {
        let memman = unsafe { &mut *(MEMMAN_ADDR as *mut MemMan) };
        let task_a_fifo_addr = unsafe { *(TASK_A_FIFO_ADDR as *const usize) };
        let task_a_fifo = unsafe { &mut *(task_a_fifo_addr as *mut Fifo) };
        TIMER_MANAGER.lock().cancel(self.timer_index);
        memman.free_4k(fat.as_ptr() as u32, 4 * 2880).unwrap();
        cli();
        task_a_fifo
            .put(self.sheet_index as u32 + EXIT_OFFSET as u32)
            .unwrap();
        sti();
        let task_manager = unsafe { &mut *(TASK_MANAGER_ADDR as *mut TaskManager) };
        let task_index = task_manager.now_index();
        loop {
            task_manager.sleep(task_index);
        }
    }
}
```

自分自身をcloseしないといけないため、一旦fifo経由で`task_a`に`sheet_index`渡し、`task_a`に終了してもらうようにする。
また、終了にあたり、スタック領域を記憶しておく。

```rust
// mt.rs
#[derive(Debug, Clone, Copy)]
pub struct Task {
    pub select: i32,
    pub flag: TaskFlag,
    pub level: usize,
    pub priority: i32,
    pub tss: TSS,
    pub fifo_addr: usize,
    pub console_addr: usize,
    pub ds_base: usize,
    pub console_stack: usize, // <- 追加
}

impl TaskManager {
    pub fn close_task(&mut self, task_index: usize) {
        self.sleep(task_index);
        let mut task = &mut self.tasks_data[task_index];
        let memman = unsafe { &mut *(MEMMAN_ADDR as *mut MemMan) };
        memman.free_4k(task.console_stack as u32, 64 * 1024).unwrap();
        memman.free_4k(task.fifo_addr as u32, 128 * 4).unwrap();
        task.flag = TaskFlag::AVAILABLE;
    }
}
```

```rust
// sheet.rs
impl SheetManager {
    pub fn close(&mut self, sheet_index: usize) {
        let memman = unsafe { &mut *(MEMMAN_ADDR as *mut MemMan) };
        let task_manager = unsafe { &mut *(TASK_MANAGER_ADDR as *mut TaskManager) };
        let sheet = self.sheets_data[sheet_index];
        memman.free_4k(sheet.buf_addr as u32, 256 * 165).unwrap();
        self.free(sheet_index);
        task_manager.close_task(sheet.task_index);
    }
}
```

コンソールが0になることもあるので、`lib.rs`では`active_window`でチェックするようにする。

```rust
// lib.rs
pub const TASK_A_FIFO_ADDR: usize = 0xfec;
pub const EXIT_OFFSET: usize = 768;

#[no_mangle]
#[start]
pub extern "C" fn hrmain() {
    // 省略
        if fifo.status() != 0 {
            let i = fifo.get().unwrap();
            sti();
            let active_sheet = sheet_manager.sheets_data[active_window];
            if active_window != 0 && active_sheet.flag == SheetFlag::AVAILABLE {
                // ウィンドウが閉じられた
                if let Some(zmax) = sheet_manager.z_max {
                    // もうマウスと背景しかない
                    if zmax == 1 {
                        active_window = 0;
                    } else {
                        active_window = sheet_manager.sheets[zmax - 1];
                        window_on(sheet_manager, task_manager, active_window);
                    }
                }
            }
    // 省略
                if chr != 0 && active_window != 0 {
    // 省略
                // タブ
                if key == 0x0f && active_window != 0 {
    // 省略
                // Shift + F2 でコンソール起動
                if key == 0x3c && (key_shift.0 == true || key_shift.1 == true) {
                    if active_window != 0 {
                        window_off(sheet_manager, task_manager, active_window);
                    }
                    active_window = open_console(sheet_manager, task_manager, memtotal);
    // 省略
                // Shift + F1 でアプリケーションを強制終了
                {
                    let mut console_task_mut =
                        &mut task_manager.tasks_data[active_sheet.task_index];
                    if key == 0x3b
                        && (key_shift.0 == true || key_shift.1 == true)
                        && console_task_mut.tss.ss0 != 0
                        && active_window != 0
                    {
    // 省略
            } else if EXIT_OFFSET as u32 <= i && i <= 1023 {
                sheet_manager.close(i as usize - EXIT_OFFSET);
            }
    // 省略
    }
}

fn open_console(
    sheet_manager: &mut SheetManager,
    task_manager: &mut TaskManager,
    memtotal: u32,
) -> usize {
    // 省略
    console_task_mut.console_stack = memman.alloc_4k(64 * 1024).unwrap() as usize; // スタックの起点のアドレスを覚えておく
    console_task_mut.tss.esp = console_task_mut.console_stack as i32 + 64 * 1024 - 12;
    // 省略
}
```

### 実行結果

以下の通り、コンソールを終了できるようになった。

<img src="/images/20190810/exit.gif" class="blog-img img-responsive" alt="コンソールの終了" title="コンソールの終了"/>  


## exitをマウス操作でできるようにする

コンソールの×ボタンをクリックしたときに先ほど作ったexitコマンドが発行されるようにする。

```rust
// lib.rs
pub const EXIT_CONSOLE: u32 = 4;


#[no_mangle]
#[start]
pub extern "C" fn hrmain() {
    // 省略
    // マウスクリックのハンドリング箇所
    if sheet.width - 21 <= x
        && x < sheet.width - 5
        && 5 <= y
        && y < 19
    {
        //×ボタンクリック
        if sheet.from_app {
        // 省略
        } else {
            // コンソールのクローズ
            let task =
                task_manager.tasks_data[sheet.task_index];
            cli();
            let console_fifo = unsafe { &mut *(task.fifo_addr as *mut Fifo) };
            console_fifo.put(EXIT_CONSOLE).unwrap();
            sti();
        }
    // 省略
    }
}
```

```rust
// console.rs
pub extern "C" fn console_task(sheet_index: usize, memtotal: usize) {
    // 省略
            } else if i == EXIT_CONSOLE {
                console.cmd_exit(fat);
            }
    // 省略
}
```

これでマウスクリックでもコンソールを閉じられるようになった。

## start コマンド

新しいコンソールを開いた上でコマンドを実行する、`start` コマンドを実装する。

```rust
impl Console {
    fn run_cmd(&mut self, cmdline: [u8; MAX_CMD], memtotal: usize, fat: &[u32; MAX_FAT]) {
        // 省略
        } else if cmd_str == "start" {
            self.cmd_start(cmdline_strs, memtotal as u32);
        // 省略
    }

    pub fn cmd_start<'a>(&mut self, cmdline_strs: impl Iterator<Item = &'a [u8]>, memtotal: u32) {
        let mut cmd = cmdline_strs.skip_while(|strs| strs.len() == 0);
        let cmd = cmd.next();
        if cmd.is_none() {
            self.display_error("Command Not Found");
            return;
        }
        let cmd = cmd.unwrap();
        let sheet_manager = unsafe { &mut *(self.sheet_manager_addr as *mut SheetManager) };
        let task_manager = unsafe { &mut *(TASK_MANAGER_ADDR as *mut TaskManager) };
        let sheet_index = open_console(sheet_manager, task_manager, memtotal);
        let task = &task_manager.tasks_data[sheet_manager.sheets_data[sheet_index].task_index];
        let fifo = unsafe { &mut *(task.fifo_addr as *mut Fifo) };
        sheet_manager.slide(sheet_index, 32, 4);
        sheet_manager.updown(sheet_index, sheet_manager.z_max);
        for ci in 0..cmd.len() {
            fifo.put(cmd[ci] as u32 + 256).unwrap();
        }
        fifo.put(10 + 256).unwrap(); // Enter
        self.newline();
    }
}


```

### 実行結果

以下の通り、コンソールを 新たに起動してアプリケーション実行ができるようになった。

<img src="/images/20190810/start.gif" class="blog-img img-responsive" alt="startコマンド" title="startコマンド"/>  

## コンソールなしのstart

コンソールなしの`start`を実装する。コマンド名は本にならって`ncst`とした。  
コンソールなしを実現するために、`sheet_index`が0の`Console`を作れるようにする。

```rust
impl Console {
    fn run_cmd(&mut self, cmdline: [u8; MAX_CMD], memtotal: usize, fat: &[u32; MAX_FAT]) {
        // 省略
        if cmd_str == "mem" && self.sheet_index != 0 {
            self.cmd_mem(memtotal);
        } else if cmd_str == "clear" && self.sheet_index != 0 {
            self.cmd_clear();
        } else if cmd_str == "ls" && self.sheet_index != 0 {
            self.cmd_ls();
        } else if cmd_str == "cat" && self.sheet_index != 0 {
            self.cmd_cat(cmdline_strs, fat);
        } else if cmd_str == "start" {
            self.cmd_start(cmdline_strs, memtotal as u32);
        } else if cmd_str == "ncst" {
            self.cmd_ncst(cmdline_strs, memtotal as u32);
        } else if cmd_str == "exit" {
            self.cmd_exit(fat);
        } else {
            self.cmd_app(&cmd, fat);
        }
    }
    
    pub fn cmd_ncst<'a>(&mut self, cmdline_strs: impl Iterator<Item = &'a [u8]>, memtotal: u32) {
        let task_manager = unsafe { &mut *(TASK_MANAGER_ADDR as *mut TaskManager) };
        let mut cmd = cmdline_strs.skip_while(|strs| strs.len() == 0);
        let cmd = cmd.next();
        if cmd.is_none() {
            self.display_error("Command Not Found");
            return;
        }
        let cmd = cmd.unwrap();
        let task_index = open_console_task(task_manager, 0, memtotal);
        let task = &task_manager.tasks_data[task_index];
        let fifo = unsafe { &mut *(task.fifo_addr as *mut Fifo) };
        for ci in 0..cmd.len() {
            fifo.put(cmd[ci] as u32 + 256).unwrap();
        }
        fifo.put(10 + 256).unwrap(); // Enter
        self.newline();
    }
}
```

コンソール画面上でインタラクティブに表示する形のコマンドはそもそもコンソールがない場合は使わないのでスキップするようにした。

`open_console_task`は以下のように定義する。

```rust
// lib.rs

pub fn open_console_task(
    task_manager: &mut TaskManager,
    sheet_index: usize, memtotal: u32
) -> usize {
    let memman = unsafe { &mut *(MEMMAN_ADDR as *mut MemMan) };
    let console_task_index = task_manager.alloc().unwrap();
    let mut console_task_mut = &mut task_manager.tasks_data[console_task_index];

    let console_fifo_addr = memman.alloc_4k(128 * 4).unwrap() as usize;
    let console_fifo = unsafe { &mut *(console_fifo_addr as *mut Fifo) };
    *console_fifo = Fifo::new(128, Some(console_task_index));
    console_task_mut.fifo_addr = console_fifo_addr;

    console_task_mut.console_stack = memman.alloc_4k(64 * 1024).unwrap() as usize;
    console_task_mut.tss.esp = console_task_mut.console_stack as i32 + 64 * 1024 - 12;
    console_task_mut.tss.eip = console_task as i32;
    console_task_mut.tss.es = 1 * 8;
    console_task_mut.tss.cs = 2 * 8;
    console_task_mut.tss.ss = 1 * 8;
    console_task_mut.tss.ds = 1 * 8;
    console_task_mut.tss.fs = 1 * 8;
    console_task_mut.tss.gs = 1 * 8;

    let ptr = unsafe { &mut *((console_task_mut.tss.esp + 4) as *mut usize) };
    *ptr = sheet_index;
    let ptr = unsafe { &mut *((console_task_mut.tss.esp + 8) as *mut usize) };
    *ptr = memtotal as usize;
    task_manager.run(console_task_index, 2, 2);
    console_task_index
}


pub fn open_console(
    sheet_manager: &mut SheetManager,
    task_manager: &mut TaskManager,
    memtotal: u32,
) -> usize {
    let console_sheet = sheet_manager.alloc().unwrap();
    let memman = unsafe { &mut *(MEMMAN_ADDR as *mut MemMan) };
    let console_buf = memman
        .alloc_4k((CONSOLE_WIDTH * CONSOLE_HEIGHT) as u32)
        .unwrap() as usize;
    sheet_manager.set_buf(
        console_sheet,
        console_buf,
        CONSOLE_WIDTH as i32,
        CONSOLE_HEIGHT as i32,
        None,
    );
    make_window(
        console_buf,
        CONSOLE_WIDTH as isize,
        CONSOLE_HEIGHT as isize,
        "console",
        false,
    );
    make_textbox(
        console_buf,
        CONSOLE_WIDTH as isize,
        8,
        28,
        240,
        128,
        Color::Black,
    );
    {
        let mut sheet_console = &mut sheet_manager.sheets_data[console_sheet];
        sheet_console.task_index = open_console_task(task_manager, console_sheet, memtotal);
        sheet_console.cursor = true;
    }
    console_sheet
}
```

`open_console`のほうも`open_console_task`を使うように変更している。  
その他細々修正があるが、一旦ここでは記載を省略する。

### 実行結果

以下の通り、アプリケーションウィンドウだけ起動することができるようになった。

<img src="/images/20190810/ncst.gif" class="blog-img img-responsive" alt="ncstコマンド" title="ncstコマンド"/>  


26日目は以上となる。ここまでの内容のコードは[yoshitsugu/hariboteos_in_rustのday26](https://github.com/yoshitsugu/hariboteos_in_rust/tree/day26)としてタグを打ってある。