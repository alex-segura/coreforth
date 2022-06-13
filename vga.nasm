;; -*- mode: nasm -*-

global vga_init
global vga_putc
global vga_clear
global vga_scroll_up
global vga_set_cursor
global vga_enable_cursor

extern ADD
extern DIV
extern MOD

%include "coreforth.inc"

%define COLUMNS 80
%define ROWS 25

%define CRTC_INDEX   0x3d4
%define CRTC_DATA    0x3d5

%define CURSOR_MSL   0x09
%define CURSOR_START 0x0a
%define CURSOR_END   0x0b

%macro VIDEO 2
%endmacro

[section .text]

;; ( u8 -- u8 )
crtc_read:
  push edx
  mov edx, CRTC_INDEX
  out dx, al
  mov edx, CRTC_DATA
  in al, dx
  pop edx
  ret

;; ( u8 u8 -- )
crtc_write:
  mov eax, [esp+4]
  mov ebx, [PSP]
  push edx
  mov edx, CRTC_INDEX
  out dx, bl
  mov edx, CRTC_DATA
  out dx, al
  pop edx
  _DROP
  _DROP
  ret

;; ( -- x y flag )
vga_get_cursor:
  PUSHPSP 0x0E
  call crtc_read
  shl TOS, 8
  PUSHPSP 0x0F
  call crtc_read
  call ADD
  PUSHPSP COLUMNS
  call DIVMOD
  PUSHPSP 0x0A
  call crtc_read
  and TOS, (1<<5)
  not TOS
  ret

;; ( x y -- )
vga_set_cursor:

;; ( s -- )
vga_enable_cursor:

;; ( row ch attr -- )
vga_clear_line:
  ret

;; ( -- )
vga_scroll_up:
  ret

;; ( ch attr -- )
vga_fill:
  ret

;; ( -- )
vga_clear:
  ret

;; ( row col c -- )
vga_putc:
  ret

;; ( -- )
vga_init_cursor:
  ret

;; ( -- )
vga_init:
  call vga_init_cursor

[section .data]
