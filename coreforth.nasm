;; -*- mode: nasm -*-

[bits 32]

%define TOS eax
%define RSP esp
%define PSP esi

%define RETURN_STACK_SIZE 4096
%define DATA_STACK_SIZE 4096
%define TEXT_BUFFER_SIZE 80
%define CELL_SIZE 4             ; cell size in bytes

%define READ_UPCASE 0
%define READ_NORMAL 1

%macro _DUP 0
  sub PSP, CELL_SIZE
  mov [PSP], TOS
%endmacro

%macro _SWAP 0
  xchg TOS, [PSP]
%endmacro

%macro _OVER 0
  sub PSP, CELL_SIZE
  mov [PSP], TOS
  mov TOS, [PSP+CELL_SIZE]
%endmacro

%macro _DROP 0
  lodsd
%endmacro

%macro PUSHPSP 1
  _DUP
  mov TOS, %1
%endmacro

%macro POPPSP 1
   mov %1, TOS
  _DROP
%endmacro

F_IMMED     equ 0x80
F_COMP_ONLY equ 0x40
F_HIDDEN    equ 0x20
F_LENMASK   equ 0x1f

F_BLK_UPDATED equ 0x80000000
F_BLK_NUMMASK equ 0x7fffffff

%xdefine link 0

%macro ENDWORDLIST 2
%strlen %%cnt %1
[section .rodata]
%2:
  dd link
name_%2:
  db %%cnt
  db %1
align CELL_SIZE, db 0
%xdefine link 0
%endmacro

%macro DEFCODE 2-3 0
[section .rodata]
%strlen %%cnt %1
align CELL_SIZE, db 0
cell_%2:
  dd link
  dd name_%2
name_%2:
  db %3+%%cnt
  db %1
align CELL_SIZE, db 0
cfa_%2:
  dd %2
[section .text]
%xdefine link cell_%2
%2:
%endmacro

%macro DEFVAR 3-4 0
[section .rodata]
%strlen %%cnt %1
align CELL_SIZE, db 0
cell_%2:
  dd link
  dd name_%2
name_%2:
  db %4+%%cnt
  db %1
align CELL_SIZE, db 0
  dd %2
var_%2:
  dd %3
[section .text]
%2:
  mov ebx, var_%2
  PUSHPSP ebx
  ret
%xdefine link cell_%2
%endmacro

%macro DEFCONST 3-4 0
[section .rodata]
%strlen %%cnt %1
align CELL_SIZE, db 0
cell_%2:
  dd link
  dd name_%2
name_%2:
  db %4+%%cnt
  db %1
align CELL_SIZE, db 0
  dd %2
const_%2:
  dd %3
[section .text]
%2:
  PUSHPSP [const_%2]
  ret
%xdefine link cell_%2
%endmacro

%define OPCODE_PUSH 0x68
%define OPCODE_CALL 0xe8
%define OPCODE_RET  0xc3
%define OPCODE_NOP  0x90

%define SIZE_CONTEXT 16

%define BLOCK_BUFFERS 32
%define BLOCK_LINES 16
%define BLOCK_LINE_LENGTH 64
%define BLOCK_SIZE BLOCK_LINES*BLOCK_LINE_LENGTH

extern getchar
extern putchar
extern console_write
extern getline
extern hexdump

extern malloc
extern free
extern realloc

extern ata_lba_write
extern ata_lba_read

global main

struc _INPUT_SOURCE
.type   resd 1
.count  resd 1
.buffer resd 1
endstruc

%define INPUT_SOURCE(Y) [input_source + _INPUT_SOURCE. %+ Y]

%define INPUT_TYPE_STRING   0
%define INPUT_TYPE_KEYBOARD 1
%define INPUT_TYPE_BLOCK    2

[section .text]

main:
  cld                           ; clear decrement flag
  xor ebp, ebp                  ; zero out base pointer
  mov PSP, [var_S0]
  add PSP, CELL_SIZE            ; first push hits the register
  call VERSION
  PUSHPSP version.end - version
  call TYPE
  call QUIT                     ; run interpreter
.spin:
  jmp .spin                     ; should never return

;; nucleus words

;; ( n -- )
DEFCODE 'DROP',DROP
  _DROP
  ret

;; ( n1 n2 -- n2 n1 )
DEFCODE 'SWAP',SWAP
  _SWAP
  ret

;; ( x -- x x )
DEFCODE 'DUP',DUP
  _DUP
  ret

;; ( -- )
DEFCODE 'OVER',OVER
  _OVER
  ret

;; ( x1 x2 x3 -- x2 x3 x1 )
DEFCODE 'ROT',ROT
  mov ebx, [PSP]
  mov ecx, [PSP+CELL_SIZE]
  mov [PSP+CELL_SIZE], ebx
  mov [PSP], TOS
  mov TOS, ecx
  ret

;; ( xu ... x1 x0 u -- xu ... x1 x0 xu )
DEFCODE 'PICK',PICK
  mov TOS, [PSP+TOS*CELL_SIZE]
  ret

;; ( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
DEFCODE 'ROLL',ROLL
  test TOS, TOS
  jz .zero
  cmp dword TOS, 1
  je .one
  mov ebx, [PSP+TOS]
  mov ecx, TOS
  test ecx, ecx
  mov TOS, ebx
  ret
.zero:
  _DROP
  ret
.one:
  _DROP
  ret

;; ( x1 x2 -- x2 )
DEFCODE 'NIP',NIP
  lea PSP, [PSP+CELL_SIZE]
  ret

DEFCODE 'TUCK',TUCK
  _SWAP
  _OVER
  ret

;; ( n1 n2 -- )
DEFCODE '2DROP',TWODROP
  _DROP
  _DROP
  ret

;; ( n1 n2 -- n1 n2 n1 n2 )
DEFCODE '2DUP',TWODUP
  mov ebx, [PSP]
  sub PSP, 2*CELL_SIZE
  mov [PSP+CELL_SIZE], TOS
  mov [PSP], ebx
  ret

;; ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
DEFCODE '2SWAP',TWOSWAP
  mov ebx, [PSP]
  mov ecx, [PSP+CELL_SIZE]
  mov edx, [PSP+2*CELL_SIZE]
  mov [PSP+2*CELL_SIZE], ebx
  mov [PSP+CELL_SIZE], TOS
  mov [PSP], edx
  mov TOS, ecx
  ret

;; ( x -- 0 | x x )
DEFCODE '?DUP',QDUP
  test TOS, TOS
  jz .no_dup
  _DUP
.no_dup:
  ret

DEFCODE '1+',INCR
  inc TOS
  ret

DEFCODE '1-',DECR
  dec TOS
  ret

DEFCODE 'CELL+',CELLPLUS
  add dword TOS, CELL_SIZE
  ret

DEFCODE 'CELLS',CELLS
  imul TOS, CELL_SIZE
  ret

DEFCODE 'CHAR+',CHARPLUS
  inc TOS
  ret

DEFCODE 'CHARS',CHARS
  ;; no-op
  ret

;; ( n1 n2 -- n1+n2 )
DEFCODE '+',ADD
  add TOS, [PSP]
  add PSP, CELL_SIZE
  ret

;; ( n1 n2 -- n1-n2 )
DEFCODE '-',SUB
  mov ebx, [PSP]
  sub ebx, TOS
  mov TOS, ebx
  add PSP, CELL_SIZE
  ret

;; ( n1 n2 -- n1*n2 )
DEFCODE '*',MUL
  imul TOS, [PSP]
  add PSP, CELL_SIZE
  ret

;; ( n1 n2 -- n3 n4 )
DEFCODE '/MOD',DIVMOD
  xor edx, edx
  mov ebx, TOS
  mov TOS, [PSP]
  idiv ebx
  mov [PSP], edx
  ret

;; ( n1 n2 -- n3 )
DEFCODE '/',DIV
  call DIVMOD
  _SWAP
  _DROP
  ret

;; ( n1 n2 -- n3 )
DEFCODE 'MOD',MOD
  call DIVMOD
  _DROP
  ret

DEFCODE '2*',TWOSTAR
  shl TOS, 1
  ret

DEFCODE '2/',TWOSLASH
  shr TOS, 1
  ret

DEFCONST 'TRUE',TRUE,1

DEFCONST 'FALSE',FALSE,0

DEFCODE '=',EQU
  mov ebx, [PSP]
  cmp TOS, ebx
  sete al
  movzx TOS, al
  lea PSP, [PSP+CELL_SIZE]
  ret

DEFCODE '<>',NEQU
  mov ebx, [PSP]
  cmp TOS, ebx
  setne al
  movzx TOS, al
  lea PSP, [PSP+CELL_SIZE]
  ret

DEFCODE '<',LT
  mov ebx, [PSP]
  cmp ebx, TOS
  setl al
  movzx TOS, al
  lea PSP, [PSP+CELL_SIZE]
  ret

DEFCODE '>',GT
  mov ebx, [PSP]
  cmp ebx, TOS
  setg al
  movzx TOS, al
  lea PSP, [PSP+CELL_SIZE]
  ret

DEFCODE '<=',LTE
  mov ebx, [PSP]
  cmp ebx, TOS
  setle al
  movzx TOS, al
  lea PSP, [PSP+CELL_SIZE]
  ret

DEFCODE '>=',GTE
  mov ebx, [PSP]
  cmp ebx, TOS
  setge al
  movzx TOS, al
  lea PSP, [PSP+CELL_SIZE]
  ret

DEFCODE '0=',ZEQU
  test TOS, TOS
  setz al
  movzx TOS, al
  ret

DEFCODE '0<>',ZNEQU
  test TOS, TOS
  setnz al
  movzx TOS, al
  ret

DEFCODE '0<',ZLT
  test TOS, TOS
  setl al
  movzx TOS, al
  ret

DEFCODE '0>',ZGT
  test TOS, TOS
  setg al
  movzx TOS, al
  ret

DEFCODE '0<=',ZLE
  test TOS, TOS
  setle al
  movzx TOS, al
  ret

DEFCODE '0>=',ZGE
  test TOS, TOS
  setge al
  movzx TOS, al
  ret

;; ( n1 n2 -- n3 )
DEFCODE 'MAX',MAX
  cmp TOS, [PSP]
  cmovl TOS, [PSP]
  lea PSP, [PSP+CELL_SIZE]
  ret

;; ( n1 n2 -- n3 )
DEFCODE 'MIN',MIN
  cmp TOS, [PSP]
  cmovg TOS, [PSP]
  lea PSP, [PSP+CELL_SIZE]
  ret

DEFCODE 'ABS',_ABS
  mov ebx, TOS
  sar ebx, 31
  xor TOS, ebx
  sub TOS, ebx
  ret

;; ( x1 x2 -- x3 )
DEFCODE 'AND',AND
  and dword TOS, [PSP]
  add PSP, CELL_SIZE
  ret

;; ( x1 x2 -- x3 )
DEFCODE 'OR',OR
  or dword TOS, [PSP]
  add PSP, CELL_SIZE
  ret

DEFCODE 'XOR',XOR
  xor dword TOS, [PSP]
  add PSP, CELL_SIZE
  ret

DEFCODE 'LSHIFT',LSHIFT
  mov ecx, TOS
  mov TOS, [PSP]
  shl TOS, cl
  lea PSP, [PSP+CELL_SIZE]
  ret

DEFCODE 'RSHIFT',RSHIFT
  mov ecx, TOS
  mov TOS, [PSP]
  shr TOS, cl
  lea PSP, [PSP+CELL_SIZE]
  ret

;; ( u -- u )
DEFCODE 'NEGATE',NEGATE
  PUSHPSP 0
  _SWAP
  call SUB
  ret

;; ( u -- u )
DEFCODE 'INVERT',INVERT
  not dword TOS
  ret

;; ( u a -- )
DEFCODE '!',STORE
  mov ebx, [PSP]
  mov dword [TOS], ebx
  _DROP
  _DROP
  ret

DEFCODE '+!',PLUSSTORE
  mov ebx, [PSP]
  add dword [TOS], ebx
  _DROP
  _DROP
  ret

;; ( ud1 -- ud2 )
DEFCODE '#',NUMBER_SIGN
  mov ebx, [var_BASE]
  idiv ebx
  ret

;; ( c a -- )
DEFCODE 'C!',CSTORE
  mov ebx, [PSP]
  mov byte [TOS], bl
  _DROP
  _DROP
  ret

;; ( a -- u )
DEFCODE '@',FETCH
  mov dword TOS, [TOS]
  ret

;; ( a -- value )
DEFCODE 'C@',CFETCH
  xor ebx, ebx
  mov byte bl, [TOS]
  mov TOS, ebx
  ret

%macro BLOCK_MOVE 1
  push esi
  mov ecx, TOS
  mov edi, [PSP]
  mov esi, [PSP+CELL_SIZE]
  rep %1
  pop esi
  _DROP
  _DROP
  _DROP
%endmacro

DEFCODE 'MOVE',MOVE
  BLOCK_MOVE movsd
  ret

;; ( c-addr1 c-addr2 u -- )
DEFCODE 'CMOVE',CMOVE
  BLOCK_MOVE movsb
  ret

DEFCODE 'CMOVE>',CMOVEUP
  std
  BLOCK_MOVE movsb
  cld
  ret

;; ( c-addr u char -- )
DEFCODE 'FILL',FILL
  mov edi, [PSP+CELL_SIZE]
  mov ecx, [PSP]
  rep stosb
  _DROP
  _DROP
  _DROP
  ret

;; ( addr u -- )
DEFCODE 'ERASE',ERASE
  PUSHPSP 0
  call FILL
  ret

;; ( -- )
DEFCODE 'ALIGN',_ALIGN
  add dword [var_HERE], CELL_SIZE-1
  and dword [var_HERE], ~(CELL_SIZE-1)
  ret

;; ( addr -- a-addr )
DEFCODE 'ALIGNED',ALIGNED
  add TOS, CELL_SIZE-1
  and TOS, ~(CELL_SIZE-1)
  ret

;; ( n -- )
DEFCODE 'ALLOT',ALLOT
  add TOS, CELL_SIZE-1
  and TOS, ~(CELL_SIZE-1)
  add [var_HERE], TOS
  _DROP
  ret

;; ( -- )
DEFCODE 'COUNT',COUNT
  xor ebx, ebx
  mov byte bl, [TOS]
  lea TOS, [TOS+1]
  PUSHPSP ebx
  ret

;; ( x -- ) ( R: -- x )
DEFCODE '>R',TOR
  pop ebx
  push TOS
  _DROP
  push ebx
  ret

;; ( -- x ) (R: x -- )
DEFCODE 'R>',FROMR
  pop ebx
  _DUP
  pop TOS
  push ebx
  ret

;; ( -- x ) ( R: x -- x )
DEFCODE 'R@',RFETCH
  _DUP
  mov TOS, [RSP]
  ret

;; ( x1 x2 -- ) ( R: -- x1 x2 )
DEFCODE '2>R',TWOTOR
  pop ebx
  push TOS
  _DROP
  push TOS
  _DROP
  push ebx
  ret

DEFCODE '2R>',TWOFROMR
  pop ebx
  _DUP
  pop TOS
  _DUP
  pop TOS
  push ebx
  ret

DEFCODE '2R@',TWORFETCH
  _DUP
  _DUP
  mov ebx, [RSP+CELL_SIZE]
  mov [PSP], ebx
  mov TOS, [RSP]
  ret

;; ( i*x xt -- j*x )
DEFCODE 'EXECUTE',EXECUTE
  mov ebx, [TOS]
  _DROP
  call ebx
  ret

;; input/output

DEFVAR '>IN',TOIN,0

DEFVAR 'SOURCE-ID',SOURCEID,0

;; ( -- flag )
DEFCODE 'REFILL',REFILL
  mov ebx, INPUT_SOURCE(type)
  cmp ebx, INPUT_TYPE_STRING
  je .string
  cmp ebx, INPUT_TYPE_KEYBOARD
  je .keyboard
  cmp ebx, INPUT_TYPE_BLOCK
  je .block
.string:
  PUSHPSP 0
  ret
.keyboard:
  mov ebx, tib
  PUSHPSP ebx
  PUSHPSP TEXT_BUFFER_SIZE
;; not quite right, should check
;; to see if input is available
  call ACCEPT
  mov INPUT_SOURCE(count), TOS
  mov dword [var_TOIN], 0
  mov dword TOS, 1
.block:
  mov ebx, [var_BLK]
  inc ebx
  mov [var_BLK], ebx
  mov dword [var_TOIN], 0
  PUSHPSP 1
  ret

;; ( -- c-addr u )
DEFCODE 'SOURCE',SOURCE
  mov ebx, INPUT_SOURCE(buffer)
  PUSHPSP ebx
  mov ebx, INPUT_SOURCE(count)
  PUSHPSP ebx
  ret

;; ( c-addr +n1 -- +n2 )
DEFCODE 'ACCEPT',ACCEPT
  mov ebx, [PSP]
  mov [ebx], byte 0
  push TOS
  push ebx
  call getline
  add esp, 2*CELL_SIZE
  lea PSP, [PSP+CELL_SIZE]
  ret

;; ( -- char )
DEFCODE 'KEY',KEY
  _DUP
  call getchar
  ret

;; ( char -- )
DEFCODE 'EMIT',EMIT
  push TOS
  call putchar
  add esp, CELL_SIZE
  _DROP
  ret

;; ( c-addr u -- )
DEFCODE 'TYPE',TYPE
  push TOS
  mov ebx, [PSP]
  push ebx
  call console_write
  add esp, 2*CELL_SIZE
  _DROP
  _DROP
  ret

DEFCONST "'\\n'",NEWLINE,10
DEFCONST 'BL',BLANK,32

DEFCODE 'SPACE',SPACE
  call BLANK
  call EMIT
  ret

DEFCODE 'SPACES',SPACES
.loop:
  cmp TOS, 0
  jle .out
  call SPACE
  call DECR
  jmp .loop
.out:
  _DROP
  ret

DEFCODE 'CR',CR
  call NEWLINE
  call EMIT
  ret

;; ( -- xn ... x1 n )
DEFCODE 'SAVE-INPUT',SAVE_INPUT
  mov ebx, INPUT_SOURCE(type)
  cmp ebx, INPUT_TYPE_KEYBOARD
  je .keyboard
  cmp ebx, INPUT_TYPE_STRING
  je .string
  cmp ebx, INPUT_TYPE_BLOCK
  je .block
  jmp .error
.keyboard:
.string:
  mov ebx, [var_TOIN]
  PUSHPSP ebx
  PUSHPSP 1
  jmp .return
.block:
  mov ebx, [var_TOIN]
  PUSHPSP ebx
  mov ebx, [var_BLK]
  PUSHPSP ebx
  PUSHPSP 2
  jmp .return
.error:
  call ERROR
.return:
  ret

;; ( xn ... x1 n -- flag )
DEFCODE 'RESTORE-INPUT',RESTORE_INPUT
  mov ebx, INPUT_SOURCE(type)
  cmp ebx, INPUT_TYPE_KEYBOARD
  je .keyboard
  cmp ebx, INPUT_TYPE_STRING
  je .string
  cmp ebx, INPUT_TYPE_BLOCK
  je .block
;; error, return flag
  mov ecx, TOS
.drop_err:
  _DROP
  loop .drop_err
  mov TOS, 1
  ret
.keyboard:
.string:
  cmp TOS, 1
  jne .drop_err
  mov ebx, [PSP]
  _DROP
  mov TOS, 0
  ret
.block:
  cmp TOS, 2
  jne .drop_err
  mov ebx, [PSP]
  mov [var_BLK], ebx
  mov ebx, [PSP+CELL_SIZE]
  mov [var_TOIN], ebx
  _DROP
  _DROP
  mov TOS, 0
  ret

;; facility

DEFCODE 'AT-XY',AT_XY
  ret

DEFCODE 'KEY?',KEYQ
  ret

DEFCODE 'PAGE',PAGE
  ret

DEFCODE 'EKEY',EKEY
  ret

DEFCODE 'EKEY>CHAR',EKEYTOCHAR
  ret

DEFCODE 'EKEY?',EKEYQ
  ret

DEFCODE 'EMIT?',EMITQ
  ret

DEFCODE 'MS',MS
  ret

DEFCODE 'TIME&DATE',TIMEANDDATE
  ret

;; blocks

DEFVAR 'CURRENT-BLOCK',CURRENT_BLOCK,0,F_HIDDEN

DEFVAR 'BLK',BLK,0

;; ( bnum -- lba )
DEFCODE '>LBA',TOLBA,F_HIDDEN
  dec TOS
  shl TOS, 1
  ret

;; ( ix -- a-addr )
;; transform a buffer index to buffer address
DEFCODE '>BLOCK',TOBLOCK,F_HIDDEN
  shl TOS, 10
  lea TOS, [blocks+TOS]
  ret

;; ( u -- ix flag )
;; Block number to buffer index, set flag if
DEFCODE 'BUFFER-INDEX',BUFFER_INDEX,F_HIDDEN
  test TOS, TOS
  jz .error
  mov edi, block_buffers
  mov ecx, BLOCK_BUFFERS
 .loop:
  mov ebx, ecx
  dec ebx
  mov edx, [block_buffers+ebx*CELL_SIZE]
  and edx, F_BLK_NUMMASK
  cmp eax, edx
  je .found
  loop .loop                          ;
;; not found
  PUSHPSP 0
  ret
.found:
  mov TOS, ebx
  PUSHPSP 1
  ret
.error:
  call ERROR
.spin:
  jmp .spin

;; ( -- u 1 | 0 0 )
DEFCODE 'NEXT-EMPTY-BUFFER',NEXT_EMPTY_BUFFER,F_HIDDEN
  mov ecx, BLOCK_BUFFERS-1
.loop:
  mov ebx, [block_buffers+ecx*CELL_SIZE]
  test bl, bl
  jz .found
  loop .loop
;; not found
  PUSHPSP 0
  _DUP
  ret
.found:
  PUSHPSP ecx
  PUSHPSP 1
  ret

;;; ( u -- )
DEFCODE 'EVICT-BUFFER',EVICT_BUFFER,F_HIDDEN
  xor ebx, ebx
  mov [block_buffers+TOS*CELL_SIZE], ebx
  _DUP
  dec TOS
  call TOBLOCK
  mov edi, TOS
  call TOLBA
  xor ecx, ecx
  mov cl, 2
  call ata_lba_write
  _DROP
  ret

;; ( u -- flag )
DEFCODE '?UPDATED',QUPDATED,F_HIDDEN
  mov ebx, [block_buffers+TOS*CELL_SIZE]
  xor TOS, TOS
  test ebx, F_BLK_UPDATED
  setnz al
  ret

;; ( y -- a-addr flag )
DEFCODE 'FIND-BUFFER',FIND_BUFFER,F_HIDDEN
  _DUP
  call BUFFER_INDEX
  test TOS, TOS
  _DROP
  jnz .found_buffer
  _DROP
  call NEXT_EMPTY_BUFFER
  test TOS, TOS
  _DROP
  jnz .found_empty
  mov TOS, 0
  _DUP
  call QUPDATED
  test TOS, TOS
  jnz .found_empty
  call EVICT_BUFFER
  jmp .found_empty
.found_buffer:
  mov ebx, [PSP]
  mov [block_buffers+TOS*CELL_SIZE], ebx
  call TOBLOCK
  _SWAP
  mov TOS, 0
  ret
.found_empty:
  mov ebx, [PSP]
  mov [block_buffers+TOS*CELL_SIZE], ebx
  call TOBLOCK
  _SWAP
  mov TOS, 1
  ret

;; ( u -- a-addr )
DEFCODE 'BLOCK',BLOCK
  _DUP
  _DUP
  call CURRENT_BLOCK
  call STORE
  call FIND_BUFFER              ; ( u a-addr flag )
  test TOS, TOS                 ;
  jz .no_read
  _DROP                         ; ( u a-addr )
  mov edi, TOS
  _SWAP                         ; ( a-addr u )
  call TOLBA                    ; ( a-addr lba )
  xor ecx, ecx
  mov cl, 2
  call ata_lba_read
  _DROP                         ; ( a-addr )
  ret
.no_read:
  _DROP                         ; ( a-addr )
  ret

;; ( u -- a-addr )
DEFCODE 'BUFFER',BUFFER
  _DUP                          ; ( u u )
  call CURRENT_BLOCK            ; ( u u a )
  call STORE                    ; ( u )
  call FIND_BUFFER              ; ( a-addr flag )
  _DROP                         ; ( a-addr )
  ret

;; ( -- )
DEFCODE 'FLUSH',FLUSH
  call SAVE_BUFFERS
  call EMPTY_BUFFERS
  ret

;; ( -- )
DEFCODE 'SAVE-BUFFERS',SAVE_BUFFERS
  _DUP
  mov ecx, 0
  xor eax, eax
.loop:
  cmp ecx, 31
  jg .done
  mov eax, [block_buffers+ecx*CELL_SIZE]
  test eax, eax
  jz .next
  call TOLBA
  mov ebx, ecx
  shl ebx, 10
  lea edi, [blocks+ebx]
  push eax
  push ecx
  mov dword ecx, 2
  call ata_lba_write
  pop ecx
  pop eax
.next:
  inc ecx
  jmp .loop
.done:
  _DROP
  ret

;; ( -- )
DEFCODE 'UPDATE',UPDATE
  mov ebx, [var_CURRENT_BLOCK]
  test ebx, ebx
  jz .done
  PUSHPSP ebx
  call BUFFER_INDEX
  test TOS, TOS
  _DROP
  jz .drop
  lea ebx, [block_buffers+TOS*CELL_SIZE]
  or dword [ebx], F_BLK_UPDATED
.drop:
  _DROP
.done:
  ret

;; ( -- )
DEFCODE 'EMPTY-BUFFERS',EMPTY_BUFFERS
  _DUP
  xor eax, eax
  mov ecx, 32
  mov edi, block_buffers
  rep stosd
  _DROP
  ret

;; ( i*x u -- j*x )
DEFCODE 'LOAD',LOAD
  mov ebx, INPUT_SOURCE(type)
  push ebx
  mov ebx, INPUT_SOURCE(count)
  push ebx
  mov ebx, INPUT_SOURCE(buffer)
  push ebx
  mov ebx, [var_TOIN]
  push ebx
  _DUP
  call BLK
  call STORE
  call BLOCK
  mov INPUT_SOURCE(buffer), TOS
  _DROP
  mov dword [var_TOIN], 0
  mov dword INPUT_SOURCE(count), BLOCK_SIZE
  mov dword INPUT_SOURCE(type), INPUT_TYPE_BLOCK
  call INTERPRET
  pop ebx
  mov [var_TOIN], ebx
  pop ebx
  mov INPUT_SOURCE(buffer), ebx
  pop ebx
  mov INPUT_SOURCE(count), ebx
  pop ebx
  mov INPUT_SOURCE(type), ebx
  ret

DEFVAR 'SCR',SCR,0

;; ( u -- )
DEFCODE 'LIST',LIST
  _DUP
  call SCR
  call STORE
  _DUP
  call CURRENT_BLOCK
  call STORE
  call BLOCK
  mov ecx, 0
.loop:
  cmp ecx, BLOCK_LINES
  je .done
  push ecx
  _DUP
  PUSHPSP ecx
  call CR
  call UDOT
  call SPACE
  PUSHPSP BLOCK_LINE_LENGTH
  call TYPE
  pop ecx
  inc ecx
  add TOS, BLOCK_LINE_LENGTH
  jmp .loop
.done:
  _DROP
  _DROP
  ret

;; ( i*x u1 u2 -- j*x )
DEFCODE 'THRU',THRU
  mov ebx, TOS
  mov edx, [PSP]
  _DROP
  _DROP
.loop:
  cmp edx, ebx
  jg .done
  PUSHPSP edx
  push edx
  push ebx
  call LOAD
  pop ebx
  pop edx
  jmp .loop
.done:
  ret

DEFCONST 'VERSION',VERSION,version,F_HIDDEN
DEFVAR 'S0',S0,data_stack+DATA_STACK_SIZE
DEFVAR 'R0',R0,return_stack+RETURN_STACK_SIZE
DEFVAR 'STATE',STATE,0
DEFVAR 'HERE',HERE,data_space
DEFVAR 'COMPILE-HERE',COMPILE_HERE,code_space,F_HIDDEN

DEFVAR 'BASE',BASE,10

DEFCODE 'HEX',HEX
  mov dword [var_BASE], 16
  ret

DEFCODE 'DECIMAL',DECIMAL
  mov dword [var_BASE], 10
  ret

;; ( -- nt )
DEFCODE 'LATEST',LATEST
  mov ebx, [var_COMPILATION_WORDLIST]
  mov ebx, [ebx]
  lea ebx, [ebx+CELL_SIZE]
  PUSHPSP ebx
  ret

DEFCODE 'EXIT',EXIT,F_IMMED|F_COMP_ONLY
  push eax
  mov edi, [var_COMPILE_HERE]
  xor eax, eax
  mov al, OPCODE_RET
  stosb
  mov [var_COMPILE_HERE], edi
  pop eax
  ret

DEFCODE 'QUIT',QUIT
;; reset return stack and STATE
  mov RSP, [var_R0]
  mov dword [var_STATE], 0
.loop:
  mov ebx, tib
  mov dword INPUT_SOURCE(buffer), ebx
  mov dword INPUT_SOURCE(count), 0
  mov dword [var_SOURCEID], 0
  mov dword [var_TOIN], 0
  PUSHPSP ebx
  PUSHPSP TEXT_BUFFER_SIZE
  call ACCEPT
  mov dword INPUT_SOURCE(count), TOS
  _DROP
  call INTERPRET
  call PROMPT
  jmp .loop

;; ( i*x c-addr u -- j*x )
DEFCODE 'EVALUATE',EVALUATE
  mov dword [var_SOURCEID], -1
  mov dword [var_BLK], 0
  mov ebx, [PSP]
  mov dword INPUT_SOURCE(buffer), ebx
  mov dword INPUT_SOURCE(count), TOS
  _DROP
  _DROP
  call INTERPRET
  ret

DEFCODE 'PROMPT',PROMPT
  mov ebx, [var_STATE]
  test ebx, ebx
  jnz .compiling
  PUSHPSP ok
  PUSHPSP ok.end - ok
  jmp .type
.compiling:
  PUSHPSP compiled
  PUSHPSP compiled.end - compiled
.type:
  call TYPE
  ret

;; ( i*x -- j*x )
DEFCODE 'INTERPRET',INTERPRET,F_HIDDEN
  call PARSE_NAME
  test TOS, TOS
  jz .out
  call TWODUP
  call FIND
  test TOS, TOS
  jz .literal
  call TWOSWAP
  call TWODROP
  call INTERP_W0RD
  jmp INTERPRET
.literal:
  call TWODROP
  call INTERP_LITERAL
  jmp INTERPRET
.out:
  _DROP
  _DROP
  ret

INTERP_LITERAL:
  PUSHPSP 0
  xchg TOS, [PSP+CELL_SIZE]
  _SWAP
  call TONUMBER
  test TOS, TOS
  jnz .error
  _DROP
  _DROP
  mov ebx, [var_STATE]          ; check compile/interpret state
  test ebx, ebx
  jnz .compiling                ; literal already on top of stack
  ret
.compiling:
  call LITERAL                  ; compile literal into definition
  ret
.error:
  call ERROR
.spin:                          ; never returns
  jmp .spin

INTERP_W0RD:
  cmp dword TOS, 1              ; immediate
  je .execute                   ; just execute it
  mov edx, [var_STATE]          ; check compile/interpret state
  test edx, edx
  jnz .compiling
.execute:                       ; executing
  _DROP                         ; drop flag
  call EXECUTE                  ; execute
  ret                           ; continue interpreting
.compiling:                     ; compiling
  _DROP                         ; drop flag
  call COMPILE_COMMA            ; compile word into definition
  ret

;; ( i*x -- )
DEFCODE 'ABORT',ABORT
  mov PSP, [var_S0]
  xor TOS, TOS
  call QUIT

;; ( i*x -- )
DEFCODE 'ABORT"',ABORTQ,F_IMMED
  ret

DEFCODE 'CHAR',CHAR
  call PARSE_NAME
  _DROP
  xor ebx, ebx
  mov byte bl, [TOS]
  xor eax, eax
  movzx TOS, bl
  ret

DEFCODE `\\`,BACKSLASH,F_IMMED
  mov ebx, [var_BLK]
  test ebx, ebx
  jnz .block
  mov ebx, INPUT_SOURCE(count)
  mov [var_TOIN], ebx
  ret
.block:
  mov ebx, [var_TOIN]
  add ebx, BLOCK_LINE_LENGTH-1
  and ebx, ~(BLOCK_LINE_LENGTH-1)
  mov [var_TOIN], ebx
  ret

DEFCODE `(`,PAREN,F_IMMED
  PUSHPSP `)`
  call PARSE
  _DROP
  _DROP
  add dword [var_TOIN], 1       ; increment once more
  ret

;; ( char "ccc<char>" -- c-addr u )
DEFCODE 'PARSE',PARSE
  call SOURCE
  mov edx, TOS                  ; size of input buffer
  mov edi, [PSP]                ; start of input buffer
  _DROP
  _DROP
  mov ebx, [var_TOIN]
  add edi, ebx                  ; start of parse area
  sub edx, ebx                  ; max charse to parse
  xor ecx, ecx                  ; number of chars consumed
.loop:
  cmp ecx, edx
  jge .done
  mov byte bl, [edi+ecx]
  cmp bl, al
  je .done
  inc ecx
  jmp .loop
.done:
  _DROP
  PUSHPSP edi
  PUSHPSP ecx
  inc ecx
  add [var_TOIN], ecx
  ret

;; ( <spaces>name -- c-addr u )
DEFCODE 'PARSE-NAME',PARSE_NAME
  call SOURCE
  mov edx, TOS                  ; size of input buffer
  mov edi, [PSP]                ; start of input buffer
  _DROP
  _DROP
  mov ebx, [var_TOIN]
  add edi, ebx                  ; start of parse area
  sub edx, ebx                  ; max chars to parse
  xor ebx, ebx
  xor ecx, ecx
.consume_spaces:
  cmp ecx, edx
  jge .nothing
  mov byte bl, [edi+ecx]
  cmp bl, ` `
  ja .consume_chars             ; when space is delimiter, we can skip control chars
  inc ecx
  jmp .consume_spaces
.consume_chars:
  lea edi, [edi+ecx]
  add [var_TOIN], ecx           ; update >IN
  sub edx, ecx
  xor ecx, ecx
.loop:
  cmp ecx, edx
  jge .done
  mov byte bl, [edi+ecx]
  cmp bl, ` `
  jbe .done                     ; space or control char is delimiter
  inc ecx
  jmp .loop
.done:
  PUSHPSP edi
  PUSHPSP ecx
  inc ecx
  add [var_TOIN], ecx           ; update >IN
  ret
.nothing:
  PUSHPSP edi
  PUSHPSP 0

;; ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
DEFCODE '>NUMBER',TONUMBER
  mov ecx, TOS
  mov edi, [PSP]
  mov eax, [PSP+CELL_SIZE]
  test ecx, ecx
  jz .done

  xor ebx, ebx
  mov edx, [var_BASE]
  push ebx                      ; push zero on stack
  mov byte bl, [edi]
  inc edi
  cmp byte bl, `-`
  jnz .convert
  mov [esp], ebx                ; put <> 0 on stack, indicate negate
  dec ecx
  jnz .digits
  pop ebx                       ; error, restore stack
  mov dword ecx, 1
  dec edi
  jmp .done

.digits:
  imul eax, edx
  mov byte bl, [edi]
  inc edi

.convert:
  sub bl, `0`
  jb .finish
  cmp bl, 10
  jb .check_base
  sub bl, 17
  jb .finish
  add bl, 10

.check_base:
  cmp bl, dl
  jge .finish
  add eax, ebx
  dec ecx
  jnz .digits

.finish:
  pop ebx                       ; pop the stack, check for '-' (non-zero)
  test ebx, ebx
  jz .done
  neg eax

.done:
  mov [PSP+CELL_SIZE], eax
  mov [PSP], edi
  mov TOS, ecx
  ret

DEFCODE 'DEPTH',DEPTH
  mov ebx, PSP
  sub ebx, [var_S0]
  shr ebx, 2
  PUSHPSP ebx
  ret

DEFCODE 'UNUSED',UNUSED
  mov ebx, code_space
  sub ebx, [var_HERE]
  PUSHPSP ebx
  ret

DEFCONST 'PAD',PAD,pad

DEFCODE 'ERROR',ERROR
  mov ebx, error
  PUSHPSP ebx
  PUSHPSP error.end - error
  call TYPE
  call ABORT
.spin:
  jmp .spin

;; ( u -- width )
DEFCODE 'UWIDTH',UWIDTH
  call BASE
  call FETCH
  call DIV
  call QDUP
  test TOS, TOS
  _DROP
  jz .one
  call UWIDTH
  call INCR
  ret
.one:
  PUSHPSP 1
  ret

;; ( n width -- )
DEFCODE '.R',DOTR
  call SWAP
  call DUP
  call ZLT
  test TOS, TOS
  _DROP
  jz .else
  call NEGATE
  PUSHPSP 1
  call SWAP
  call ROT
  call DECR
  jmp .then
.else:
  PUSHPSP 0
  call SWAP
  call ROT
.then:
  call SWAP
  call DUP
  call UWIDTH
  call ROT
  call SWAP
  call SUB
  call SPACES
  call SWAP
  test TOS, TOS
  _DROP
  jz .pos
  PUSHPSP `-`
  call EMIT
.pos:
  call UDOT
  ret

DEFCODE '.',DOT
  call SPACE
  PUSHPSP 0
  call DOTR
  ret

DEFCODE 'U.',UDOT
  call BASE
  call FETCH
  call DIVMOD
  call QDUP
  test TOS, TOS
  _DROP
  jz .remainder
  call UDOT
.remainder:
  call DUP
  PUSHPSP 10
  call LT
  test TOS, TOS
  _DROP
  jz .hex
  PUSHPSP 48
  jmp .emit
.hex:
  PUSHPSP 10
  call SUB
  PUSHPSP 65
.emit:
  call ADD
  call EMIT
  ret

;; ( char "ccc<char>" -- a-addr u )
DEFCODE 'PARSE>DATA-SPACE',PARSE_DATA_SPACE,F_HIDDEN
  call PARSE                    ; c-addr u
  call HERE                     ; c-addr u a-addr
  call FETCH                    ; c-addr u a-addr
  call TWODUP                   ; c-addr u a-addr u a-addr
  call TOR                      ; c-addr u a-addr u
  call TOR                      ; c-addr u a-addr
  _SWAP                         ; c-addr a-addr u
  _DUP                          ; c-addr a-addr u u
  call ALLOT                    ; c-addr a-addr u
  call CMOVE                    ;
  call FROMR                    ; a-addr
  call FROMR                    ; a-addr u
  ret

DEFCODE 'S"',SQUOTE,F_IMMED|F_COMP_ONLY
  PUSHPSP `"`
  call PARSE_DATA_SPACE
  call LITERAL
  call LITERAL
  ret

;; ( char "ccc<char>" -- c-addr )
DEFCODE 'CPARSE>DATA-SPACE',CPARSE_DATA_SPACE,F_HIDDEN
  call PARSE                    ; c-addr u
  call HERE                     ; c-addr u a-addr
  call FETCH                    ; c-addr u a-addr
  call TWODUP                   ; c-addr u a-addr u a-addr
  call TOR                      ; c-addr u a-addr u
  call TOR                      ; c-addr u a-addr
  _SWAP                         ; c-addr a-addr u
  _DUP                          ; c-addr a-addr u u
  call INCR                     ; c-addr a-addr u u+1
  call ALLOT                    ; c-addr a-addr u
  _SWAP                         ; c-addr u a-addr
  call INCR                     ; c-addr u a-addr+1
  _SWAP                         ; c-addr a-addr+1 u
  call CMOVE                    ;
  call FROMR                    ; a-addr
  call FROMR                    ; a-addr u
  mov ebx, [PSP]
  mov byte [ebx], al
  _DROP                         ; c-addr
  ret

DEFCODE 'C"',CQUOTE,F_IMMED|F_COMP_ONLY
  PUSHPSP `"`
  call CPARSE_DATA_SPACE
  call LITERAL
  ret

DEFCODE '."',DOTQUOTE,F_IMMED|F_COMP_ONLY
  PUSHPSP `"`
  call PARSE_DATA_SPACE
  call LITERAL
  call LITERAL
  PUSHPSP cfa_TYPE
  call COMPILE_COMMA
  ret

;; ( "ccc<paren>" -- )
DEFCODE '.(',DOTPAREN,F_IMMED|F_COMP_ONLY
  PUSHPSP `)`
  call PARSE_DATA_SPACE
  call LITERAL
  call LITERAL
  PUSHPSP cfa_TYPE
  call COMPILE_COMMA
  ret

DEFCONST 'WORDLISTS',WORDLISTS,SIZE_CONTEXT

DEFVAR 'COMPILATION-WORDLIST',COMPILATION_WORDLIST,FORTH_WORDLIST,F_HIDDEN

;; ( -- wid )
DEFCODE 'GET-CURRENT',GET_CURRENT
  call COMPILATION_WORDLIST
  call FETCH
  ret

;; ( wid -- )
DEFCODE 'SET-CURRENT',SET_CURRENT
  call COMPILATION_WORDLIST
  call STORE
  ret

DEFVAR 'SEARCH-ORDER',SEARCH_ORDER,0,F_HIDDEN

;; ( -- widn ... wid1 n )
DEFCODE 'GET-ORDER',GET_ORDER
  call SEARCH_ORDER
  call FETCH
  mov ecx, TOS
  _DROP
.loop:
  mov edx, ecx
  dec edx
  mov ebx, [CONTEXT+edx*CELL_SIZE]
  PUSHPSP ebx
  loop .loop
  call SEARCH_ORDER
  call FETCH
  ret

;; ( widn ... wid1 n  -- )
DEFCODE 'SET-ORDER',SET_ORDER
  cmp TOS, -1
  jne .set
  _DROP
  PUSHPSP MINIMAL_WORDLIST
  PUSHPSP 1
.set:
  mov [var_SEARCH_ORDER], TOS
  mov ecx, TOS
  _DROP
.loop:
  mov edx, ecx
  dec edx
  mov [CONTEXT+edx*CELL_SIZE], TOS
  _DROP
  loop .loop
  ret

;; ( -- )
DEFCODE 'DEFINITIONS',DEFINITIONS
  _DUP
  mov TOS, [CONTEXT]
  call SET_CURRENT
  ret

;; ( -- wid )
DEFCODE 'WORDLIST',WORDLIST
  _DUP
  mov TOS, [var_HERE]
  add dword [var_HERE], CELL_SIZE
  ret

;; ( -- wid )
DEFCODE 'FORTH-WORDLIST',GET_FORTH_WORDLIST
  _DUP
  mov TOS, FORTH_WORDLIST
  ret

;; ( wid -- )
DEFCODE 'SET-FIRST-WORDLIST',SET_FIRST_WORDLIST,F_HIDDEN
  mov ebx, [var_SEARCH_ORDER]
  mov [CONTEXT+ebx*CELL_SIZE], TOS
  _DROP
  ret

;; ( -- )
DEFCODE 'ALSO',ALSO
  mov ebx, [var_SEARCH_ORDER]
  cmp ebx, SIZE_CONTEXT-1
  jg .error
  mov edx, [CONTEXT+ebx*CELL_SIZE]
  inc ebx
  mov [CONTEXT+ebx*CELL_SIZE], edx
  mov [var_SEARCH_ORDER], ebx
  ret
.error:
  call ERROR
.spin:
  jmp .spin

; ( -- )
DEFCODE 'FORTH',FORTH
  call GET_FORTH_WORDLIST
  call SET_FIRST_WORDLIST
  ret

;; ( -- )
DEFCODE 'ONLY',ONLY
  xor ebx, ebx
  mov [var_SEARCH_ORDER], ebx
  mov ebx, MINIMAL_WORDLIST
  mov dword [CONTEXT], ebx
  ret

;; ( -- )
DEFCODE 'ORDER',ORDER
  mov ebx, [var_SEARCH_ORDER]
  xor ecx, ecx
.loop:
  cmp ecx, ebx
  jg .done
  mov edx, [CONTEXT+ecx*CELL_SIZE]
  lea edx, [edx+CELL_SIZE]
  push ecx
  push ebx
  PUSHPSP edx
  call CR
  call COUNT
  call TYPE
  pop ebx
  pop ecx
  inc ecx
  jmp .loop
.done:
  ret

;; ( -- )
DEFCODE 'PREVIOUS',PREVIOUS
  mov ebx, [var_SEARCH_ORDER]
  test ebx, ebx
  jz .error
  dec ebx
  mov [var_SEARCH_ORDER], ebx
  ret
.error:
  call ERROR
.spin:
  jmp .spin

;; ( c-addr u wid -- c-addr 0  |  xt 1  |  xt -1 )
DEFCODE 'SEARCH-WORDLIST',SEARCH_WORDLIST
  push esi
  mov edx, TOS
  mov edx, [edx]
  mov ecx, [PSP]
  mov edi, [PSP+CELL_SIZE]

.loop:
  test edx, edx
  je .not_found

;; get word header
  mov ebx, [edx+CELL_SIZE]

;; check hidden flag
  xor eax, eax
  mov byte al, [ebx]
  and byte al, F_HIDDEN|F_LENMASK
  cmp al, cl
  jne .next

  mov eax, [var_STATE]
  test eax, eax
  jnz .compare_string           ; compiling

;; check compile-only
  xor eax, eax
  mov byte al, [ebx]
  and byte al, F_COMP_ONLY
  test al, al
  jnz .next                     ; compile only word

.compare_string:
;; compare strings
  push ecx
  push edi
  lea esi, [ebx+1]
  repe cmpsb
  pop edi
  pop ecx
  jne .next

;; found
  pop esi
  add PSP, CELL_SIZE
  mov TOS, ebx
  push ebx
  call TOCFA
  pop ebx
  mov [PSP], TOS
  mov dword TOS, -1
  mov dword ecx, 1
  mov byte bl, [ebx]
  and bl, F_IMMED
  test bl, bl
  cmovnz TOS, ecx
  ret

.next:
  ;; next link
  mov edx, [edx]
  jmp .loop

.not_found:
  pop esi
  add PSP, CELL_SIZE
  mov [PSP], edi
  mov dword TOS, 0
  ret

DEFCODE '?HIDDEN',QHIDDEN
  xor ebx, ebx
  mov byte bl, [TOS]
  and bl, F_HIDDEN
  PUSHPSP ebx
  ret

DEFCODE 'TYPE-WORD-NAME',TYPE_WORD_NAME,F_HIDDEN
  call QHIDDEN

  call NAMETOSTRING
  call CR
  call TYPE
.next:
  call TRUE
  ret

;; ( wid -- )
DEFCODE 'WORDLIST-WORDS',WORDLIST_WORDS,F_HIDDEN
  mov edx, TOS
  mov edx, [edx]
.loop:
  test edx, edx
  jz .done

  mov ebx, [edx+CELL_SIZE]

;; check hidden
  xor ecx, ecx
  mov byte cl, [ebx]
  and cl, F_HIDDEN
  test cl, cl
  jnz .next

;; push string
  lea ecx, [ebx+1]
  PUSHPSP ecx

;; push length
  xor ecx, ecx
  mov byte cl, [ebx]
  and cl, F_LENMASK
  PUSHPSP ecx

;; call TYPE
  push ebx
  push edx
  call CR
  call TYPE
  pop edx
  pop ebx

.next:
  mov edx, [edx]
  jmp .loop

.done:
  _DROP
  ret

DEFCODE 'WORDS',WORDS
  mov ecx, [var_SEARCH_ORDER]
  mov edx, [CONTEXT]
.loop:
  PUSHPSP edx
  push ebx
  push ecx
  push edx
  call WORDLIST_WORDS
  pop edx
  pop ecx
  pop edx
  mov edx, [CONTEXT+ecx*CELL_SIZE]
  loopnz .loop
.done:
  ret

;; ( c-addr u -- c-addr 0  |  xt 1  |  xt -1 )
DEFCODE 'FIND',FIND
  xor ecx, ecx
  mov ebx, [var_SEARCH_ORDER]
  mov edx, [CONTEXT]
.loop:
  cmp ecx, ebx
  jg .not_found
  push ebx
  push ecx
  push edx
  call TWODUP
  PUSHPSP edx
  call SEARCH_WORDLIST
  pop edx
  pop ecx
  pop ebx
  test TOS, TOS
  jnz .found
.next:
  _DROP
  _DROP
  inc ecx
  mov edx, [CONTEXT+ecx*CELL_SIZE]
  jmp .loop
.found:
  mov ebx, TOS
  mov ecx, [PSP]
  _DROP
  _DROP
  mov TOS, ebx
  mov [PSP], ecx
  ret
.not_found:
  mov TOS, 0
  ret

;; ( header -- xt )
DEFCODE '>CFA',TOCFA
  xor ebx, ebx
  mov byte bl, [TOS]
  inc TOS
  and byte bl, F_LENMASK
  add TOS, ebx
  add TOS, dword CELL_SIZE-1
  and TOS, dword ~(CELL_SIZE-1)
  ret

;; ( xt -- a-addr )
DEFCODE '>BODY',TOBODY
  add TOS, CELL_SIZE
  ret

;; ( "<spaces>name" -- )
DEFCODE 'CREATE',CREATE
;; read a word from input
  call PARSE_NAME
  mov ecx, TOS
  mov ebx, [PSP]

;; extend the compilation wordlist
;; with a 'cons' cell
  mov edi, [var_HERE]
  mov eax, [var_COMPILATION_WORDLIST]
  mov eax, [eax]
  stosd
  mov eax, edi
  add eax, CELL_SIZE
  stosd

;; store the name
  mov al, cl
  stosb
  push esi
  mov esi, ebx
  rep movsb
  pop esi

;; align to cell size
  add edi, (CELL_SIZE-1)
  and edi, ~(CELL_SIZE-1)

;; store code field
  mov eax, [var_COMPILE_HERE]
  stosd

;; update LATEST and HERE
  mov eax, [var_HERE]
;; head of current wordlist points to the new cell
  mov ebx, [var_COMPILATION_WORDLIST]
  mov [ebx], eax
  mov [var_HERE], edi

;; compile the default execution semantics:
;; pushing the address of the data field (in edi)
;; onto the stack
  PUSHPSP edi
  call LITERAL
  call COMPILE_EXIT
  _DROP
  _DROP
  ret

;; ( x -- )
DEFCODE ',',COMMA
  mov edi, [var_HERE]
  stosd
  mov [var_HERE], edi
  _DROP
  ret

;; ( char -- )
DEFCODE 'C,',CCOMMA
  mov edi, [var_HERE]
  stosb
  mov [var_HERE], edi
  _DROP
  ret

;; ( x -- )
;; 83 ee 04 : sub PSP, CELL_SIZE
;; 89 06    : mov [PSP], TOS
;; b8 xx xx xx xx : mov TOS, xxxxxxxx
DEFCODE 'LITERAL',LITERAL,F_IMMED|F_COMP_ONLY
  mov ebx, TOS
  xor eax, eax
  mov edi, [var_COMPILE_HERE]
  mov al, 0x83
  stosb
  mov al, 0xee
  stosb
  mov al, 0x04
  stosb
  mov al, 0x89
  stosb
  mov al, 0x06
  stosb
  mov al, 0xb8
  stosb
  mov eax, ebx
  stosd
  mov [var_COMPILE_HERE], edi
  _DROP
  ret

;; ( xt -- )
DEFCODE 'COMPILE,',COMPILE_COMMA,F_COMP_ONLY
  mov ebx, [TOS]
  xor eax, eax
  mov al, OPCODE_CALL
  mov edi, [var_COMPILE_HERE]
  stosb                         ; store call opcode
  sub ebx, edi
  sub ebx, 4
  mov eax, ebx
  stosd                         ; store code address
  mov [var_COMPILE_HERE], edi   ; update compile_here
  _DROP
  ret

DEFCODE 'DOES>',DOES,F_IMMED|F_COMP_ONLY
  PUSHPSP cfa_PCODE
  call COMPILE_COMMA
  ret

;; ( -- ) ( R: addr -- )
DEFCODE '(;CODE)',PCODE
;; reset compile pointer
  call LATEST
  call FETCH
  _DUP
  call TOCFA
  call FETCH
  call COMPILE_HERE
  call STORE
;; compile push pfa
  call TOCFA
  call TOBODY
  call LITERAL
;; compile call to the subsequent action
  _DUP
;; put pointer to pointer to action in TOS
  mov TOS, esp
  call COMPILE_COMMA
;; compile a return
  call COMPILE_EXIT
;; drop action address
  add esp, CELL_SIZE
  ret

DEFCODE '[',LBRAC,F_IMMED
  xor ebx, ebx
  mov [var_STATE], ebx
  ret

DEFCODE ']',RBRAC
  mov dword [var_STATE], 1
  ret

DEFCODE ':',COLON
;; create a word
  call CREATE
;; and make it hidden
  call HIDDEN
  call LATEST
  call FETCH
;; set to replace default CREATE semantics
  call TOCFA
  call FETCH
  call COMPILE_HERE
  call STORE
;; switch to compile mode
  call RBRAC
  ret

COMPILE_EXIT:
  push eax
  mov dword eax, OPCODE_RET
  mov edi, [var_COMPILE_HERE]
  stosb
  mov [var_COMPILE_HERE], edi
  pop eax
  ret

DEFCODE ';',SEMICOLON,F_IMMED|F_COMP_ONLY
  call COMPILE_EXIT
  call HIDDEN
  call LBRAC
  ret

DEFCODE ':NONAME',COLON_NONAME
  call HERE
  call FETCH                    ; xt on the stack
  call DUP
  call CELLPLUS
  call HERE
  call STORE
  call COMPILE_HERE
  call FETCH
  call COMMA                    ; store 'code field'
  call RBRAC
  ret

; ( -- )
DEFCODE 'IMMEDIATE',IMMEDIATE,F_IMMED
  call LATEST
  call FETCH
  xor byte [TOS], F_IMMED
  ret

;; ( -- )
DEFCODE 'COMPILE-ONLY',COMPILE_ONLY
  call LATEST
  call FETCH
  xor byte [TOS], F_COMP_ONLY
  ret

;; ( -- )
DEFCODE 'HIDDEN',HIDDEN
  call LATEST
  call FETCH
  xor byte [TOS], F_HIDDEN
  ret

DEFCODE 'HIDE',HIDE
  call PARSE_NAME
  _DROP
  _DROP
  ret

DEFCODE "'",TICK
  xor edx, edx
  mov dl, ` `
  PUSHPSP edx
  call PARSE_NAME
  call FIND
  test TOS, TOS
  jz .not_found
  _DROP
  ret
.not_found:
  call ABORT
.spin:                          ; never returns
  jmp .spin

DEFCODE "[']",BRACKET_TICK,F_IMMED
  xor edx, edx
  mov dl, ` `
  PUSHPSP edx
  call PARSE_NAME
  call FIND
  test TOS, TOS
  jz .error
  _DROP
  call LITERAL
  ret
.error:
  call ERROR
.spin:                          ; never returns
  jmp .spin

;; 85 c0 : test TOS, TOS
;; ad    : lodsd         ( DROP )
;; 74 00 : jz 2
;; eb 00 : jmp 2
DEFCODE 'IF',IF,F_IMMED|F_COMP_ONLY
  push eax
  xor eax, eax
  mov edi, [var_COMPILE_HERE]
  mov al, 0x85
  stosb
  mov al, 0xc0
  stosb
  mov al, 0xad
  stosb
  pop eax
;; control flow info is pointer to reserved room for branch
;; and opcode of jump
  PUSHPSP edi
  PUSHPSP 0x74
  add edi, 2                    ; reserve room for branch
  mov [var_COMPILE_HERE], edi   ; continue compiling after branch
  ret

DEFCODE 'ELSE',ELSE,F_IMMED|F_COMP_ONLY
  mov edi, [PSP]
  stosb
  mov eax, [var_COMPILE_HERE]
  sub eax, [PSP]
  stosb
  mov ebx, [var_COMPILE_HERE]
  mov [PSP], ebx
  add dword ebx, 2
  mov [var_COMPILE_HERE], ebx
  mov dword TOS, 0xeb         ;
  ret

DEFCODE 'THEN',THEN,F_IMMED|F_COMP_ONLY
  mov edi, [PSP]
  stosb
  mov eax, [var_COMPILE_HERE]
  sub eax, [PSP]
  sub al, 2
  stosb
  _DROP                         ; remove control flow info from stack
  _DROP
  ret


;; 8b 1e       : mov ebx, [PSP]
;; 53          : push ebx
;; 50          : push TOS
;; ad          : _DROP
;; ad          : _DROP
;; 8b 0c 24    : mov ecx, [esp]  <-\
;; 8b 54 24 04 : mov edx, [esp+4]  |
;; 39 d1       : cmp ecx, edx      |
;; 7d 00       : jge 2 --------\   |
;; [ loop body ]               |   |
;; eb 00       : jmp 2 --------|---/
;; 83 c4 08    : add esp, 8  <-/
DEFCODE 'DO',DO,F_IMMED|F_COMP_ONLY
  push eax
  mov edi, [var_COMPILE_HERE]
  xor eax, eax
  mov al, 0x8b
  stosb
  mov al, 0x1e
  stosb
  mov al, 0x53
  stosb
  mov al, 0x50
  stosb
  mov al, 0xad
  stosb
  stosb
  mov ebx, edi                  ; keep pointer back to top of loop
  mov al, 0x8b
  stosb
  mov al, 0x0c
  stosb
  mov al, 0x24
  stosb
  mov al, 0x8b
  stosb
  mov al, 0x54
  stosb
  mov al, 0x24
  stosb
  mov al, 0x04
  stosb
  mov al, 0x39
  stosb
  mov al, 0xd1
  stosb
  pop eax
  PUSHPSP edi
  PUSHPSP ebx
  add edi, 2                    ; reserve room for jge
  mov [var_COMPILE_HERE], edi
  ret

;; 8b 1c 24 : mov ebx, [esp]
;; 83 ee 04 : sub PSP, 4
;; 89 06    : mov [PSP], TOS
;; 89 d8    : mov TOS, ebx
DEFCODE 'I',I,F_IMMED|F_COMP_ONLY
  push eax
  xor eax, eax
  mov edi, [var_COMPILE_HERE]
  mov al, 0x8b
  stosb
  mov al, 0x1c
  stosb
  mov al, 0x24
  stosb
  mov al, 0x83
  stosb
  mov al, 0xee
  stosb
  mov al, 0x04
  stosb
  mov al, 0x89
  stosb
  mov al, 0x06
  stosb
  mov al, 0x89
  stosb
  mov al, 0xd8
  stosb
  mov [var_COMPILE_HERE], edi
  pop eax
  ret

;; 8b 5c 24 0c : mov ebx, [esp+12]
;; 83 ee 04    : sub PSP, 4
;; 89 06       : mov [PSP], TOS
;; 89 d8       : mov TOS, ebx
DEFCODE 'J',J,F_IMMED|F_COMP_ONLY
  push eax
  mov edi, [var_COMPILE_HERE]
  xor eax, eax
  mov al, 0x8b
  stosb
  mov al, 0x5c
  stosb
  mov al, 0x24
  stosb
  mov al, 0x0c
  stosb
  mov al, 0x83
  stosb
  mov al, 0xee
  stosb
  mov al, 0x04
  stosb
  mov al, 0x89
  stosb
  mov al, 0x06
  stosb
  mov al, 0x89
  stosb
  mov al, 0xd8
  stosb
  mov [var_COMPILE_HERE], edi
  pop eax
  ret

;; ff 04 24    : inc dword [esp]
;; eb 00       : jmp 2
;; 83 c4 08    : add esp, 8

DEFCODE 'LOOP',LOOP,F_IMMED|F_COMP_ONLY
;; TOS contains loop top
;; NOS contains pointer to patch
  mov ebx, TOS
  mov edi, [var_COMPILE_HERE]
  xor eax, eax
  mov al, 0xff
  stosb
  mov al, 0x04
  stosb
  mov al, 0x24
  stosb
  sub ebx, edi
  sub bl, 2
;; compile jmp to loop top
  mov al, 0xeb
  stosb
  mov al, bl
  stosb
;; save edi for jge destination
  push edi
;; compile postlude
  mov al, 0x83
  stosb
  mov al, 0xc4
  stosb
  mov al, 0x08
  stosb
;; restore jge destination
  pop ebx
;; save current compile pointer
  mov [var_COMPILE_HERE], edi
  mov edi, [PSP]
;; compute offset
  sub ebx, edi
  sub bl, 2
;; compile jge
  mov al, 0x7d
  stosb
  mov al, bl
  stosb
  _DROP
  _DROP
  ret

DEFCODE 'BEGIN',BEGIN,F_IMMED|F_COMP_ONLY
  mov ebx, [var_COMPILE_HERE]
  PUSHPSP ebx
  ret

DEFCODE 'AGAIN',AGAIN,F_IMMED|F_COMP_ONLY
  mov ebx, TOS
  xor eax, eax
  mov edi, [var_COMPILE_HERE]
  sub ebx, edi
  sub bl, 2
  mov al, 0xeb
  stosb
  mov al, bl
  stosb
  mov [var_COMPILE_HERE], edi
  _DROP
  ret

DEFCODE 'WHILE',WHILE,F_IMMED|F_COMP_ONLY
  mov ebx, TOS
  xor eax, eax
  mov edi, [var_COMPILE_HERE]
;; 85 c0 : test TOS, TOS
;; ad    : lodsd         ( DROP )
  mov al, 0x85
  stosb
  mov al, 0xc0
  stosb
  mov al, 0xad
  stosb
  mov TOS, edi
  PUSHPSP ebx
  add edi, 2                    ; reserve room
  mov [var_COMPILE_HERE], edi
  ret

DEFCODE 'REPEAT',REPEAT,F_IMMED|F_COMP_ONLY
;; compile backward jmp to dest
  mov ebx, TOS
  xor eax, eax
  mov edi, [var_COMPILE_HERE]
  ;; eb 00 : jmp 2
  sub ebx, edi
  sub bl, 2
  mov al, 0xeb
  stosb
  mov al, bl
  stosb
  mov [var_COMPILE_HERE], edi
;; compile forward jz from origin
  mov ebx, edi
  sub ebx, [PSP]
  sub bl, 2
  mov edi, [PSP]
  mov al, 0x74
  stosb
  mov al, bl
  stosb
  _DROP
  _DROP
  ret

DEFCODE 'UNTIL',UNTIL,F_IMMED|F_COMP_ONLY
  mov ebx, TOS
  xor eax, eax
  mov edi, [var_COMPILE_HERE]
;; 85 c0 : test eax, eax
;; ad    : lodsd (DROP)
;; 74 00 : jz 2
  mov al, 0x85
  stosb
  mov al, 0xc0
  stosb
  mov al, 0xad
  stosb
  sub ebx, edi
  mov al, 0x74
  stosb
  mov al, bl
  stosb
  mov [var_COMPILE_HERE], edi
  _DROP
  ret

DEFCODE 'RECURSE',RECURSE,F_IMMED|F_COMP_ONLY
  call LATEST
  call FETCH
  call TOCFA
  call COMPILE_COMMA
  ret

%define LEAVEMAGIC 0xdead

;; ( exit -- )
RESOLVE_LEAVES:
  call LATEST
  call TOCFA
  call FETCH
  mov edx, [PSP]
  mov edi, edx
  xor eax, eax
.loop:
  cmp edi, edx
  je .end
  mov ax, [edi]
  cmp ax, LEAVEMAGIC
  je .patch
  inc edi
  jmp .loop
.patch:
  push ebx
  sub ebx, edi
  mov al, 0xeb
  stosb
  mov al, bl
  stosb
  pop ebx
  inc edi
  jmp .loop
.end:
  _DROP
  _DROP
  _DROP
  ret

DEFCODE 'LEAVE',LEAVE,F_IMMED|F_COMP_ONLY
  push eax
  mov edi, [var_COMPILE_HERE]
  mov ax, LEAVEMAGIC
  stosw
  mov [var_COMPILE_HERE], edi
  pop eax
  ret

DEFCODE 'UNLOOP',UNLOOP
  pop ebx                       ; take return address
  add esp, 8                    ; eliminate loop variables
  push ebx                      ; restore return address
  ret

DEFCODE '+LOOP',PLUSLOOP
  ret

DEFCODE '[COMPILE]',BRACKET_COMPILE,F_IMMED|F_COMP_ONLY
  call PARSE_NAME
  call FIND
  call TOCFA
  call COMPILE_COMMA
  ret

;; ( x "name" -- )
DEFCODE 'VARIABLE',VARIABLE
  call CREATE
  call COMMA
  ret

_RESET_COMPILE:
  call LATEST
  call FETCH
  call DUP

  call TOCFA
  call FETCH
  call COMPILE_HERE
  call STORE

  call TOBODY
  call LITERAL
  ret

;; ( x "name" -- )
DEFCODE 'CONSTANT',CONSTANT
  call CREATE
  call COMMA

  call COMPILE_HERE
  call FETCH
  call DECR
  call COMPILE_HERE
  call STORE

  PUSHPSP cfa_FETCH
  call COMPILE_COMMA
  call COMPILE_EXIT
  ret

DEFCODE 'VALUE',VALUE
  call CREATE
  call COMMA

  call COMPILE_HERE
  call FETCH
  call DECR
  call COMPILE_HERE
  call STORE

  mov ebx, cfa_FETCH
  PUSHPSP ebx
  call COMPILE_COMMA
  call COMPILE_EXIT
  ret

DEFCODE 'TO',T0,F_IMMED
  call PARSE_NAME
  call FIND
  test TOS, TOS
  _DROP
  jz .not_found
  call TOBODY
  mov ebx, [PSP]
  mov dword [TOS], ebx
.not_found:
  _DROP
  ret

DEFCODE '[CHAR]',BRACKET_CHAR,F_IMMED|F_COMP_ONLY
  call PARSE_NAME
  test TOS, TOS
  jz .error
  _DROP
  mov byte al, [TOS]
  call LITERAL
  ret
.error:
  call ERROR
.spin:                          ; never returns
  jmp .spin

;; ( "<spaces>name" -- )
DEFCODE 'POSTPONE',POSTPONE,F_IMMED|F_COMP_ONLY
  call PARSE_NAME
  call FIND
  test TOS, TOS
  jz .error
  _DROP
  call COMPILE_COMMA
  ret
.error:
  call ERROR
.spin:
  jmp .spin

;; memory

;; ( u -- a-addr ior )
DEFCODE 'ALLOCATE',ALLOCATE
  push TOS
  call malloc
  add esp, CELL_SIZE
  test TOS, TOS
  jz .lose
  PUSHPSP 0
  ret
.lose:
  PUSHPSP 1
  ret

;; ( a-addr -- ior )
DEFCODE 'FREE',FREE
  push TOS
  call free
  add esp, CELL_SIZE
  PUSHPSP 0
  ret

;; ( a-addr1 u -- a-addr2 ior )
DEFCODE 'RESIZE',RESIZE
  push eax
  mov ebx, [PSP]
  push ebx
  call realloc
  add esp, 2*CELL_SIZE
  test TOS, TOS
  jz .lose
  PUSHPSP 0
  ret
.lose:
  PUSHPSP 1
  ret

;; tools

DEFCODE '?',Q
  call FETCH
  call DOT
  ret

DEFCODE 'DUMP',DUMP
  PUSHPSP crlf
  PUSHPSP 2
  call TYPE
  push TOS
  mov ebx, [PSP]
  push ebx
  call hexdump
  add esp, CELL_SIZE*2
  _DROP
  _DROP
  ret

DEFCODE '.S',DOTS
  _DUP
  call SPACE
  mov ebx, PSP
.loop:
  cmp ebx, [var_S0]
  jge .out
  push ebx
  mov ebx, [ebx]
  PUSHPSP ebx
  call UDOT
  call SPACE
  pop ebx
  add ebx, CELL_SIZE
  jmp .loop
.out:
  _DROP
  ret

;; ( i * n +n -- ) ( R: -- j * x +n )
DEFCODE 'N>R',NTOR
  mov ecx, TOS
  mov edx, ecx
  _DROP
.loop:
  push TOS
  _DROP
  loop .loop
  push edx
  ret

DEFCODE 'NR>',NFROMR
  pop ecx
  mov edx, ecx
.loop:
  pop ebx
  PUSHPSP ebx
  loop .loop
  PUSHPSP edx
  ret

DEFCODE 'NAME>COMPILE',NAMETOCOMPILE
  call TOCFA
  ret

DEFCODE 'NAME>INTERPRET',NAMETOINTERPRET
  call TOCFA
  ret

;; ( nt -- c-addr u )
DEFCODE 'NAME>STRING',NAMETOSTRING
  xor ebx, ebx
  mov byte bl, [TOS]
  inc TOS
  _DUP
  mov TOS, ebx
  ret

DEFCODE 'AHEAD',AHEAD
  mov ebx, [var_COMPILE_HERE]
  PUSHPSP ebx
  add dword [var_COMPILE_HERE], CELL_SIZE
  ret

;; ( i * x xt wid -- j * x )
DEFCODE 'TRAVERSE-WORDLIST',TRAVERSE_WORDLIST
  mov edx, TOS
  mov edx, [edx]
  _DROP                         ; i*x xt
.loop:
  test edx, edx
  jz .done
  mov ebx, [edx+CELL_SIZE]
  push TOS
  PUSHPSP ebx                   ; i*x xt nt
  _SWAP                         ; i*x nt xt
  call EXECUTE                  ; j*x flag
  test TOS, TOS
  pop TOS                       ; j*x xt
  jz .done
  mov edx, [edx]
  jmp .loop
.done:
  _DROP                         ; j*x
  ret

DEFCODE 'FORGET',FORGET
  ret

DEFCODE 'CODE',CODE,F_IMMED
  ret

DEFCODE ';CODE',SEMICOLON_CODE,F_IMMED
  ret

DEFCODE 'END-CODE',END_CODE,F_IMMED
  ret

DEFCODE 'ASSEMBLER',ASSEMBLER
  PUSHPSP ASSEMBLER_WORDLIST
  call SET_FIRST_WORDLIST
  ret

DEFCODE 'ASSEMBLER-WORDLIST',GET_ASSEMBLER_WORDLIST
  PUSHPSP ASSEMBLER_WORDLIST
  ret

DEFCODE 'EDITOR',EDITOR
  PUSHPSP EDITOR_WORDLIST
  call SET_FIRST_WORDLIST
  ret

DEFCODE 'EDITOR-WORDLIST',GET_EDITOR_WORDLIST
  PUSHPSP EDITOR_WORDLIST
  ret

ENDWORDLIST 'FORTH',FORTH_WORDLIST

;; editor words

DEFVAR 'CURSOR',CURSOR,0

DEFCODE 'LINE',LINE
  call CURSOR
  call FETCH
  PUSHPSP BLOCK_LINE_LENGTH
  call DIVMOD
  _SWAP
  _DROP
  ret

DEFCODE 'POS',POS
  call CURSOR
  call FETCH
  PUSHPSP BLOCK_LINE_LENGTH
  call DIVMOD
  _DROP
  ret

DEFCODE 'L',L
  call CURRENT_BLOCK
  call FETCH
  call LIST
  ret

DEFCODE 'T',T
  cmp TOS, 16
  jge .error
  PUSHPSP 6
  call LSHIFT
  call CURSOR
  call STORE
  call SPACE
  call CURRENT_BLOCK
  call FETCH
  call BUFFER
  call CURSOR
  call FETCH
  call ADD
  PUSHPSP BLOCK_LINE_LENGTH
  call TYPE
  call LINE
  call DOT
  ret
.error:
  call ERROR
.spin:
  jmp .spin

;; ( -- c-addr u )
DEFCODE 'INPUT-BUFFER',EDITOR_INPUT_BUFFER,F_HIDDEN
  mov ebx, tib
  mov edx, [var_TOIN]
  add ebx, edx
  PUSHPSP ebx
  mov ebx, INPUT_SOURCE(count)
  mov edx, [var_TOIN]
  sub ebx, edx
  PUSHPSP ebx
  ret

DEFCODE 'CONSUME-INPUT',EDITOR_CONSUME_INPUT,F_HIDDEN
  mov ebx, INPUT_SOURCE(count)
  mov [var_TOIN], ebx
  ret

DEFCODE 'P',P
  call EDITOR_INPUT_BUFFER
  call CURRENT_BLOCK
  call FETCH
  call BUFFER
  call LINE
  PUSHPSP 6
  call LSHIFT
  call ADD
  _SWAP
  PUSHPSP BLOCK_LINE_LENGTH
  call MIN
  call CMOVE
  call EDITOR_CONSUME_INPUT
  ret

DEFCODE 'F',F
  call EDITOR_INPUT_BUFFER
  test TOS, TOS
  jz .find_next
  PUSHPSP find_buffer
  _SWAP
  call CMOVE
.find_next:
  _DROP
  call CURRENT_BLOCK
  call BUFFER
  call CURSOR
  call ADD
  ret

DEFCODE 'I',EDITOR_I
  ret

DEFCODE 'E',E
  ret

DEFCODE 'D',D
  ret

DEFCODE 'R',R
  ret

DEFCODE 'TILL',TILL
  ret

DEFCODE 'U',U

  ret

DEFCODE 'X',X
  ret

DEFCODE 'WIPE',WIPE
  ret

DEFCODE 'N',N
  ret

DEFCODE 'B',B
  ret

DEFCODE 'COPY',COPY
  ret

DEFCODE 'S',S
  ret

DEFCODE 'M',M
  ret

ENDWORDLIST 'EDITOR',EDITOR_WORDLIST

;; assembler

ENDWORDLIST 'ASSEMBLER',ASSEMBLER_WORDLIST

[section .bss]

data_space:
  resb 4096*2

code_space:
  resb 4096*2

return_stack:
  resb RETURN_STACK_SIZE

data_stack:
  resb DATA_STACK_SIZE

pad:
  resb BLOCK_SIZE

tib:
  resb TEXT_BUFFER_SIZE

find_buffer:
  resb BLOCK_LINE_LENGTH

block_buffers:
  resd BLOCK_BUFFERS

blocks:
  resd BLOCK_BUFFERS*BLOCK_SIZE

[section .data]

input_source:
  istruc _INPUT_SOURCE
    at _INPUT_SOURCE.type,   dd INPUT_TYPE_KEYBOARD
    at _INPUT_SOURCE.count,  dd 0
    at _INPUT_SOURCE.buffer, dd tib
  iend

CONTEXT:
  dd FORTH_WORDLIST
  times (SIZE_CONTEXT-1) dd 0

[section .rodata]

MINIMAL_WORDLIST:
  dd .one
  db 16, "MINIMAL-WORDLIST"
  align CELL_SIZE, db 0
.one:
  dd .two
  dd name_FORTH_WORDLIST
.two:
  dd 0
  dd name_SET_ORDER

version:
  db `coreforth 0.1 (32-bit)\r\n`
.end:

error:
  db `  error\r\n`
.end:

ok:
  db `  ok\r\n`
.end:

compiled:
  db `  compiled\r\n`
.end:

redefined:
  db `  redefined\r\n`
.end:

crlf:
  db `\r\n`

compile_only:
  db `  error: interpreting a compile-only word\r\n`
.end:
