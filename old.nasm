;; ( char "<chars>ccc<char>" -- c-addr u )
DEFCODE 'WORD',W0RD
  movzx edx, al
  mov edi, wordbuf

.again:
  call consume_char
  test al, al
  jz .done
  cmp byte al, `\\`
  je .consume_line
  cmp byte al, dl
  jbe .again

.consume_chars:
  stosb
  call consume_char
  test al, al
  jz .done
  cmp byte al, dl
  ja .consume_chars
  jmp .done

.consume_line:
  call consume_char
  test al, al
  jz .done
  cmp byte  al, `\n`
  jne .consume_line
  jmp .again

.done:
  sub edi, wordbuf
  mov ecx, edi
  mov edi, wordbuf
  sub PSP, CELL_SIZE
  mov [PSP], edi
  mov TOS, ecx
  ret

consume_char:
  push edx
  mov ebx, [var_TOIN]
  cmp ebx, INPUT_SOURCE(count)
  jge .nochar
  mov edx, INPUT_SOURCE(buffer)
  mov byte al, [edx+ebx]
  inc dword [var_TOIN]
  pop edx
  ret

.nochar:
  xor eax, eax
  pop edx
  ret
