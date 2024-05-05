format ELF64
section '.text' executable

;; Enable for including a main function
TESTING = 0

public strlen
strlen:
    push    rbp
    mov     rbp, rsp
    mov     QWORD [rbp-24], rdi
    mov     QWORD [rbp-8], 0
    jmp     .L2
.L3:
    add     QWORD [rbp-8], 1
    add     QWORD [rbp-24], 1
.L2:
    mov     rax, QWORD [rbp-24]
    movzx   eax, BYTE  [rax]
    test    al, al
    jne     .L3
    mov     rax, QWORD [rbp-8]
    pop     rbp
    ret

public print_string
print_string:
    push    rbp
    mov     rbp, rsp
    sub     rsp, 16
    mov     QWORD [rbp-8], rdi
    mov     rax, QWORD [rbp-8]
    mov     rdi, rax
    call    strlen
    mov     rdx, rax
    mov     rax, QWORD [rbp-8]
    mov     rsi, rax
    mov     edi, 1
    mov     rax, 1
    syscall
    mov     rax, 1
    mov     rdi, 1
    mov     rsi, new_line
    mov     rdx, 1
    syscall
    nop
    leave
    ret

public print_unsigned
print_unsigned:
    mov     r9, -3689348814741910323
    sub     rsp, 40
    mov     BYTE [rsp+31], 10
    lea     rcx, [rsp+30]
.L2:
    mov     rax, rdi
    lea     r8, [rsp+32]
    mul     r9
    mov     rax, rdi
    sub     r8, rcx
    shr     rdx, 3
    lea     rsi, [rdx+rdx*4]
    add     rsi, rsi
    sub     rax, rsi
    add     eax, 48
    mov     BYTE [rcx], al
    mov     rax, rdi
    mov     rdi, rdx
    mov     rdx, rcx
    sub     rcx, 1
    cmp     rax, 9
    ja      .L2
    lea     rax, [rsp+32]
    mov     edi, 1
    sub     rdx, rax
    xor     eax, eax
    lea     rsi, [rsp+32+rdx]
    mov     rdx, r8
    mov     rax, 1
    syscall
    add     rsp, 40
    ret

public exit
exit:
    mov rax, 60
    syscall

;; Main function for testing the library
if TESTING
    public _start
    _start:
        mov rdi, 69
        call print_unsigned

        mov rdi, hello_world_text
        call print_string

        mov rax, 60
        mov rdi, 0
        syscall
end if

section '.data' writeable
if TESTING
    hello_world_text: db "Hello, World", 0
end if
new_line: db 10

section '.note.GNU-stack'
