%ifndef _A20_INC
%define _A20_INC

bits 16

;------------------------------------------------------------------------------
; Check if A20 is enabled from real mode
; returns 0 to ax if A20 is disabled
; returns 1 to ax if A20 is enabled
; We always check A20 before enabling, this is the function used in real mode
check_a20_rm:
    pushf
    push ds
    push es
    push di
    push si
 
    cli
 
    xor ax, ax ; ax = 0
    mov es, ax
 
    not ax ; ax = 0xFFFF
    mov ds, ax
 
    mov di, 0x0500
    mov si, 0x0510
 
    mov al, byte [es:di]
    push ax
 
    mov al, byte [ds:si]
    push ax
 
    mov byte [es:di], 0x00
    mov byte [ds:si], 0xFF
 
    cmp byte [es:di], 0xFF
 
    pop ax
    mov byte [ds:si], al
 
    pop ax
    mov byte [es:di], al
 
    mov ax, 0
    je check_a20__exit
 
    mov ax, 1
 
check_a20__exit:
    pop si
    pop di
    pop es
    pop ds
    popf
 
    ret

;------------------------------------------------------------------------------
; The original method of enabaling A20 is using they keyboard controller method.
enable_A20:
    cli
 
    call    a20wait
    mov     al,0xAD
    out     0x64,al
 
    call    a20wait
    mov     al,0xD0
    out     0x64,al
 
    call    a20wait2
    in      al,0x60
    push    eax
 
    call    a20wait
    mov     al,0xD1
    out     0x64,al
 
    call    a20wait
    pop     eax
    or      al,2
    out     0x60,al
 
    call    a20wait
    mov     al,0xAE
    out     0x64,al
 
    call    a20wait
    sti
    ret
 
a20wait:
    in      al,0x64
    test    al,2
    jnz     a20wait
    ret
 
 
a20wait2:
    in      al,0x64
    test    al,1
    jz      a20wait2
    ret

;------------------------------------------------------------------------------
; Fast A20 enable, should work on most modern computers, but not all
enable_A20_fast:
    in al, 0x92
    or al, 2
    out 0x92, al

%endif