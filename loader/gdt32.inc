%ifndef _GDT32_INC
%define _GDT32_INC

gdt32:      ; NULL descriptor
    dq 0

    ; 32-bit code segment
    dw 0FFFFh
    dw 0
    db 0
    db 10011010b
    db 11001111b
    db 0

    ; 32-bit data segment
    dw 0FFFFh
    dw 0
    db 0
    db 10010010b
    db 11001111b
    db 0

    ; 16-bit code segment
    dw 0FFFFh
    dw 0
    db 0
    db 10011010b
    db 00001111b
    db 0

    ; 16-bit data segment
    dw 0FFFFh
    dw 0
    db 0
    db 10010010b
    db 00001111b
    db 0

gdt32Desc:  dw gdt32Desc - gdt32 - 1
            dd gdt32
%endif