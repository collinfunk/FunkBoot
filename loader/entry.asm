org 0x1000
bits 16

stage2_start:
    cli

	mov [g_BootDrive], dl

    xor ax, ax
    mov es, ax
    mov ds, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    mov sp, 0x3000

    ; Message we entered stage 2
    mov si, msg_enter_stage2
    call putchar

    ; Check if our a20 gate is enabled
    call check_a20_rm
    cmp ax, 0
    jne .skip_enable_a20
    
;Enable a20, skip this it is already enabled
.enable_a20:
    call enable_A20

; If a20 was enabled, we go here and skip the enabling
.skip_enable_a20:
    ; Check if our a20 gate is enabled
    call check_CPUID    ; Call our CPUID check routine
    cmp eax, 0
    je error_no_CPUID   ; If we don't have CPUID, we can't continue and halt computer

has_CPUID:
    mov si, msg_has_CPUID
    call putchar


	; We have CPUID, we continue and check if we have a 64 bit cpu
	call check_64bit_CPUID

	; check our low memory
	call check_low_memory

	; get memory map
	call do_e820
	
    cli
    lgdt [gdt32Desc]

    mov eax, cr0
    or eax, 1
    mov cr0, eax

    
    jmp 0x08:pmode_init




; 16 bit BIOS print functions
;------------------------------------------------------------------------------
putchar:
    push si         ; save si
    push ax         ; save ax
    push bx         ; save bh

.loop:
    lodsb           ; load a byte from ds:si
    or al, al       ; if al == 0, we're done
    jz .done        ; if al is 0, we're done

    mov ah, 0x0e    ; set ah to 0x0e to print a character
    mov bh, 0       ; set page number to 0
    int 0x10        ; call the BIOS


    jmp .loop ; loop

    
.done:
    pop bx      ; restore bh
    pop ax      ; restore ax
    pop si      ; restore si
    ret         ; return

printreg16:
   mov di, outstr16
   mov ax, [reg16]
   mov si, hexstr
   mov cx, 4   ;four places
hexloop:
   rol ax, 4   ;leftmost will
   mov bx, ax   ; become
   and bx, 0x0f   ; rightmost
   mov bl, [si + bx];index into hexstr
   mov [di], bl
   inc di
   dec cx
   jnz hexloop
 
   mov si, outstr16
   call putchar
 
   ret


; CPUID functions
;------------------------------------------------------------------------------
check_CPUID:
    pushfd                      ; save EFLAGS
    pushfd                      ; store EFLAGS

    xor dword [esp], 0x00200000 ; Invert ID bit
    popfd                       ; restore EFLAGS (with ID bit inverted)
    pushfd                      ; save EFLAGS (ID flag may or may not be inverted)

    pop eax                     ; eax = modified EFLAGS (ID bit may or may not be inverted)
    xor eax, [esp]              ; eax = modified EFLAGS ^ 0x00200000 = original EFLAGS
    popfd                       ; restore EFLAGS (restore original EFLAGS
    and eax, 0x00200000         ; eax = 0 if id bit cant be changed, else non-zero
    ret

check_64bit_CPUID:
	mov eax, 0x80000000    ; Set the A-register to 0x80000000.
    cpuid                  ; CPU identification.
    cmp eax, 0x80000001    ; Compare the A-register with 0x80000001.
	jb error_not64bit     ; If the A-register is less than 0x80000001, we have a 32 bit CPU.
	mov si, msg_64bit_cpu   ; If the A-register is greater than 0x80000001, we have a 64 bit CPU.
	call putchar
	ret

; Memory map functions
;------------------------------------------------------------------------------
check_low_memory:
	clc
	int 0x12
	jc error_low_memory
	;Prints low memory string
	mov si, msg_low_memory
	call putchar
	mov  word [reg16], ax ;look at register and get value
	call printreg16
	ret

; Error handlers
;------------------------------------------------------------------------------
error_no_CPUID:
    mov si, msg_no_CPUID
    call putchar
    jmp $

error_not64bit:
	mov si, msg_not64bit
	call putchar
	jmp $

error_low_memory:
	mov si, msg_low_memory_error
	call putchar
	jmp $

; Real mode data
;------------------------------------------------------------------------------
%define ENDL 0x0D, 0x0A

msg_enter_stage2: db 'Entered stage 2', ENDL, 0
msg_a20_rm_disabled: db 'A20 remap disabled', ENDL, 0
msg_no_CPUID: db 'No CPUID', ENDL, 0
msg_has_CPUID: db 'Has CPUID', ENDL, 0
msg_protected_mode: db 'Entered protected mode...', ENDL, 0
msg_not64bit: db 'Not a 64 bit CPU, unsupported...', ENDL, 0
msg_64bit_cpu: db '64 bit CPU, supported...', ENDL, 0
msg_low_memory_error: db 'Low memory error', ENDL, 0
msg_low_memory: db 'Memory below 1MB (in KB): ', 0
outstr16:   db '0000', 0  ;register value string
reg16:   dw    0  ; pass values to printreg16
hexstr:   db '0123456789ABCDEF'

g_BootDrive: db 0
g_BootPartitionSeg: dw 0
g_BootPartitionOff: dw 0

; Protected mode includes
;------------------------------------------------------------------------------
%include "A20_func.inc"
%include "gdt32.inc"  
%include "e820_map.inc"

; Protected mode code
;------------------------------------------------------------------------------
bits 32
pmode_init:
	; setup segments, now we can no longer use bios to print
	mov ax, 0x10
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	mov ss, ax
	mov esp, 0x3000

	; Clear the tables
	mov edi, 0x1000 ; destination index 0x1000
	mov cr3, edi	; set page table base register to 0x1000
	xor eax, eax	; clear eax
	mov ecx, 0x4096 ; set ecx to 4096
	rep stosd		; fill page table with 0s
	mov edi, cr3	; set edi to page table base register

	; Setup the page tables
	mov dword[edi], 0x2003 ; set page table entry to 0x2003, last bit 3 means present, writeable
	add edi, 0x1000 	   ; add 0x1000 to edi, destination index
	mov dword[edi], 0x3003 ; set page table entry to 0x3003 (present, writeable)
	add edi, 0x1000 	   ; add 0x1000 to edi, destination index
	mov dword[edi], 0x4003 ; set page table entry to 0x4003 (present, writeable)
	add edi, 0x1000 	   ; add 0x1000 to edi, destination index

	; Identinity map the first 2 MB of memory
	mov ebx, 0x00000003
	mov ecx, 512

	; Loop through the page table entries
.setEntry:
	mov dword[edi], ebx ; int at destination index is the b register
	add ebx, 0x1000		; add 0x1000 to the b register
	add edi, 8			; add 8 to the destination index
	loop .setEntry 		; Set the next entry

	; Enable PAE-paging
	mov eax, cr4		; Set eax to cr4
	or eax, 1 << 5		; set PAE bit at bit 5 (starting from index 0)
	mov cr4, eax		; Set control register for 4 to eax
	; Paging now setup but not enabled

	; Set the long mode bit
	mov ecx, 0xC0000080 ; Set ecx to the long mode bit
	rdmsr 				; Read the MSR (model specific register)
	or eax, 1 << 8		; set the long mode bit at bit 8 (starting from index 0)
	wrmsr				; Write the MSR (model specific register)

	; Enable paging
	mov eax, cr0		; Set eax to cr0
	or eax, 1 << 31		; set the paging bit at bit 31 (starting from index 0)
	mov cr0, eax		; Set control register for 0 to eax
	; Paging now enabled, still in compatibility mode (32 bit)

	lgdt[gdt64.pointer]
	jmp gdt64.code:longmode_init

halt:
    jmp halt


; 32bit protected mode data
;------------------------------------------------------------------------------


; 32bit protected mode includes
;------------------------------------------------------------------------------
%include "gdt64.inc"


;64 bit long mode code
;------------------------------------------------------------------------------

bits 64
longmode_init:

	cli                           ; Clear the interrupt flag.
   ; mov ax, gdt64.data            ; Set the A-register to the data descriptor.
    ;mov ds, ax                    ; Set the data segment to the A-register.
    ;mov es, ax                    ; Set the extra segment to the A-register.
    ;mov fs, ax                    ; Set the F-segment to the A-register.
    ;mov gs, ax                    ; Set the G-segment to the A-register.
    ;mov ss, ax                    ; Set the stack segment to the A-register.
    ;mov edi, 0xB8000              ; Set the destination index to 0xB8000.
    ;mov rax, 0x1F201F201F201F20   ; Set the A-register to 0x1F201F201F201F20.
    ;mov ecx, 500                  ; Set the C-register to 500.
    ;rep stosq                     ; Clear the screen.
    hlt                           ; Halt the processor.
