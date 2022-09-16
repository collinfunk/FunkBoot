org 0x7c00
bits 16

; FAT Header
jmp short start
nop

bdb_oem:                    db 'FUNKOSOS'           ; 8 bytes
bdb_bytes_per_sector:       dw 512
bdb_sectors_per_clusterterterterterter:    db 1
bdb_reserved_sectors:       dw 1
bdb_fat_count:              db 2
bdb_dir_entries_count:      dw 0E0h
bdb_total_sectors:          dw 2880                 ; 2880 * 512 = 1.44MB
bdb_media_descriptor_type:  db 0F0h                 ; F0 = 3.5" floppy disk
bdb_sectors_per_fat:        dw 9                    ; 9 sectors/fat
bdb_sectors_per_track:      dw 18
bdb_heads:                  dw 2
bdb_hidden_sectors:         dd 0
bdb_large_sector_count:     dd 0

; extended boot record
ebr_drive_number:           db 0                    ; 0x00 floppy, 0x80 hdd, useless
                            db 0                    ; reserved
ebr_signature:              db 29h
ebr_volume_id:              db 12h, 34h, 56h, 78h   ; serial number, value doesn't matter
ebr_volume_label:           db 'FUNKFUNKOSS'        ; 11 bytes, padded with spaces
ebr_system_id:              db 'FAT12   '           ; 8 bytes

; messages and errors to print
%define ENDL 0x0D, 0x0A
msg_floppy_read_error: db 'error reading floppy disk', ENDL, 0
msg_loading: db 'loading', ENDL, 0
msg_found: db 'found', ENDL, 0
msg_STAGE2_search: db 'STAGE2  BIN'
STAGE2_cluster: dw 0

STAGE2_LOAD_OFFSET equ 0x1000
STAGE2_LOAD_SEGMENT equ 0

start:
    jmp 0x0000:main

main:
    ; setup data segment
    xor ax, ax
    mov ds, ax
    mov es, ax

    ; stack segment, set stack to start from top of memory
    mov ss, ax
    mov sp, 0x7c00

    mov [ebr_drive_number], dl              ; set drive number

    ;show loading message
    mov si, msg_loading
    call putchar

    ; load floppy disk
    push es
    mov ah, 0x08                            ; read sector
    int 0x13                                ; call BIOS
    jc msg_floppy_read_error
    pop es

    and cl, 0x3F            
    xor ch, ch                              ; clear CH
    mov [bdb_sectors_per_track], cx         ; set sectors per track

    inc dh
    mov [bdb_heads], dh                     ; set heads

    mov ax, [bdb_sectors_per_fat]           ; set sectors per fat
    mov bl, [bdb_fat_count]
    xor bh, bh                              ; clear BH
    mul bx                                  ; sectors per fat * fat count
    add ax, [bdb_reserved_sectors]
    push ax
    
    mov ax, [bdb_dir_entries_count]
    shl ax, 5                               ; multiply by 32
    xor dx, dx                              ; clear DX
    div word[bdb_bytes_per_sector]          ; number of sectors to read

    test dx, dx
    jz .root_dir_after
    inc ax

.root_dir_after:

    mov cl, al
    pop ax
    mov dl, [ebr_drive_number]
    mov bx, buffer
    call read_disc

    xor bx, bx
    mov di, buffer

.search_STAGE2:
    mov si, msg_STAGE2_search
    mov cx, 11
    push di
    repe cmpsb
    pop di
    je .STAGE2_found

    add di, 32             
    inc bx
    cmp bx, [bdb_dir_entries_count]
    jl .search_STAGE2

    jmp STAGE2_not_found_error

.STAGE2_found:
    mov ax, [di + 26]
    mov [STAGE2_cluster], ax

    ; load FAT from disc to memory
    mov ax, [bdb_reserved_sectors]
    mov bx, buffer
    mov cl, [bdb_sectors_per_fat]
    mov dl, [ebr_drive_number]
    call read_disc

    ; read STAGE2 from disc to memory
    mov bx, STAGE2_LOAD_SEGMENT
    mov es, bx
    mov bx, STAGE2_LOAD_OFFSET

.load_STAGE2_iteration:
    mov ax, [STAGE2_cluster]

    add ax, 31

    mov cl, 1
    mov dl, [ebr_drive_number]
    call read_disc

    add bx, [bdb_bytes_per_sector]

    mov ax, [STAGE2_cluster]
    mov cx, 3
    mul cx
    mov cx, 2
    div cx

    mov si, buffer
    add si, ax
    mov ax, [ds:si]

    or dx, dx
    jz .even

.odd:
    shr ax, 4
    jmp .skip_cluster

.even:
    and ax, 0x0FFF

.skip_cluster:
    cmp ax, 0x0FF8
    jae .finished_read

    mov [STAGE2_cluster], ax
    jmp .load_STAGE2_iteration

.finished_read:
    mov si, msg_found
    call putchar
    mov dl, [ebr_drive_number]
	

    mov ax, STAGE2_LOAD_SEGMENT
    mov ds, ax
    mov es, ax

    jmp STAGE2_LOAD_SEGMENT:STAGE2_LOAD_OFFSET ; jump to STAGE2

    jmp wait_keypress_and_restart

    cli
    hlt

; ================================================================

; print a character to the screen
; Prints a message to screen
; param: ds:si - message to print
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


; Operations for FAT12 and Disc Drive

; Converts lba to chs (lba to cyliner-head-sector)
; param:
;   ax - lba
; returns:
;   cx [bits 0-5] - sector
;   cx [bits 6-15] - cylinder
;   dh - head
lbaconv:
    push ax
    push dx

    xor dx,dx                                   ; clear dx = 0
    div word [bdb_sectors_per_track]            ; divide by sectors per track


    inc dx                                      ; increment to + 1
    mov cx, dx                                  ; set cx = sector

    xor dx,dx                                   ; clear dx = 0
    div word [bdb_heads]                        ; divide by heads

    mov dh, dl                                  ; set dh = head
    mov ch, al                                  ; set ch = cylinder (lower 8 bits)
    shl ah, 6                                   ; shift cylinder to upper 8 bits
    or cl, ah                                   ; set cl = cylinder (upper 8 bits)

    pop ax
    mov dl, al                                  ; restore dl = lba
    pop ax
    ret


; Read from disc
; param:
;   ax - lba address
;   cl - number of sectors to read (1-255)
;   dl - drive number (0x80 for hdd)
;   es:bx - buffer to read into
read_disc:
    ; save registers used
    push ax
    push bx
    push cx
    push dx
    push di

    push cx                                   ; save number of sectors to read
    call lbaconv                              ; convert lba to chs
    pop ax                                    ; al = sector

    mov ah, 0x02                              ; set ah to 0x02 to read from disc
    mov di, 5                                 ; set di to 5 to read 5 times before returning


.retry_read:
    pusha                                     ; save all registers
    stc                                       ; set carry flag
    int 0x13                                  ; call the BIOS
    jnc .success                                 ; if carry flag is clear, loop

    ; read failed, try again
    popa                                  ; restore all registers
    call reset_disc                       ; reset disc

    dec di
    test di, di                           ; if di is not 0, loop
    jnz .retry_read

.fail:
    ; read failed, return error
    jmp floppy_read_error


.success:
    popa
    ; restore registers used
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret

    
; reset disc
; param:
;   dl - drive number (0x80 for hdd)
reset_disc:
    pusha                                   ; save all registers
    mov ah, 0                               ; set ah to 0 to reset disc
    stc                                     ; set carry flag
    int 0x13                                ; call the BIOS
    jc floppy_read_error                    ; if carry flag is set, return error
    popa                                    ; restore all registers
    ret

;
; Error handling functions
; Floppy Read Error
floppy_read_error:
    mov si, msg_floppy_read_error
    call putchar
    jmp wait_keypress_and_restart


STAGE2_not_found_error:
    mov si, msg_STAGE2_search
    call putchar
    jmp wait_keypress_and_restart

; Wait for keypress and restart
wait_keypress_and_restart:
    ; wait for a keypress
    mov si, msg_floppy_read_error            ; set si to msg_floppy_read_error
    call putchar                         ; print error message
    mov ah, 0                            ; set ah to 0 to read from keyboard
    int 0x16                             ; call the BIOS
    int 0x19                             ; restart the program

.halt:
    cli                                  ; disable interrupts
    hlt                                  ; halt the processor  
    
; no code after this
times 510-($-$$) db 0
dw 0AA55h
; no code after this

buffer: