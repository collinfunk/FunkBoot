DIR_STAGE1		:= boot
DIR_STAGE2		:= loader

TARGET			:= funkboot.bin
IMAGE			:= funkboot.img

STAGE1 			:= $(DIR_STAGE1)/stage1.bin
STAGE2 			:= $(DIR_STAGE2)/stage2.bin

STAGE1_ASM		:= $(wildcard $(DIR_STAGE1)/*.asm)
STAGE2_ASM		:= $(wildcard $(DIR_STAGE2)/*.asm)


NASM			:= nasm
NASMFLAGS		:= -f bin

.PHONY: all run clean
all: $(IMAGE)

run: $(IMAGE)
	qemu-system-x86_64 -drive file=$(IMAGE),format=raw

$(IMAGE): $(STAGE2)
	dd if=/dev/zero of=$(IMAGE) bs=512 count=2880
	mkfs.fat -F 12 -n "FUNKBOOT" $(IMAGE)
	dd if=$(STAGE1) of=$(IMAGE) bs=512 count=1 conv=notrunc
	mcopy -i $(IMAGE) $(STAGE2) "::stage2.bin"

$(STAGE1):
	$(NASM) $(NASMFLAGS) -o $(STAGE1) $(STAGE1_ASM)
	

$(STAGE2): $(STAGE1)
	$(NASM) $(NASMFLAGS) -I$(DIR_STAGE2) -o $(STAGE2) $(STAGE2_ASM)

clean:
	rm -rf $(STAGE1) $(STAGE2) $(IMAGE)