.text
.align 2
.global main

main:
	push {r11, r14}     @ save fp and lr
	add r11, r13, #4    @ set fp (r11) to previous sp
	sub r13, r13, #1024 @ allocate stack space (sp = r13)
	mov r0, #1
	str r0, [r11, #-12] @ store v0
	ldr r0, [r11, #-12] @ arg 0
	bl _min_caml_print_int
	sub r13, r11, #4    @ restore sp (r13)
	pop {r11, r15}      @ restore fp and jump to lr (pc = r15)
