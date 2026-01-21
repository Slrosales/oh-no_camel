.text
.global _start

 _start:
	push {r11, r14}
	add r11, r13, #4
	sub r13, r13, #2048
	mov r0, #10
	str r0, [r11, #-12] @ store result a
	mov r0, #5
	str r0, [r11, #-16] @ store result b
	ldr r0, [r11, #-12]
	ldr r1, [r11, #-16] @ load b
	add r0, r0, r1
	str r0, [r11, #-20] @ store result sum
	ldr r0, [r11, #-20] @ load sum
	bl _min_caml_print_int
	bl _min_caml_print_newline
	sub r13, r11, #4
	pop {r11, r14}
	bl	_min_caml_exit
