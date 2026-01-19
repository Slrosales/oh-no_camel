.text
.global _add_x

_add_x:
	push {r11, r14}
	mov r11, r13
	sub r13, r13, #16 @ reqserve frame
	str r0, [r11, #-12] @ save param y 
	ldr r0, [r11, #-16]
	mov r1, #4
	ldr r0, [r0, r1, lsl #2]
	str r0, [r11, #-20] @ store result x
	ldr r0, [r11, #-12]
	ldr r1, [r11, #-20] @ load x
	add r0, r0, r1
	add r13, r13, #16
	pop {r11, r15}

.text
.global _apply_to_zero

_apply_to_zero:
	push {lr}
	ldr r1, [r11, #-12]
	blx r1
	pop {pc}

.text
.global _start

 _start:
	push {r11, r14}
	add r11, r13, #4
	sub r13, r13, #2048
	mov r0, #1
	str r0, [r11, #-12] @ store result x0
	mov r0, #8
	lsl r0, r0, #2
	bl _min_caml_alloc
	str r0, [r11, #-16] @ store result add_x
	str r0, [r11, #-20] @ store result addr_add_x
	ldr r0, [r11, #-16]
	mov r1, #0
	ldr r2, [r11, #-20]
	str r2, [r0, r1, lsl #2]
	str r0, [r11, #-24] @ store result tu0
	ldr r0, [r11, #-16]
	mov r1, #4
	ldr r2, [r11, #-12]
	str r2, [r0, r1, lsl #2]
	str r0, [r11, #-28] @ store result tu1
	ldr r0, [r11, #-16] @ load add_x
	bl _apply_to_zero
	str r0, [r11, #-32] @ store result res
	ldr r0, [r11, #-32] @ load res
	bl _min_caml_print_int
	bl _min_caml_print_newline
	sub r13, r11, #4
	pop {r11, r14}
	bl	_min_caml_exit
