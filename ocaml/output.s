.text
.align 2
.global main

main:
	push {fp, lr}
	add fp, sp, #4
	sub sp, sp, #1024
	mov r0, #0
	str r0, [fp, #-12] @ store a
	mov r0, #1
	str r0, [fp, #-16] @ store b
	ldr r0, [fp, #-12]
	ldr r1, [fp, #-16]
	cmp r0, r1
	bne else_v2
	mov r0, #0
	str r0, [fp, #-20] @ store res1
	ldr r0, [fp, #-24] @ arg 0: min_caml_print_int
	ldr r1, [fp, #-20] @ arg 1: res1
	bl min_caml_print_int
	b end_v3
else_v2:
	mov r0, #1
	str r0, [fp, #-28] @ store res2
	ldr r0, [fp, #-24] @ arg 0: min_caml_print_int
	ldr r1, [fp, #-28] @ arg 1: res2
	bl min_caml_print_int
end_v3:
	sub sp, fp, #4
	pop {fp, pc}
