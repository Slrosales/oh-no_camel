.text
.global _start

 _start:
	push {r11, r14}
	add r11, r13, #4
	sub r13, r13, #2048
	mov r0, #2
	str r0, [r11, #-12] @ store result size
	mov r0, #0
	str r0, [r11, #-16] @ store result init
	ldr r0, [r11, #-12] @ load size
	ldr r1, [r11, #-16] @ load init
	bl _min_caml_create_array
	str r0, [r11, #-20] @ store result arr
	mov r0, #2
	str r0, [r11, #-24] @ store result t1_size
	ldr r0, [r11, #-24] @ load t1_size
	lsl r0, r0, #2
	bl _min_caml_alloc
	str r0, [r11, #-28] @ store result t1
	mov r0, #65
	str r0, [r11, #-32] @ store result char_a
	mov r0, #100
	str r0, [r11, #-36] @ store result num_100
	mov r0, #0
	str r0, [r11, #-40] @ store result idx0
	mov r0, #1
	str r0, [r11, #-44] @ store result idx1
	ldr r0, [r11, #-28]
	ldr r1, [r11, #-40] @ load idx0
	ldr r2, [r11, #-32]
	str r2, [r0, r1, lsl #2]
	str r0, [r11, #-48] @ store result d1
	ldr r0, [r11, #-28]
	ldr r1, [r11, #-44] @ load idx1
	ldr r2, [r11, #-36]
	str r2, [r0, r1, lsl #2]
	str r0, [r11, #-52] @ store result d2
	mov r0, #2
	str r0, [r11, #-56] @ store result t2_size
	ldr r0, [r11, #-56] @ load t2_size
	lsl r0, r0, #2
	bl _min_caml_alloc
	str r0, [r11, #-60] @ store result t2
	mov r0, #66
	str r0, [r11, #-64] @ store result char_b
	mov r0, #200
	str r0, [r11, #-68] @ store result num_200
	ldr r0, [r11, #-60]
	ldr r1, [r11, #-40] @ load idx0
	ldr r2, [r11, #-64]
	str r2, [r0, r1, lsl #2]
	str r0, [r11, #-72] @ store result d3
	ldr r0, [r11, #-60]
	ldr r1, [r11, #-44] @ load idx1
	ldr r2, [r11, #-68]
	str r2, [r0, r1, lsl #2]
	str r0, [r11, #-76] @ store result d4
	mov r0, #0
	str r0, [r11, #-80] @ store result i0
	mov r0, #1
	str r0, [r11, #-84] @ store result i1
	ldr r0, [r11, #-20]
	ldr r1, [r11, #-80] @ load i0
	ldr r2, [r11, #-28]
	str r2, [r0, r1, lsl #2]
	str r0, [r11, #-88] @ store result d5
	ldr r0, [r11, #-20]
	ldr r1, [r11, #-84] @ load i1
	ldr r2, [r11, #-60]
	str r2, [r0, r1, lsl #2]
	str r0, [r11, #-92] @ store result d6
	ldr r0, [r11, #-20]
	ldr r1, [r11, #-84] @ load i1
	ldr r0, [r0, r1, lsl #2]
	str r0, [r11, #-96] @ store result grabbed_t2
	ldr r0, [r11, #-96]
	ldr r1, [r11, #-44] @ load idx1
	ldr r0, [r0, r1, lsl #2]
	str r0, [r11, #-100] @ store result res_num
	ldr r0, [r11, #-100] @ load res_num
	bl _min_caml_print_int
	bl _min_caml_print_newline
	sub r13, r11, #4
	pop {r11, r14}
	bl	_min_caml_exit
