.text 
.global _start

 _start:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024
	 mov r0, #3
	 str r0, [r11, #-12] @ store result size
	 mov r0, #0
	 str r0, [r11, #-16] @ store result init
	 ldr r0, [r11, #-12] @ load size
	 ldr r1, [r11, #-16] @ load init
	 bl _min_caml_create_array
	 str r0, [r11, #-20] @ store result arr
	 mov r0, #0
	 str r0, [r11, #-24] @ store result i0
	 mov r0, #15
	 str r0, [r11, #-28] @ store result v0
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-24] @ load i0
	 ldr r2, [r11, #-28]
	 str r2, [r0, r1, lsl #2]
	 str r0, [r11, #-32] @ store result d1
	 mov r0, #1
	 str r0, [r11, #-36] @ store result i1
	 mov r0, #5
	 str r0, [r11, #-40] @ store result v1
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-36] @ load i1
	 ldr r2, [r11, #-40]
	 str r2, [r0, r1, lsl #2]
	 str r0, [r11, #-44] @ store result d2
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-24] @ load i0
	 ldr r0, [r0, r1, lsl #2]
	 str r0, [r11, #-48] @ store result a
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-36] @ load i1
	 ldr r0, [r0, r1, lsl #2]
	 str r0, [r11, #-52] @ store result b
	 ldr r0, [r11, #-48]
	 ldr r1, [r11, #-52] @ load b
	add r0, r0, r1
	 str r0, [r11, #-56] @ store result sum
	 mov r0, #10
	 str r0, [r11, #-60] @ store result ten
	 ldr r0, [r11, #-56]
	 ldr r1, [r11, #-60] @ load ten
	sub r0, r0, r1
	 str r0, [r11, #-64] @ store result res
	 mov r0, #2
	 str r0, [r11, #-68] @ store result i2
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-68] @ load i2
	 ldr r2, [r11, #-64]
	 str r2, [r0, r1, lsl #2]
	 str r0, [r11, #-72] @ store result d3
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-68] @ load i2
	 ldr r0, [r0, r1, lsl #2]
	 str r0, [r11, #-76] @ store result final
	 ldr r0, [r11, #-76] @ load final
	 bl _min_caml_print_int
	 bl _min_caml_print_newline
	 sub r13, r11, #4
	 pop {r11, r14}
	 bl	_min_caml_exit
