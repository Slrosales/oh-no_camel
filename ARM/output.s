.text 
.global _start

 _start:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024
	 mov r0, #2
	 str r0, [r11, #-12] @ store result size
	 mov r0, #0
	 str r0, [r11, #-16] @ store result init
	 ldr r0, [r11, #-12] @ load size
	 ldr r1, [r11, #-16] @ load init
	 bl _min_caml_create_array
	 str r0, [r11, #-20] @ store result arr
	 mov r0, #0
	 str r0, [r11, #-24] @ store result i0
	 mov r0, #1
	 str r0, [r11, #-28] @ store result v0
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-24] @ load i0
	 ldr r2, [r11, #-28]
	 str r2, [r0, r1]
	 str r0, [r11, #-32] @ store result tu
	 mov r0, #1
	 str r0, [r11, #-36] @ store result i1
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-24] @ load i0
	 ldr r0, [r0, r1]
	 str r0, [r11, #-40] @ store result v1
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-36] @ load i1
	 ldr r2, [r11, #-40]
	 str r2, [r0, r1]
	 bl _min_caml_print_newline
	 sub r13, r11, #4
	 pop {r11, r14}
	 bl	_min_caml_exit
