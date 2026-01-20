
.text
.global _start
_start:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024
	 mov r0, #0
	 str r0, [r11, #-12] @ store result a
	 mov r0, #1
	 str r0, [r11, #-16] @ store result b
	 ldr r0, [r11, #-12]
	 ldr r1, [r11, #-16] @ load b
	 cmp r0, r1
	 bne _else_0
	 mov r0, #0
	 str r0, [r11, #-20] @ store result res1
	 ldr r0, [r11, #-20] @ load res1
	 bl _min_caml_print_int
	 b _end_1
_else_0:
	 mov r0, #1
	 str r0, [r11, #-24] @ store result res2
	 ldr r0, [r11, #-24] @ load res2
	 bl _min_caml_print_int
_end_1:
	 bl _min_caml_print_newline
	 sub r13, r11, #4
	 pop {r11, r14}
	 bl _min_caml_exit
