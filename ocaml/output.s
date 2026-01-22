
.text
.global _start
_start:
	 push {r4-r11, r12, r14}
	 mov r11, r13
	 sub r13, r13, #1024
	 mov r4, #5
	 mov r6, #3
	 mov r8, #5
	 mov r1, r5
	 mov r2, r8
	 add r9, r1, r2
	 mov r1, r9
	 mov r2, r7
	 sub r10, r1, r2
	 mov r12, #3
	 mov r1, r5
	 mov r2, r12
	 add r0, r1, r2
	 str r0, [r11, #-4]
	 ldr r1, [r11, #-4]
	 mov r2, r7
	 sub r0, r1, r2
	 str r0, [r11, #-8]
	 mov r1, r10
	 ldr r2, [r11, #-8]
	 sub r0, r1, r2
	 str r0, [r11, #-12]
	 ldr r1, [r11, #-12]
	 mov r2, r5
	 add r0, r1, r2
	 str r0, [r11, #-16]
	 ldr r1, [r11, #-16]
	 mov r2, r7
	 sub r0, r1, r2
	 str r0, [r11, #-20]
	 ldr r0, [r11, #-20]
	 bl _min_caml_print_int
	 bl _min_caml_print_newline
	 mov r13, r11
	 pop {r4-r11, r12, r14}
	 bl _min_caml_exit
