
.global _sum_of_5_arguments
_sum_of_5_arguments:
	 push {r4-r10, r11, r14}
	 add r11, r13, #32
	 sub r13, r13, #1024
	 add r4, r0, r1
	 add r5, r4, r2
	 add r6, r5, r3
	 ldr r3, [r11, #1060]
	 add r7, r6, r3
	 ldr r3, [r11, #1064]
	 add r0, r7, r3
	 sub r13, r11, #32
	 pop {r4-r10, r11, r15}

.text
.global _start
_start:
	 push {r4-r10, r11, r14}
	 add r11, r13, #32
	 sub r13, r13, #1024
	 mov r4, #1
	 mov r5, #2
	 mov r6, #3
	 mov r7, #4
	 mov r8, #5
	 mov r9, #6
	 mov r0, r4
	 mov r1, r5
	 mov r2, r6
	 mov r3, r7
	 mov r3, r8
	 str r3, [r13, #0]
	 mov r3, r9
	 str r3, [r13, #4]
	 bl _sum_of_5_arguments
	 mov r10, r0
	 mov r0, r10
	 bl _min_caml_print_int
	 bl _min_caml_print_newline
	 sub r13, r11, #32
	 pop {r4-r10, r11, r14}
	 bl _min_caml_exit
