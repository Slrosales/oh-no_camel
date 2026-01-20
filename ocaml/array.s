
.text
.global _start
_start:
	 push {r4-r10, r11, r14}
	 add r11, r13, #32
	 sub r13, r13, #1024
	 mov r4, #2
	 mov r5, #0
	 mov r0, r4
	 mov r1, r5
	 bl _min_caml_create_array
	 mov r6, r0
	 mov r7, #0
	 mov r8, #1
	 mov r0, r6
	 mov r1, r7
	 mov r2, r8
	 str r2, [r0, r1, lsl #2]
	 mov r10, #1
	 mov r0, r6
	 mov r1, r7
	 ldr r0, [r0, r1, lsl #2]
	 str r0, [r11, #4]
	 mov r0, r6
	 mov r1, r10
	 ldr r2, [r11, #4]
	 str r2, [r0, r1, lsl #2]
	 bl _min_caml_print_newline
	 sub r13, r11, #32
	 pop {r4-r10, r11, r14}
	 bl _min_caml_exit
