
.global _test
_test:
	 push {r4-r10, r11, r14}
	 add r11, r13, #32
	 sub r13, r13, #1024
	 mov r4, #1
	 add r0, r0, r4
	 sub r13, r11, #32
	 pop {r4-r10, r11, r15}

.text
.global _start
_start:
	 push {r4-r10, r11, r14}
	 add r11, r13, #32
	 sub r13, r13, #1024
	 mov r4, #5
	 mov r0, r4
	 bl _test
	 mov r5, r0
	 mov r0, r5
	 bl _min_caml_print_int
	 bl _min_caml_print_newline
	 sub r13, r11, #32
	 pop {r4-r10, r11, r14}
	 bl _min_caml_exit
