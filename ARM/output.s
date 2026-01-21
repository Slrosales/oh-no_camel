
.text
.global _start
_start:
	 push {r4-r10, r11, r14}
	 add r11, r13, #32
	 sub r13, r13, #1024
	 mov r4, #3
	 mov r5, #6
	 cmp r4, r5
	 ble then_1
	 mov r3, #1
	 cmp r4, r3
	 ble then_3
	 sub r9, r5, r4
	 mov r0, r9
	 bl _min_caml_print_int
	 b else_end_2
then_3:
	 add r8, r4, r5
	 mov r0, r8
	 bl _min_caml_print_int
else_end_2:
	 b else_end_0
then_1:
	 mov r3, #2
	 cmp r4, r3
	 ble then_5
	 mov r7, #4
	 mov r0, r7
	 bl _min_caml_print_int
	 b else_end_4
then_5:
	 mov r6, #3
	 mov r0, r6
	 bl _min_caml_print_int
else_end_4:
else_end_0:
	 bl _min_caml_print_newline
	 sub r13, r11, #32
	 pop {r4-r10, r11, r14}
	 bl _min_caml_exit
