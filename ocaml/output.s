
.global _ack.15
_ack.15:
	 push {r4-r11, r12, r14}
	 mov r11, r13
	 sub r13, r13, #1024
	 mov r4, r0
	 mov r5, r1
	 mov r6, #0
	 mov r1, r4
	 mov r2, r6
	 cmp r1, r2
	 ble then_1
	 mov r8, #0
	 mov r1, r5
	 mov r2, r8
	 cmp r1, r2
	 ble then_3
	 mov r0, #1
	 str r0, [r11, #-4]
	 mov r1, r4
	 ldr r2, [r11, #-4]
	 sub r0, r1, r2
	 str r0, [r11, #-8]
	 mov r0, #1
	 str r0, [r11, #-12]
	 mov r1, r5
	 ldr r2, [r11, #-12]
	 sub r0, r1, r2
	 str r0, [r11, #-16]
	 mov r0, r4
	 ldr r1, [r11, #-16]
	 bl _ack.15
	 str r0, [r11, #-20]
	 ldr r0, [r11, #-8]
	 ldr r1, [r11, #-20]
	 bl _ack.15
	 b else_end_2
then_3:
	 mov r9, #1
	 mov r1, r4
	 mov r2, r9
	 sub r10, r1, r2
	 mov r12, #1
	 mov r0, r10
	 mov r1, r12
	 bl _ack.15
else_end_2:
	 b else_end_0
then_1:
	 mov r7, #1
	 mov r1, r5
	 mov r2, r7
	 add r0, r1, r2
else_end_0:
	 mov r13, r11
	 pop {r4-r11, r12, r15}

.text
.global _start
_start:
	 push {r4-r11, r12, r14}
	 mov r11, r13
	 sub r13, r13, #1024
	 mov r4, #3
	 mov r5, #10
	 mov r0, r4
	 mov r1, r5
	 bl _ack.15
	 mov r6, r0
	 mov r0, r6
	 bl _min_caml_print_int
	 bl _min_caml_print_newline
	 mov r13, r11
	 pop {r4-r11, r12, r14}
	 bl _min_caml_exit
