
.global _sum_array
_sum_array:
	 push {r4-r11, r12, r14}
	 mov r11, r13
	 sub r13, r13, #1024
	 mov r4, r0
	 mov r5, r1
	 mov r6, r2
	 mov r1, r5
	 mov r2, r6
	 cmp r1, r2
	 beq then_1
	 mov r1, r4
	 mov r2, r5
	 ldr r7, [r1, r2, lsl #2]
	 mov r8, #1
	 mov r1, r5
	 mov r2, r8
	 add r9, r1, r2
	 mov r0, r4
	 mov r1, r9
	 mov r2, r6
	 bl _sum_array
	 mov r10, r0
	 mov r1, r7
	 mov r2, r10
	 add r0, r1, r2
	 b else_end_0
then_1:
	 mov r0, #0
else_end_0:
	 mov r13, r11
	 pop {r4-r11, r12, r15}

.text
.global _start
_start:
	 push {r4-r11, r12, r14}
	 mov r11, r13
	 sub r13, r13, #1024
	 mov r4, #5
	 mov r0, r4
	 lsl r0, r0, #2
	 bl _min_caml_alloc
	 mov r5, r0
	 mov r6, #0
	 mov r7, #1
	 mov r8, #2
	 mov r9, #3
	 mov r10, #4
	 mov r12, #1
	 mov r0, #2
	 str r0, [r11, #-4]
	 mov r0, #3
	 str r0, [r11, #-8]
	 mov r0, #4
	 str r0, [r11, #-12]
	 mov r0, #5
	 str r0, [r11, #-16]
	 mov r1, r5
	 mov r2, r6
	 mov r3, r12
	 str r3, [r1, r2, lsl #2]
	 str r0, [r11, #-20]
	 mov r1, r5
	 mov r2, r7
	 ldr r3, [r11, #-4]
	 str r3, [r1, r2, lsl #2]
	 str r0, [r11, #-20]
	 mov r1, r5
	 mov r2, r8
	 ldr r3, [r11, #-8]
	 str r3, [r1, r2, lsl #2]
	 str r0, [r11, #-20]
	 mov r1, r5
	 mov r2, r9
	 ldr r3, [r11, #-12]
	 str r3, [r1, r2, lsl #2]
	 str r0, [r11, #-20]
	 mov r1, r5
	 mov r2, r10
	 ldr r3, [r11, #-16]
	 str r3, [r1, r2, lsl #2]
	 str r0, [r11, #-20]
	 mov r0, r5
	 mov r1, r6
	 mov r2, r4
	 bl _sum_array
	 str r0, [r11, #-24]
	 ldr r0, [r11, #-24]
	 bl _min_caml_print_int
	 bl _min_caml_print_newline
	 mov r13, r11
	 pop {r4-r11, r12, r14}
	 bl _min_caml_exit
