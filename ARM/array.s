
.text
.global _start
_start:
	 push {r4-r10, r11, r14}
	 add r11, r13, #32
	 sub r13, r13, #1024
	 mov r4, #3
	 mov r5, #0
	 mov r0, r4
	 mov r1, r5
	 bl _min_caml_create_array
	 mov r6, r0
	 mov r7, #0
	 mov r8, #15
	 mov r0, r6
	 mov r1, r7
	 mov r2, r8
	 str r2, [r0, r1, lsl #2]
	 mov r10, #1
	 mov r0, #5
	 str r0, [r11, #4]
	 mov r0, r6
	 mov r1, r10
	 ldr r2, [r11, #4]
	 str r2, [r0, r1, lsl #2]
	 str r0, [r11, #8]
	 mov r0, r6
	 mov r1, r7
	 ldr r0, [r0, r1, lsl #2]
	 str r0, [r11, #12]
	 mov r0, r6
	 mov r1, r10
	 ldr r0, [r0, r1, lsl #2]
	 str r0, [r11, #16]
	 ldr r2, [r11, #12]
	 ldr r3, [r11, #16]
	 add r0, r2, r3
	 str r0, [r11, #20]
	 mov r0, #10
	 str r0, [r11, #24]
	 ldr r2, [r11, #20]
	 ldr r3, [r11, #24]
	 sub r0, r2, r3
	 str r0, [r11, #28]
	 mov r0, #2
	 str r0, [r11, #32]
	 mov r0, r6
	 ldr r1, [r11, #32]
	 ldr r2, [r11, #28]
	 str r2, [r0, r1, lsl #2]
	 str r0, [r11, #36]
	 mov r0, r6
	 ldr r1, [r11, #32]
	 ldr r0, [r0, r1, lsl #2]
	 str r0, [r11, #40]
	 ldr r0, [r11, #40]
	 bl _min_caml_print_int
	 bl _min_caml_print_newline
	 sub r13, r11, #32
	 pop {r4-r10, r11, r14}
	 bl _min_caml_exit
