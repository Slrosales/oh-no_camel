
.global _getx.23
_getx.23:
	 push {r4-r11, r12, r14}
	 mov r11, r13
	 sub r13, r13, #1024
	 mov r4, r0
	 mov r1, r4
	 mov r2, #0
	 ldr r5, [r1, r2, lsl #2]
	 mov r1, r4
	 mov r2, #4
	 ldr r6, [r1, r2, lsl #2]
	 mov r1, r4
	 mov r2, #8
	 ldr r7, [r1, r2, lsl #2]
	 mov r13, r11
	 pop {r4-r11, r12, r15}

.global _gety.28
_gety.28:
	 push {r4-r11, r12, r14}
	 mov r11, r13
	 sub r13, r13, #1024
	 mov r4, r0
	 mov r1, r4
	 mov r2, #0
	 ldr r5, [r1, r2, lsl #2]
	 mov r1, r4
	 mov r2, #4
	 ldr r6, [r1, r2, lsl #2]
	 mov r1, r4
	 mov r2, #8
	 ldr r7, [r1, r2, lsl #2]
	 mov r13, r11
	 pop {r4-r11, r12, r15}

.global _getz.33
_getz.33:
	 push {r4-r11, r12, r14}
	 mov r11, r13
	 sub r13, r13, #1024
	 mov r4, r0
	 mov r1, r4
	 mov r2, #0
	 ldr r5, [r1, r2, lsl #2]
	 mov r1, r4
	 mov r2, #4
	 ldr r6, [r1, r2, lsl #2]
	 mov r1, r4
	 mov r2, #8
	 ldr r7, [r1, r2, lsl #2]
	 mov r13, r11
	 pop {r4-r11, r12, r15}

.global _inprod.38
_inprod.38:
	 push {r4-r11, r12, r14}
	 mov r11, r13
	 sub r13, r13, #1024
	 mov r4, r0
	 mov r5, r1
	 mov r0, r4
	 bl _getx.23
	 mov r4, r0
	 mov r0, r5
	 bl _getx.23
	 mov r5, r0
	 mov r1, r4
	 mov r2, r5
	 mul r6, r1, r2
	 mov r0, r4
	 bl _gety.28
	 mov r7, r0
	 mov r0, r5
	 bl _gety.28
	 mov r8, r0
	 mov r1, r7
	 mov r2, r8
	 mul r9, r1, r2
	 mov r1, r6
	 mov r2, r9
	 add r10, r1, r2
	 mov r0, r4
	 bl _getz.33
	 mov r12, r0
	 mov r0, r5
	 bl _getz.33
	 str r0, [r11, #-4]
	 mov r1, r12
	 ldr r2, [r11, #-4]
	 mul r0, r1, r2
	 str r0, [r11, #-8]
	 mov r1, r10
	 ldr r2, [r11, #-8]
	 add r0, r1, r2
	 mov r13, r11
	 pop {r4-r11, r12, r15}

.text
.global _start
_start:
	 push {r4-r11, r12, r14}
	 mov r11, r13
	 sub r13, r13, #1024
	 mov r4, #1000000
	 mov r5, #1
	 mov r6, #2
	 mov r7, #3
	 mov r0, #12
	 lsl r0, r0, #2
	 bl _min_caml_alloc
	 mov r8, r0
	 mov r1, r8
	 mov r2, #0
	 mov r3, r5
	 str r3, [r1, r2, lsl #2]
	 mov r1, r8
	 mov r2, #4
	 mov r3, r6
	 str r3, [r1, r2, lsl #2]
	 mov r1, r8
	 mov r2, #8
	 mov r3, r7
	 str r3, [r1, r2, lsl #2]
	 mov r10, #4
	 mov r12, #5
	 mov r0, #6
	 str r0, [r11, #-4]
	 mov r0, #12
	 lsl r0, r0, #2
	 bl _min_caml_alloc
	 str r0, [r11, #-8]
	 ldr r1, [r11, #-8]
	 mov r2, #0
	 mov r3, r10
	 str r3, [r1, r2, lsl #2]
	 ldr r1, [r11, #-8]
	 mov r2, #4
	 mov r3, r12
	 str r3, [r1, r2, lsl #2]
	 ldr r1, [r11, #-8]
	 mov r2, #8
	 ldr r3, [r11, #-4]
	 str r3, [r1, r2, lsl #2]
	 mov r0, r8
	 ldr r1, [r11, #-8]
	 bl _inprod.38
	 str r0, [r11, #-12]
	 mov r1, r4
	 ldr r2, [r11, #-12]
	 mul r0, r1, r2
	 str r0, [r11, #-16]
	 ldr r0, [r11, #-16]
	 bl truncate
	 str r0, [r11, #-20]
	 ldr r0, [r11, #-20]
	 bl _print_int
	 bl _min_caml_print_newline
	 mov r13, r11
	 pop {r4-r11, r12, r14}
	 bl _min_caml_exit
