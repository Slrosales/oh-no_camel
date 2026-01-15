.text 
.global _start

 _start:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024
	 mov r0, #50
	 str r0, [r11, #-12] @ store result a
	 mov r0, #2
	 str r0, [r11, #-16] @ store result b
	 mov r0, #3
	 str r0, [r11, #-20] @ store result c
	 ldr r0, [r11, #-12]
	 ldr r1, [r11, #-16] @ load b
	 mov r2, #0
subtraction_loop_0:
	 cmp r0, r1
	 blt subtraction_end_1
	 sub r0, r0, r1
	 add r2, r2, #1
	 b subtraction_loop_0
subtraction_end_1:
	 str r2, [r11, #-12]
	 mov r0, r2
	 str r0, [r11, #-24] @ store result d
	 ldr r0, [r11, #-24]
	 ldr r1, [r11, #-20] @ load c
	 mov r2, #0
subtraction_loop_2:
	 cmp r0, r1
	 blt subtraction_end_3
	 sub r0, r0, r1
	 add r2, r2, #1
	 b subtraction_loop_2
subtraction_end_3:
	 str r2, [r11, #-24]
	 mov r0, r2
	 str r0, [r11, #-28] @ store result e
	 mov r0, #4
	 str r0, [r11, #-32] @ store result f
	 ldr r0, [r11, #-28]
	 ldr r1, [r11, #-32] @ load f
	 mov r2, #0
subtraction_loop_4:
	 cmp r0, r1
	 blt subtraction_end_5
	 sub r0, r0, r1
	 add r2, r2, #1
	 b subtraction_loop_4
subtraction_end_5:
	 str r2, [r11, #-28]
	 mov r0, r2
	 str r0, [r11, #-36] @ store result g
	 ldr r0, [r11, #-36] @ load g
	 bl _min_caml_print_int
	 sub r13, r11, #4
	 pop {r11, r14}
