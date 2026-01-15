.text 
.global _start

 _start:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024
<<<<<<< HEAD
	 mov r0, #1000
	 str r0, [r11, #-12] @ store result a
	 mov r0, #2
	 str r0, [r11, #-16] @ store result b
	 mov r0, #15
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
	 mov r0, #2
	 str r0, [r11, #-32] @ store result f
	 mov r0, #4
	 str r0, [r11, #-36] @ store result g
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
	 str r0, [r11, #-40] @ store result h
	 ldr r0, [r11, #-40]
	 ldr r1, [r11, #-36] @ load g
	 mov r2, #0
subtraction_loop_6:
	 cmp r0, r1
	 blt subtraction_end_7
	 sub r0, r0, r1
	 add r2, r2, #1
	 b subtraction_loop_6
subtraction_end_7:
	 str r2, [r11, #-40]
	 mov r0, r2
	 str r0, [r11, #-44] @ store result i
	 ldr r0, [r11, #-44] @ load i
=======
	 mov r0, #3
	 str r0, [r11, #-12] @ store result size
	 mov r0, #0
	 str r0, [r11, #-16] @ store result init
	 ldr r0, [r11, #-12] @ load size
	 ldr r1, [r11, #-16] @ load init
	 bl _min_caml_create_array
	 str r0, [r11, #-20] @ store result arr
	 mov r0, #0
	 str r0, [r11, #-24] @ store result i0
	 mov r0, #15
	 str r0, [r11, #-28] @ store result v0
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-24] @ load i0
	 ldr r2, [r11, #-28]
	 str r2, [r0, r1, lsl #2]
	 str r0, [r11, #-32] @ store result d1
	 mov r0, #1
	 str r0, [r11, #-36] @ store result i1
	 mov r0, #5
	 str r0, [r11, #-40] @ store result v1
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-36] @ load i1
	 ldr r2, [r11, #-40]
	 str r2, [r0, r1, lsl #2]
	 str r0, [r11, #-44] @ store result d2
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-24] @ load i0
	 ldr r0, [r0, r1, lsl #2]
	 str r0, [r11, #-48] @ store result a
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-36] @ load i1
	 ldr r0, [r0, r1, lsl #2]
	 str r0, [r11, #-52] @ store result b
	 ldr r0, [r11, #-48]
	 ldr r1, [r11, #-52] @ load b
	add r0, r0, r1
	 str r0, [r11, #-56] @ store result sum
	 mov r0, #10
	 str r0, [r11, #-60] @ store result ten
	 ldr r0, [r11, #-56]
	 ldr r1, [r11, #-60] @ load ten
	sub r0, r0, r1
	 str r0, [r11, #-64] @ store result res
	 mov r0, #2
	 str r0, [r11, #-68] @ store result i2
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-68] @ load i2
	 ldr r2, [r11, #-64]
	 str r2, [r0, r1, lsl #2]
	 str r0, [r11, #-72] @ store result d3
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-68] @ load i2
	 ldr r0, [r0, r1, lsl #2]
	 str r0, [r11, #-76] @ store result final
	 ldr r0, [r11, #-76] @ load final
>>>>>>> 2a0ffeec2f2ac532f840bfba45c8450bd4061b37
	 bl _min_caml_print_int
	 bl _min_caml_print_newline
	 sub r13, r11, #4
	 pop {r11, r14}
	 bl	_min_caml_exit
