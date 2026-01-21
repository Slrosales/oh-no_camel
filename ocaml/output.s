.text
 _build_tupple.7

 _build_tupple.7:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024 @ reserve frame
	 str r0, [r11, #-12] @ save param x.10 
	 str r1, [r11, #-16] @ save param y.9 
	 str r2, [r11, #-20] @ save param z.8 
	 str r0, [r11, #-24] @ store result ?v16
	 str r0, [r11, #-28] @ store result _
	 str r0, [r11, #-28] @ store result _
	 str r0, [r11, #-28] @ store result _
	 ldr r0, [r11, #-24] @ load ?v16
	 sub r13, r11, #4
	 pop {r11, r15}
.text 
.global main

 main:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024
	 mov r0, #5
	 str r0, [r11, #-12] @ store result v3
	 mov r0, #3
	 str r0, [r11, #-16] @ store result v4
	 mov r0, #7
	 str r0, [r11, #-20] @ store result v5
	 ldr r0, [r11, #-12] @ load v3
	 ldr r1, [r11, #-16] @ load v4
	 ldr r2, [r11, #-20] @ load v5
	 bl _build_tupple.7
	 str r0, [r11, #-24] @ store result v6
	 str r0, [r11, #-28] @ store result _
	 str r0, [r11, #-28] @ store result _
	 str r0, [r11, #-28] @ store result _
	 ldr r0, [r11, #-32] @ load x.13
	 bl print_int
	 str r0, [r11, #-36] @ store result ?v1.14
	 ldr r0, [r11, #-40] @ load y.12
	 bl print_int
	 str r0, [r11, #-44] @ store result ?v2.15
	 ldr r0, [r11, #-48] @ load z.11
	 bl print_int
	 sub r13, r11, #4
	 pop {r11, r15}
