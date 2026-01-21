.text
 _ack.15

 _ack.15:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024 @ reserve frame
	 str r0, [r11, #-12] @ save param x.17 
	 str r1, [r11, #-16] @ save param y.16 
	 mov r0, #0
	 str r0, [r11, #-20] @ store result v1
	 sub r13, r11, #4
	 pop {r11, r15}
.text 
.global main

 main:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024
	 mov r0, #3
	 str r0, [r11, #-12] @ store result v12
	 mov r0, #10
	 str r0, [r11, #-16] @ store result v13
	 ldr r0, [r11, #-12] @ load v12
	 ldr r1, [r11, #-16] @ load v13
	 bl _ack.15
	 str r0, [r11, #-20] @ store result v14
	 ldr r0, [r11, #-20] @ load v14
	 bl print_int
	 sub r13, r11, #4
	 pop {r11, r15}
