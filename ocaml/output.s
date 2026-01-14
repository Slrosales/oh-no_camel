.text 
.global _start

 _start:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024
	 mov r0, #1
	 str r0, [r11, #-12] @ store result v1
	 str r0, [r11, #-16] @ store result x.3
	 mov r0, #2
	 str r0, [r11, #-20] @ store result v2
	 str r0, [r11, #-24] @ store result y.4
	 ldr r0, [r11, #-16]
	 ldr r1, [r11, #-24] @ load y.4
	add r0, r0, r1
	 str r0, [r11, #-28] @ store result z.5
	 sub r13, r11, #4
	 pop {r11, r14}
