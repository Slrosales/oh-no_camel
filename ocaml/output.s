.text
 f.9

 f.9:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024 @ reserve frame
	 str r0, [r11, #-12] @ save param n.10 
	 mov r0, #0
	 str r0, [r11, #-16] @ store result v2
	 sub r13, r11, #4
	 pop {r11, r15}
.text 
.global main

 main:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024
	 mov r0, #9
	 str r0, [r11, #-12] @ store result v8
	 ldr r0, [r11, #-12] @ load v8
	 bl _f.9
	 sub r13, r11, #4
	 pop {r11, r15}
