.text
 _f

 _f:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024 @ reserve frame
	 str r0, [r11, #-12] @ save param x 
	 str r1, [r11, #-16] @ save param y 
	 ldr r0, [r11, #-12]
	 ldr r1, [r11, #-16] @ load y
	 add r0, r0, r1
	 str r0, [r11, #-20] @ store result z
	 mov r0, #2
	 str r0, [r11, #-24] @ store result t
	 ldr r0, [r11, #-24]
	 ldr r1, [r11, #-20] @ load z
	 sub r0, r0, r1
	 sub r13, r11, #4
	 pop {r11, r15}
.text 
.global _start

 _start:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024
	 mov r0, #0
	 str r0, [r11, #-12] @ store result x
	 mov r0, #1
	 str r0, [r11, #-16] @ store result y
	 ldr r0, [r11, #-12] @ load x
	 ldr r1, [r11, #-16] @ load y
	 bl _f
	 str r0, [r11, #-20] @ store result z
	 ldr r0, [r11, #-20] @ load z
	 bl _min_caml_print_int
	 sub r13, r11, #4
	 pop {r11, r14}
