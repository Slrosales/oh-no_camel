.text 
.global _start

 _start:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024
	 mov r0, #0
	 str r0, [r11, #-12] @ store result x
	 ldr r0, [r11, #-12] @ load x
	 bl _min_caml_print_int
	 sub r13, r11, #4
	 pop {r11, r14}
