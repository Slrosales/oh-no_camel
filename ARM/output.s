.text 
.global _start

 _start:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024
	 mov r0, #1
	 str r0, [r11, #-12] @ store result x
	 mov r0, #1
	 str r0, [r11, #-16] @ store result y
	ldr r0, [r11, #-12]
	 ldr r1, [r11, #-16] @ load y
	cmp r0, r1
	beq then_0
	bne else_1
then_0:
	 mov r0, #3
	 str r0, [r11, #-20] @ store result z
	 ldr r0, [r11, #-20] @ load z
	 bl _min_caml_print_int
	b end_2
else_1:
	 mov r0, #4
	 str r0, [r11, #-24] @ store result w
	 ldr r0, [r11, #-24] @ load w
	 bl _min_caml_print_int
end_2:
	 bl _min_caml_print_newline
	 sub r13, r11, #4
	 pop {r11, r14}
	 bl	_min_caml_exit
