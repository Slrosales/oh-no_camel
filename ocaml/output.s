.text
.align 2
.global main

main:
	push {r11, r14}
	add r11, r13, #4
	sub r13, r13, #1024
	mov r0, #1
	str r0, [r11, #-12] @ store result v1
	mov r0, #3
	str r0, [r11, #-16] @ store result v2
	mov r0, #3
	str r0, [r11, #-20] @ store result v3
	str r0, [r11, #-24] @ store result v4
	str r0, [r11, #-28] @ store result x.5
	sub r13, r11, #4
	pop {r11, r15}
