.text
.align 2
.global main

main:
	push {r11, r14}
	add r11, r13, #4
	sub r13, r13, #1024
	mov r0, #1
	str r0, [r11, #-12] @ store result x.1
	mov r0, #2
	str r0, [r11, #-16] @ store result y.2
	ldr r0, [r11, #-12]
	ldr r1, [r11, #-16] @ load y.2
	sub r0, r0, r1
	str r0, [r11, #-20] @ store result z.3
	sub r13, r11, #4
	pop {r11, r15}
