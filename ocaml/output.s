.text
.align 2
.global main

main:
	push {r11, r14}
	add r11, r13, #4
	sub r13, r13, #1024
	mov r0, #5
	str r0, [r11, #-12] @ store result z
	mov r0, #3
	str r0, [r11, #-16] @ store result w
	mov r0, #5
	str r0, [r11, #-20] @ store result v1
	ldr r0, [r11, #-12]
	ldr r1, [r11, #-20] @ load v1
	add r0, r0, r1
	str r0, [r11, #-24] @ store result v2
	ldr r0, [r11, #-24]
	ldr r1, [r11, #-16] @ load w
	sub r0, r0, r1
	str r0, [r11, #-28] @ store result x
	mov r0, #3
	str r0, [r11, #-32] @ store result v3
	ldr r0, [r11, #-12]
	ldr r1, [r11, #-32] @ load v3
	add r0, r0, r1
	str r0, [r11, #-36] @ store result v4
	ldr r0, [r11, #-36]
	ldr r1, [r11, #-16] @ load w
	sub r0, r0, r1
	str r0, [r11, #-40] @ store result y
	ldr r0, [r11, #-28]
	ldr r1, [r11, #-40] @ load y
	sub r0, r0, r1
	str r0, [r11, #-44] @ store result v5
	ldr r0, [r11, #-44]
	ldr r1, [r11, #-12] @ load z
	add r0, r0, r1
	str r0, [r11, #-48] @ store result v6
	ldr r0, [r11, #-48]
	ldr r1, [r11, #-16] @ load w
	sub r0, r0, r1
	sub r13, r11, #4
	pop {r11, r15}
