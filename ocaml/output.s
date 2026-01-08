.text
.align 2
.global main

main:
	push {r11, r14}
	add r11, r13, #4
	sub r13, r13, #1024
	mov r0, #1
	str r0, [r11, #-12] @ store result a
	mov r0, #2
	str r0, [r11, #-16] @ store result b
	mov r0, #3
	str r0, [r11, #-20] @ store result c
	ldr r0, [r11, #-12]
	ldr r1, [r11, #-16] @ load b
	sub r0, r0, r1
	str r0, [r11, #-24] @ store result v1
	ldr r0, [r11, #-24]
	ldr r1, [r11, #-20] @ load c
	sub r0, r0, r1
	str r0, [r11, #-28] @ store result d
	mov r0, #4
	str r0, [r11, #-32] @ store result e
	ldr r0, [r11, #-28]
	ldr r1, [r11, #-32] @ load e
	sub r0, r0, r1
	str r0, [r11, #-36] @ store result f
	sub r13, r11, #4
	pop {r11, r15}
