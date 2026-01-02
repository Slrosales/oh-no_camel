.global ?v0
?v0:
	push {fp, lr}
	add fp, sp, #4
	sub sp, sp, #128
	mov r0, #10
	str r0, [fp, #-4]
	mov r0, #5
	str r0, [fp, #-8]
	ldr r0, [fp, #-4]
	push {r0}
	ldr r0, [fp, #-8]
	mov r1, r0
	pop {r0}
	sub r0, r0, r1
	sub sp, fp, #4
	pop {fp, pc}
