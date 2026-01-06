.text
.global main
main:
	push {fp, lr}
	add fp, sp, #4
	sub sp, sp, #1024
	mov r0, #10
	str r0, [fp, #-4]
	mov r0, #20
	str r0, [fp, #-8]
	mov r0, #30
	str r0, [fp, #-12]
	mov r0, #5
	str r0, [fp, #-16]
	ldr r0, [fp, #-4]
	ldr r1, [fp, #-8]
	bl add
	str r0, [fp, #-20]
	ldr r0, [fp, #-12]
	ldr r1, [fp, #-16]
	bl add
	str r0, [fp, #-24]
	ldr r0, [fp, #-20]
	ldr r1, [fp, #-24]
	bl add
	str r0, [fp, #-28]
	ldr r0, [fp, #-28]
	sub sp, fp, #4
	pop {fp, pc}
