.text
.global main
main:
	push {fp, lr}
	add fp, sp, #4
	sub sp, sp, #1024
	mov r0, #10
	str r0, [fp, #-4]
	mov r0, #5
	str r0, [fp, #-8]
	ldr r0, [fp, #-4]
	str r0, [fp, #-12]
	ldr r0, [fp, #-8]
	str r0, [fp, #-16]
	ldr r0, [fp, #-12]
	ldr r1, [fp, #-16]
	sub r0, r0, r1
	sub sp, fp, #4
	pop {fp, pc}
