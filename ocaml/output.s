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
	ldr r0, [fp, #-4]
	ldr r1, [fp, #-8]
	cmp r0, r1
	bgt _else_9344
	ldr r0, [fp, #-4]
	str r0, [fp, #-12]
	mov r0, #5
	str r0, [fp, #-16]
	ldr r0, [fp, #-12]
	ldr r1, [fp, #-16]
	add r0, r0, r1
	str r0, [fp, #-20]
	ldr r0, [fp, #-20]
	b _end_9344
_else_9344:
	ldr r0, [fp, #-8]
	str r0, [fp, #-24]
	mov r0, #5
	str r0, [fp, #-28]
	ldr r0, [fp, #-24]
	ldr r1, [fp, #-28]
	sub r0, r0, r1
	str r0, [fp, #-32]
	ldr r0, [fp, #-32]
_end_9344:
	str r0, [fp, #-36]
	ldr r0, [fp, #-36]
	sub sp, fp, #4
	pop {fp, pc}
