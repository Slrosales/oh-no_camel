	.text
        .globl _min_caml_exit
/* exit syscall */
_min_caml_exit:
	mov	r0, #0
	mov	r7, #1  @ exit syscall
	swi	#0      @ perform syscall

	.globl _min_caml_print_newline
_min_caml_print_newline:
	stmfd	sp!, {lr}
	mov	r0, #10
	bl	_min_caml_print_char
	mov	r0, #0
	ldmfd	sp!, {lr}
        bx	lr
        .size	_min_caml_print_newline, .-_min_caml_print_newline

	.globl stringlength
stringlength:             @ note: r1 is untouched
	mov	r3, #0   @ number of char
        b	stringlength_first
stringlength_loop:
	add	r0, #1
	add	r3, #1
stringlength_first:
	ldrb	r2, [r0]  @ r2 <- current char
	cmp	r2, #0    @ r2 /= \0?
	bne	stringlength_loop	@ yes -> shift and again
        mov	r0, r3    @ return num of char
        bx	lr
        .size	stringlength, .-stringlength

	.globl _min_caml_print_string
_min_caml_print_string:
	stmfd	sp!, {lr}
        mov	r1, r0          @ save r0 in r1 untouched by stringlength
        bl	stringlength
        mov	r2, r0                   @ length of string
        mov	r0, #1                   @ on stdout
        mov	r7, #4                   @ write syscall
        swi	#0
        nop            @ why is that needed?
	ldmfd	sp!, {lr}
        bx	lr
        .size	_min_caml_print_string, .-_min_caml_print_string

	.data
hello_world_string:
	.asciz	"Hello world!\n"

	.text
	.globl _min_caml_hello_world
_min_caml_hello_world:
	stmfd	sp!, {lr}
	ldr	r0, =hello_world_string  @ the string
        bl	_min_caml_print_string
	ldmfd	sp!, {lr}
        bx	lr
	.size _min_caml_hello_world, .-_min_caml_hello_world

	.globl _min_caml_print_char
_min_caml_print_char:
	sub	sp, #4   @ room for char
        strb	r0, [sp]
	mov	r0, #1 @ file descriptor
	mov	r1, sp
	mov	r2, #1
	mov	r7, #4 @ write syscall
	swi	#0      @ perform syscall
	nop
	add	sp, #4   @ get back room for char
        bx	lr
        .size	_min_caml_print_char, .-_min_caml_print_char

	.data
.string_for_int:
	.asciz "-2147483648"    @ 12 char are sufficient (for 32-bit ints, incl '\0')

@ Warning: does not work for -2147483648
	.text
	.globl	stringofint
stringofint:
	stmfd	sp!, {r4, r5}
	mov	r3, r0    @ the number
	ldr	r0, =.string_for_int  @ pointer to begining of buffer
        add	r0, #11     @ pointer to end of buffer (char '\0')
      @special case for -2147483648
	ldr	r2, .i2s_constants+4
        cmp	r3, r2
        moveq	r1, #56     @ char '8'
        subeq	r0, #1
        streq	r1, [r0]
        ldreq	r3, .i2s_constants+8
      @special case for 0
        cmp	r3, #0
        bne	.i2s_non_zero @ here number is zero
        moveq	r1, #48     @ char '0'
        subeq	r0, #1
        streq	r1, [r0]
        beq	.i2s_exit
.i2s_non_zero:
	movgt	r5, #43    @ char '+'
        movlt	r5, #45    @ char '-'
	rsblt	r3, r3, #0 @ get absolute number
	ldr	r4, .i2s_constants  @ load before loop
.i2s_positive:
      @ tricky stuff to divide r3 and get modulo 10
	smull	r2, r1, r4, r3
	mov	r2, r3, asr #31
	rsb	r2, r2, r1, asr #2
	add	r1, r2, r2, asl #2
	sub	r3, r3, r1, asl #1
	add	r3, r3, #48  @ add '0' to modulo
        sub	r0, #1
	strb	r3, [r0]
	mov	r3, r2       @ prepare result of division for next step
	cmp	r2, #0
        bne	.i2s_positive
        cmp	r5, #45    @ char '-'
        subeq	r0, #1
        streqb	r5, [r0]
.i2s_exit:
	ldmfd	sp!, {r4, r5}
        bx	lr
.i2s_constants:
	.word	1717986919
	.word	-2147483648
	.word	-214748364
	.size	stringofint, .-stringofint
        .align	2


        .globl _min_caml_print_int
_min_caml_print_int:
	stmfd	sp!, {lr}
	bl	stringofint
	bl	_min_caml_print_string
	ldmfd	sp!, {lr}
        bx	lr
        .size _min_caml_print_int, .-_min_caml_print_int




.data
    .align 4
heap_ptr:
    .word heap_start      @ Adress of next free byte
heap_start:
    .space 20000          @ 20k byte heap (Arbitrary value)




.text
    .globl _min_caml_alloc
_min_caml_alloc:
    ldr r1, =heap_ptr     @ Get the address of our pointer 
    ldr r2, [r1]          @ Load the actual current heap address into r2
    mov r3, r2            @ Save current address 
    add r2, r2, r0        @ Add the requested bytes to the pointer
    str r2, [r1]          @ Save new pointer back to memory
    mov r0, r3            @ Put the original start address in r0 for return
    bx  lr                @ Return to caller
    .size _min_caml_alloc, .-_min_caml_alloc



.globl _min_caml_create_array
_min_caml_create_array:
    stmfd sp!, {r4, r5, lr} @ Save registers
    mov r4, r0              @ Save size
    mov r5, r1              @ Save the initial value in r5
    
    @ Convert number of elements to bytes (elements * 4)
    lsl r0, r4, #2          @ Shift left by 2
    
    @ Allocate the memory
    bl _min_caml_alloc     
    
    @ Fill the array with the initial value
    mov r2, #0              @ i = 0 (loop counter)
.l_init_loop:
    cmp r2, r4              @ Check if i == size
    beq .l_init_done        @ If yes, exit loop
    

    str r5, [r0, r2, lsl #2] 
    
    add r2, r2, #1          @ i++
    b .l_init_loop          @ Repeat
    
.l_init_done:
    ldmfd sp!, {r4, r5, lr} @ Restore registers
    bx lr                   @ Return
    .size _min_caml_create_array, .-_min_caml_create_array
