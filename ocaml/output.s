.text
 _getx.23

 _getx.23:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024 @ reserve frame
	 str r0, [r11, #-12] @ save param v.24 
	 str r0, [r11, #-16] @ store result x.27
	 str r0, [r11, #-20] @ store result y.26
	 str r0, [r11, #-24] @ store result z.25
	 sub r13, r11, #4
	 pop {r11, r15}
.text
 _gety.28

 _gety.28:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024 @ reserve frame
	 str r0, [r11, #-12] @ save param v.29 
	 str r0, [r11, #-16] @ store result x.32
	 str r0, [r11, #-20] @ store result y.31
	 str r0, [r11, #-24] @ store result z.30
	 sub r13, r11, #4
	 pop {r11, r15}
.text
 _getz.33

 _getz.33:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024 @ reserve frame
	 str r0, [r11, #-12] @ save param v.34 
	 str r0, [r11, #-16] @ store result x.37
	 str r0, [r11, #-20] @ store result y.36
	 str r0, [r11, #-24] @ store result z.35
	 sub r13, r11, #4
	 pop {r11, r15}
.text
 _inprod.38

 _inprod.38:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024 @ reserve frame
	 str r0, [r11, #-12] @ save param v1 
	 str r1, [r11, #-16] @ save param v2 
	 ldr r0, [r11, #-12] @ load v1
	 bl _getx.23
	 str r0, [r11, #-12] @ store result v1
	 ldr r0, [r11, #-16] @ load v2
	 bl _getx.23
	 str r0, [r11, #-16] @ store result v2
	 str r0, [r11, #-20] @ store result v3
	 ldr r0, [r11, #-12] @ load v1
	 bl _gety.28
	 str r0, [r11, #-24] @ store result v4
	 ldr r0, [r11, #-16] @ load v2
	 bl _gety.28
	 str r0, [r11, #-28] @ store result v5
	 str r0, [r11, #-32] @ store result v6
	 ldr r0, [r11, #-20]
	 ldr r1, [r11, #-32] @ load v6
	 add r0, r0, r1
	 str r0, [r11, #-36] @ store result v7
	 ldr r0, [r11, #-12] @ load v1
	 bl _getz.33
	 str r0, [r11, #-40] @ store result v8
	 ldr r0, [r11, #-16] @ load v2
	 bl _getz.33
	 str r0, [r11, #-44] @ store result v9
	 str r0, [r11, #-48] @ store result v10
	 ldr r0, [r11, #-36]
	 ldr r1, [r11, #-48] @ load v10
	 add r0, r0, r1
	 sub r13, r11, #4
	 pop {r11, r15}
.text 
.global main

 main:
	 push {r11, r14}
	 add r11, r13, #4
	 sub r13, r13, #1024
	 mov r0, #1000000
	 str r0, [r11, #-12] @ store result v11
	 mov r0, #1
	 str r0, [r11, #-16] @ store result v12
	 mov r0, #2
	 str r0, [r11, #-20] @ store result v13
	 mov r0, #3
	 str r0, [r11, #-24] @ store result v14
	 str r0, [r11, #-28] @ store result v15
	 str r0, [r11, #-32] @ store result _
	 str r0, [r11, #-32] @ store result _
	 str r0, [r11, #-32] @ store result _
	 mov r0, #4
	 str r0, [r11, #-36] @ store result v16
	 mov r0, #5
	 str r0, [r11, #-40] @ store result v17
	 mov r0, #6
	 str r0, [r11, #-44] @ store result v18
	 str r0, [r11, #-48] @ store result v19
	 str r0, [r11, #-32] @ store result _
	 str r0, [r11, #-32] @ store result _
	 str r0, [r11, #-32] @ store result _
	 ldr r0, [r11, #-28] @ load v15
	 ldr r1, [r11, #-48] @ load v19
	 bl _inprod.38
	 str r0, [r11, #-52] @ store result v20
	 str r0, [r11, #-56] @ store result v21
	 ldr r0, [r11, #-56] @ load v21
	 bl truncate
	 str r0, [r11, #-60] @ store result v22
	 ldr r0, [r11, #-60] @ load v22
	 bl _print_int
	 sub r13, r11, #4
	 pop {r11, r15}
