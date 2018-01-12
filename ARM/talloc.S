@ To get the break address we perform an illegal brk statement.
@ On failure it returns the current break address.
.macro get_break
    mov r0, #0
    mov r7, #45
    swi #0
.endm

@ Macro to call the brk syscall on a given address.
.macro brk addr
    mov r0, \addr
    mov r7, #45
    swi #0
.endm

.data
.balign 4
heapsize: .word 0

.text
.global talloc
talloc:
    stmfd sp!, {r4,r5,r6,r7,r8,lr}  @ Saving registers
    mov r5, r0              @ Saving r0 (size) to use it later
    @ Increases the heap size

    @ Puts in r0 the current break
    get_break               @ Macro to get to current break of the heap
    mov r8, r0              @ Store the base address of current break in r8

    @ Adds the heap size to the current break
    ldr r3, _heapsize       @ Load address of variable
    ldr r3, [r3]            @ Load value of address
    add r4, r8, r5, LSL #2  @ Add to it the current break 
    
    @ Calls the syscall brk
    brk r4                  @ Sets the break to r4 = old break + size to alloc

    @ Initialises the values
    @ We will loop from 0 to size (stored in r1)
    @ Each time we write in adress base + offset
    mov r3, #0              @ Init counter
    loop:   
        cmp r3, r5          @ Look if we went through the segment
        beq end
        add r7, r8, r3, LSL #2  @ Get element index (has to be multiple of 4)
        str r1, [r7]            @ Store init value into element
        add r3, r3, #1          @ Increment loop counter
        b loop
    end:

    @ Returns the previous break
    mov r0, r8
    ldmfd sp!, {r4,r5,r6,r7,r8,lr}
    bx lr

_heapsize: .word heapsize
