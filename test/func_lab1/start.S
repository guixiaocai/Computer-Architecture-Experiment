#include <asm.h>
#include <regdef.h>
#include <ucas_cde.h>

#define TEST_NUM 19

##s0, number
##s1, number adress 
##s2, exception use
##s3, score
##s4, exception pc
	.set	noreorder
	.globl	_start
	.globl	start
	.globl	__main
_start:
start:

	b	locate
	nop

##avoid "j locate" not taken
    lui   t0, 0x8000
    addiu t1, t1, 1
    or    t2, t0, zero
    addu  t3, t5, t6
    lw    t4, 0(t0)
    nop

##avoid cpu run error
.org 0x0ec
    lui   t0, 0x8000
    addiu t1, t1, 1
    or    t2, t0, zero
    addu  t3, t5, t6
    lw    t4, 0(t0)
.org 0x100
test_finish:
    addiu t0, t0, 1
    li t1, 0xff
    LI (t2, UART_ADDR)
    sb t1, 0x0(t2)
    b test_finish
    nop
##avoid cpu run error
    lui   t0, 0x8000
    addiu t1, t1, 1
    or    t2, t0, zero
    addu  t3, t5, t6
    lw    t4, 0(t0)
/*
 *  exception handle
 */
.org 0x380
1:  
    addiu t0, t0, 1
    b 1b
    nop


locate:
	.set noreorder

    LI (a0, LED_RG1_ADDR)
    LI (a1, LED_RG0_ADDR)
    LI (a2, LED_ADDR)
    LI (s1, NUM_ADDR)

    LI (t1, 0x0002)
    LI (t2, 0x0001)
    LI (t3, 0x0000ffff)
    lui s3, 0

    sw t1, 0(a0)
    sw t2, 0(a1)
    sw t3, 0(a2)
    sw s3, 0(s1)
    lui s0, 0
inst_test:
    jal n1_lui_test    #lui
    nop
    jal wait_1s
    nop
    jal n2_addu_test   #addu
    nop
    jal wait_1s
    nop
    jal n3_addiu_test  #addiu
    nop
    jal wait_1s
    nop
    jal n4_subu_test   #subu
    nop
    jal wait_1s
    nop
    jal n5_slt_test    #slt
    nop
    jal wait_1s
    nop
    jal n6_sltu_test   #sltu
    nop
    jal wait_1s
    nop
    jal n7_and_test    #and
    nop
    jal wait_1s
    nop
    jal n8_or_test     #or
    nop
    jal wait_1s
    nop
    jal n9_xor_test    #xor
    nop
    jal wait_1s
    nop
    jal n10_nor_test   #nor
    nop
    jal wait_1s
    nop
    jal n11_sll_test   #sll
    nop
    jal wait_1s
    nop
    jal n12_srl_test   #srl
    nop
    jal wait_1s
    nop
    jal n13_sra_test   #sra
    nop
    jal wait_1s
    nop
    jal n14_lw_test    #lw
    nop
    jal wait_1s
    nop
    jal n15_sw_test    #sw
    nop
    jal wait_1s
    nop
    jal n16_beq_test   #beq
    nop
    jal wait_1s
    nop
    jal n17_bne_test   #bne
    nop
    jal wait_1s
    nop
    jal n18_jal_test   #jal
    nop
    jal wait_1s
    nop
    jal n19_jr_test    #jr
    nop
    jal wait_1s
    nop
    jal wait_1s
    nop


test_end:
    LI  (s0, TEST_NUM)
    beq s0, s3, 1f
    nop

    LI (a0, LED_ADDR)
	LI (a1, LED_RG1_ADDR)
    LI (a2, LED_RG0_ADDR)
	
    LI (t1, 0x0002)
    
	sw zero, 0(a0)
    sw t1, 0(a1)
    sw t1, 0(a2)
    b  2f
    nop
1:
    LI (t1, 0x0001)
    LI (a0, LED_RG1_ADDR)
	LI (a1, LED_RG0_ADDR)
    sw t1, 0(a0)
    sw t1, 0(a1)

2:
	//LI (t1, 0xff)
	//LI (t0, UART_ADDR)
	//sw t1, 0(t0)

	jal test_finish
    nop

wait_1s:
    LI (t1,SIMU_FLAG_ADDR)
    lui   t0, 0x0
    lw   t2, 0x0(t1)
    bne  t2, zero, 1f
    nop
    LI (t0,SWITCH_ADDR)
    lw  t0, 0x0(t0)   #switch[7:0]
    LI (t1, 0xff)
    xor t0, t0, t1
    sll t0, t0, 16    #t0 = switch<<16
1:
    addiu t0, 1
2:
    addiu t0, -1
    bne t0,zero, 2b
    nop
    jr ra
    nop
