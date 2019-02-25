#include <asm.h>
#include <regdef.h>
#include <ucas_cde.h>

##s0, number
##s1, number adress
##s2, exception use
##s3, score
##s4, exception pc

##set interrupt
.macro STI
		mfc0 k0, CP0_STATUS
		nop
		ori  k0, k0, 0x1
		mtc0 k0, CP0_STATUS
		nop
.endm
##clear interrupt
.macro CLI
		mfc0  k0, CP0_STATUS
		nop
		addiu k1, zero, 0xfffe
		and   k0, k0, k1
		mtc0  k0, CP0_STATUS
		nop
.endm

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
    sw t1, 0x0(t2)
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
exception_handler:
    CLI
		mfc0 k0, CP0_CAUSE
		andi k0, 0xff00
		mfc0 k1, CP0_STATUS
		andi k1, 0xff00
		and  k0, k0, k1

#根据中断原因跳转到中断处理函数
		andi k1, k0, 0x8000
		bnez k1, time
		nop
		andi k1, k0, 0x4000
		bnez k1, watch_reset
		nop
		andi k1, k0, 0x2000
		bnez k1, watch_h
		nop
		andi k1, k0, 0x1000
		bnez k1, watch_min
		nop
		andi k1, k0, 0x800
		bnez k1, watch_s
		nop
time:
    jal do_time
		nop
		la k0, loop
		mtc0 k0, CP0_EPC
		nop
		lui k0, 0x17d
		ori k0, k0, 0x7840
//		li k0, 50
		mtc0 k0, CP0_COMPARE
		nop
		mtc0 zero, CP0_COUNT
		nop
		STI
		eret

watch_reset:
    la k0, watch_stat
		lw k1, 0(k0)
		beqz k1, stat_rst
		nop
		li k1, 0
		j stat_nor
		nop
stat_rst:
    li k1, 1
		mfc0 t0, CP0_STATUS
		nop
		ori  t0, t0, 0x7800   #屏蔽时钟中断，并开启设置按键的中断
		li   a0, 0xffff7fff
		and  t0, t0, a0
		mtc0  t0, CP0_STATUS
		nop
		li t0, 1
    mtc0 t0, CP0_COMPARE
		nop
		li  t0, 2
    mtc0 t0, CP0_COUNT
		nop
		j return_r
		nop
stat_nor:
    mfc0 t0, CP0_STATUS
    nop
    ori t0, t0, 0xc000    #开时钟中断，屏蔽设置按键中断
    andi t0, t0, 0xc7ff
		mtc0 t0, CP0_STATUS
		nop
		lui t0, 0x17d
		ori t0, t0, 0x7840
//		li t0, 50
    mtc0 t0, CP0_COMPARE
		nop
    mtc0 zero, CP0_COUNT
		nop
return_r:
    sw k1, 0(k0)
		la k1, loop
		mtc0 k1, CP0_EPC
		nop
		STI
		eret

/*
 *设置状态下设置时
 */
watch_h:
    la t0, watch_stat
		lw t1, 0(t0)
		beqz t1, return_h
    nop
hour_set:
    mfc0 k0, CP0_CAUSE
		nop
		andi k0, k0, 0x2000
		beqz k0, return_h
		nop
		li t2, NUM_ADDR
		lw t3, 0(t2)
		move t5, t3
		lui t4, 0xff
		and t3, t3, t4
		srl t3, t3, 16
		ori t0, zero, 0xffff
		and t5, t5, t0
    li t0, 0
//		li t1, 50
    li t1, 0x7a1200
loop_h:
    addiu t0, t0, 1
		bne t0, t1, loop_h     #计数到达0x7a1200时，时间增加一个单位
    nop
		addiu t3, t3, 1
		move  t4, t3
		addiu t4, t4, -0xa
		bltz  t4, 19f
		nop
		beqz  t4, 20f
		nop
		move  t4, t3
		addiu t4, t4, -0x1a
		bltz  t4, 19f
		nop
		beqz  t4, 20f
		nop
		move  t4, t3
		addiu t4, t4, -0x24
		bltz  t4, 19f
		nop
		beqz  t4, 22f
		nop
19:
		j combine_h
		nop
20:
		addiu t3, t3, 6
		j combine_h
		nop
22:
		lui t3, 0x0
combine_h:
    sll t3, t3, 16
		or t3, t3, t5
		sw  t3, 0(t2)
		j hour_set
		nop
return_h:
    STI
		eret

/*
 *设置状态下设置分
 */
watch_min:
    la t0, watch_stat
    lw t1, 0(t0)
    beqz t1, return_min
    nop
minute_set:
    mfc0 k0, CP0_CAUSE
    nop
    andi k0, k0, 0x1000
    beqz k0, return_min
    nop
    li t2, NUM_ADDR
    lw t3, 0(t2)
		move t5, t3
		andi t3, t3, 0xff00
    srl t3, t3, 8
		lui t0, 0xff
		ori t0, t0, 0xff
		and t5, t5, t0
    li t0, 0
//    li t1, 50
    li t1, 0x7a1200
loop_min:
    addiu t0, t0, 1
    bne t0, t1, loop_min
    nop
    addiu t3, t3, 1
    move  t4, t3
    addiu t4, t4, -0xa
    bltz  t4, 23f
    nop
		beqz  t4, 24f
		nop
    move  t4, t3
    addiu t4, t4, -0x1a
    bltz  t4, 23f
    nop
		beqz  t4, 24f
		nop
    move  t4, t3
    addiu t4, t4, -0x2a
    bltz  t4, 23f
    nop
		beqz  t4, 24f
		nop
		move  t4, t3
    addiu t4, t4, -0x3a
    bltz  t4, 23f
    nop
		beqz  t4, 24f
		nop
		move  t4, t3
    addiu t4, t4, -0x4a
    bltz  t4, 23f
    nop
		beqz  t4, 24f
		nop
		move  t4, t3
    addiu t4, t4, -0x5a
    bltz  t4, 23f
    nop
    beqz  t4, 29f
    nop
23:
    j combine_min
    nop
24:
    addiu t3, t3, 6
    j combine_min
    nop
29:
    lui t3, 0x0
combine_min:
    sll t3, t3, 8
		or t3, t3, t5
    sw  t3, 0(t2)
    j minute_set
    nop
return_min:
    STI
    eret
/*
 *设置状态下设置秒
 */
watch_s:
		la t0, watch_stat
		lw t1, 0(t0)
		beqz t1, return_s
		nop
second_set:
		mfc0 k0, CP0_CAUSE
		nop
		andi k0, k0, 0x800
		beqz k0, return_s
		nop
		li t2, NUM_ADDR
		lw t3, 0(t2)
		move t5, t3
    andi t3, t3, 0xff
		lui t0, 0xff
		ori t0, t0, 0xff00
		and t5, t5, t0
		li t0, 0
//		li t1, 50
    li t1, 0x7a1200
loop_second:
		addiu t0, t0, 1
		bne t0, t1, loop_second
		nop
		addiu t3, t3, 1
		move  t4, t3
		addiu t4, t4, -0xa
		bltz  t4, 30f
		nop
		beqz  t4, 31f
		nop
		move  t4, t3
		addiu t4, t4, -0x1a
		bltz  t4, 30f
		nop
		beqz  t4, 31f
		nop
		move  t4, t3
		addiu t4, t4, -0x2a
		bltz  t4, 30f
		nop
		beqz  t4, 31f
		nop
    move  t4, t3
		addiu t4, t4, -0x3a
		bltz  t4, 30f
		nop
		beqz  t4, 31f
		nop
    move  t4, t3
		addiu t4, t4, -0x4a
		bltz  t4, 30f
		nop
		beqz  t4, 31f
		nop
    move  t4, t3
		addiu t4, t4, -0x5a
		bltz  t4, 30f
		nop
		beqz  t4, 36f
		nop
30:
		j combine_s
		nop
31:
		addiu t3, t3, 6
		j combine_s
		nop
36:
		lui t3, 0x0
combine_s:
    or t3, t3, t5
		sw  t3, 0(t2)
		j second_set
		nop
return_s:
		STI
		eret

EXPORT(watch_stat)       #判断是否为设置状态，为0时是计时状态，为1时是设置状态
.word 0x0

locate:
.set noreorder
#初始化
    CLI
    LI (a0, LED_RG1_ADDR)
    LI (a1, LED_RG0_ADDR)
    LI (a2, LED_ADDR)
    LI (s1, NUM_ADDR)

    LI (t1, 0x0001)
    LI (t2, 0x0001)
    LI (t3, 0x0000ffff)
    lui s3, 0x23
		ori s3, 0x5955

    sw t1, 0(a0)
    sw t2, 0(a1)
    sw t3, 0(a2)
    sw s3, 0(s1)

watch_begin:
############################
    lui t0, 0x40
	  ori t0, t0, 0xc000
		mtc0 t0, CP0_STATUS
		nop
	  mfc0 t0, CP0_CAUSE
	  nop
  	lui t1, 0xffff
	  ori t1, t1, 0xfcff
	  and t0, t0, t1
	  mtc0 t0, CP0_CAUSE
	  nop
    lui t0, 0x17d           #设置compare寄存器值为0x17d7840
    ori t0, t0, 0x7840
//    li t0, 50
    mtc0 t0, CP0_COMPARE
		nop
    mtc0 zero, CP0_COUNT
		nop
    STI
watch_running:
#进入循环，等待时钟中断来临
loop:
		j loop
		nop

############################
/*
 *时钟中断来临，计算数码管显示的数值
 */
do_time:
    li t0, NUM_ADDR
		lw t1, 0(t0)
		andi t1, t1, 0xff       #second_v
		lw t2, 0(t0)
		andi t2, t2, 0xff00
		srl t2, t2, 8           #minute_v
		lw t3, 0(t0)
		lui t4, 0xff
		and t3, t3, t4
		srl t3, t3, 16          #hour_v

		addiu t1, t1, 0x1       #second_v++
		move  t4, t1
		addiu t4, t4, -0xa
		bltz  t4, 1f
		nop
		beqz  t4, 2f
		nop
		move  t4, t1
		addiu t4, t4, -0x1a
		bltz  t4, 1f
		nop
		beqz  t4, 2f
		nop
		move  t4, t1
		addiu t4, t4, -0x2a
		bltz  t4, 1f
		nop
		beqz  t4, 2f
		nop
		move  t4, t1
		addiu t4, t4, -0x3a
		bltz  t4, 1f
		nop
		beqz  t4, 2f
		nop
		move  t4, t1
		addiu t4, t4, -0x4a
		bltz  t4, 1f
		nop
		beqz  t4, 2f
		nop
		move  t4, t1
		addiu t4, t4, -0x5a
		bltz  t4, 1f
		nop
		beqz  t4, 7f
		nop
1:
    j combine
		nop
2:
    addiu t1, t1, 6
    j combine
		nop
7:
    lui t1, 0x0
		addiu t2, t2, 0x1       #minute_v++
		move  t4, t2
		addiu t4, t4, -0xa
		bltz  t4, 8f
		nop
		beqz  t4, 9f
		nop
		move  t4, t2
		addiu t4, t4, -0x1a
		bltz  t4, 8f
		nop
		beqz  t4, 9f
		nop
		move  t4, t2
		addiu t4, t4, -0x2a
		bltz  t4, 8f
		nop
		beqz  t4, 9f
		nop
		move  t4, t2
		addiu t4, t4, -0x3a
		bltz  t4, 8f
		nop
		beqz  t4, 9f
		nop
		move  t4, t2
		addiu t4, t4, -0x4a
		bltz  t4, 8f
		nop
		beqz  t4, 9f
		nop
		move  t4, t2
		addiu t4, t4, -0x5a
		bltz  t4, 8f
		nop
		beqz  t4, 14f
		nop
8:
    j combine
		nop
9:
    addiu t2, t2, 6
    j combine
		nop
14:
    lui t2, 0x0
		addiu t3, t3, 0x1      #hour_v++
		move  t4, t3
		addiu t4, t4, -0xa
		bltz  t4, 15f
		nop
		beqz  t4, 16f
		nop
		move  t4, t3
		addiu t4, t4, -0x1a
		bltz  t4, 15f
		nop
		beqz  t4, 16f
		nop
		move  t4, t3
		addiu t4, t4, -0x24
		bltz  t4, 15f
		nop
		beqz  t4, 18f
		nop
15:
    j combine
		nop
16:
    addiu t3, t3, 6
    j combine
		nop
18:
    lui t3, 0x0
combine:               #将时分秒组合后显示
    sll t3, t3, 16
		sll t2, t2, 8
		or  t2, t2, t1
		or  t3, t3, t2
		sw  t3, 0(t0)
    jr ra
		nop
