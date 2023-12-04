	.file	"test0.c"
	.text
	.globl	a0
	.bss
	.align 4
	.type	a0, @object
	.size	a0, 4
a0:
	.zero	4
	.globl	a1
	.align 4
	.type	a1, @object
	.size	a1, 4
a1:
	.zero	4
	.globl	a2
	.align 4
	.type	a2, @object
	.size	a2, 4
a2:
	.zero	4
	.globl	a3
	.align 4
	.type	a3, @object
	.size	a3, 4
a3:
	.zero	4
	.globl	a4
	.align 4
	.type	a4, @object
	.size	a4, 4
a4:
	.zero	4
	.globl	a5
	.align 4
	.type	a5, @object
	.size	a5, 4
a5:
	.zero	4
	.globl	a6
	.align 4
	.type	a6, @object
	.size	a6, 4
a6:
	.zero	4
	.globl	a7
	.align 4
	.type	a7, @object
	.size	a7, 4
a7:
	.zero	4
	.globl	a8
	.align 4
	.type	a8, @object
	.size	a8, 4
a8:
	.zero	4
	.globl	a9
	.align 4
	.type	a9, @object
	.size	a9, 4
a9:
	.zero	4
	.globl	a10
	.align 4
	.type	a10, @object
	.size	a10, 4
a10:
	.zero	4
	.globl	a11
	.align 4
	.type	a11, @object
	.size	a11, 4
a11:
	.zero	4
	.globl	a12
	.align 4
	.type	a12, @object
	.size	a12, 4
a12:
	.zero	4
	.globl	a13
	.align 4
	.type	a13, @object
	.size	a13, 4
a13:
	.zero	4
	.globl	a14
	.align 4
	.type	a14, @object
	.size	a14, 4
a14:
	.zero	4
	.globl	a15
	.align 4
	.type	a15, @object
	.size	a15, 4
a15:
	.zero	4
	.globl	a16
	.align 4
	.type	a16, @object
	.size	a16, 4
a16:
	.zero	4
	.globl	a17
	.align 4
	.type	a17, @object
	.size	a17, 4
a17:
	.zero	4
	.globl	a18
	.align 4
	.type	a18, @object
	.size	a18, 4
a18:
	.zero	4
	.globl	a19
	.align 4
	.type	a19, @object
	.size	a19, 4
a19:
	.zero	4
	.globl	a20
	.align 4
	.type	a20, @object
	.size	a20, 4
a20:
	.zero	4
	.globl	a21
	.align 4
	.type	a21, @object
	.size	a21, 4
a21:
	.zero	4
	.globl	a22
	.align 4
	.type	a22, @object
	.size	a22, 4
a22:
	.zero	4
	.globl	a23
	.align 4
	.type	a23, @object
	.size	a23, 4
a23:
	.zero	4
	.globl	a24
	.align 4
	.type	a24, @object
	.size	a24, 4
a24:
	.zero	4
	.globl	a31
	.align 4
	.type	a31, @object
	.size	a31, 4
a31:
	.zero	4
	.globl	a32
	.align 4
	.type	a32, @object
	.size	a32, 4
a32:
	.zero	4
	.globl	a33
	.align 4
	.type	a33, @object
	.size	a33, 4
a33:
	.zero	4
	.globl	a34
	.align 4
	.type	a34, @object
	.size	a34, 4
a34:
	.zero	4
	.globl	a35
	.align 4
	.type	a35, @object
	.size	a35, 4
a35:
	.zero	4
	.globl	a36
	.align 4
	.type	a36, @object
	.size	a36, 4
a36:
	.zero	4
	.globl	a37
	.align 4
	.type	a37, @object
	.size	a37, 4
a37:
	.zero	4
	.globl	a38
	.align 4
	.type	a38, @object
	.size	a38, 4
a38:
	.zero	4
	.globl	a39
	.align 4
	.type	a39, @object
	.size	a39, 4
a39:
	.zero	4
	.globl	a40
	.align 4
	.type	a40, @object
	.size	a40, 4
a40:
	.zero	4
	.globl	res
	.align 4
	.type	res, @object
	.size	res, 4
res:
	.zero	4
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$265, res(%rip)
	movl	$265, res(%rip)
	movl	$265, res(%rip)
	movl	$265, res(%rip)
	movl	$265, res(%rip)
	movl	$265, res(%rip)
	movl	$265, res(%rip)
	movl	$265, res(%rip)
	movl	res(%rip), %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0"
	.section	.note.GNU-stack,"",@progbits
	.section	.note.gnu.property,"a"
	.align 8
	.long	1f - 0f
	.long	4f - 1f
	.long	5
0:
	.string	"GNU"
1:
	.align 8
	.long	0xc0000002
	.long	3f - 2f
2:
	.long	0x3
3:
	.align 8
4:
