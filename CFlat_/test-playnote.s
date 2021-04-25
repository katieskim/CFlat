	.text
	.file	"CFlat"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	leaq	.Lrhythm_ptr(%rip), %rax
	movq	%rax, 16(%rsp)
	leaq	.Ltone_ptr(%rip), %rax
	movq	%rax, (%rsp)
	movl	$4, 8(%rsp)
	movq	%rsp, %rdi
	callq	play_note@PLT
	xorl	%eax, %eax
	addq	$24, %rsp
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.type	.Lfmt.1,@object         # @fmt.1
.Lfmt.1:
	.asciz	"/%s/ /%d/ /%s/\n"
	.size	.Lfmt.1, 16

	.type	.Lfmt.2,@object         # @fmt.2
.Lfmt.2:
	.asciz	"%g\n"
	.size	.Lfmt.2, 4

	.type	.Lfmt.3,@object         # @fmt.3
.Lfmt.3:
	.asciz	"%s\n"
	.size	.Lfmt.3, 4

	.type	.Lfmt.4,@object         # @fmt.4
.Lfmt.4:
	.asciz	"%d\n"
	.size	.Lfmt.4, 4

	.type	.Lfmt.5,@object         # @fmt.5
.Lfmt.5:
	.asciz	"%s\n"
	.size	.Lfmt.5, 4

	.type	.Lfmt.6,@object         # @fmt.6
.Lfmt.6:
	.asciz	"%s\n"
	.size	.Lfmt.6, 4

	.type	.Ltone_ptr,@object      # @tone_ptr
.Ltone_ptr:
	.asciz	"C-"
	.size	.Ltone_ptr, 3

	.type	.Lrhythm_ptr,@object    # @rhythm_ptr
.Lrhythm_ptr:
	.asciz	"s."
	.size	.Lrhythm_ptr, 3


	.section	".note.GNU-stack","",@progbits
