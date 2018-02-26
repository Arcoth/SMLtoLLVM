	.text
	.globl _enterGC
	.extern cleanup
_enterGC:
	mov %rsp, %rdi
	jmp cleanup
