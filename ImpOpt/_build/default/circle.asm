.text
	beqz $a0, init_end
	lw $a0, 0($a1)
	jal atoi
init_end:
	subi $sp, $sp, 4
	sw $v0, 0($sp)
	jal main
	li $v0, 10
	syscall
main:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	subi $sp, $sp, 4
	sw $s0, 0($sp)
	subi $sp, $sp, 4
	sw $s1, 0($sp)
	subi $sp, $sp, 4
	sw $s2, 0($sp)
	subi $sp, $sp, 4
	sw $s3, 0($sp)
	subi $sp, $sp, 4
	sw $s4, 0($sp)
	subi $sp, $sp, 4
	sw $s5, 0($sp)
	subi $sp, $sp, 4
	sw $s6, 0($sp)
	subi $sp, $sp, 4
	sw $s7, 0($sp)
	addi $sp, $sp, 0
#Parameter arg does not need explicit allocation
#Load parameter i into register $s0
	lw $s0, 8($fp)
	li $t0, 0
	move $s0, $t0
	li $t0, 10
	move $s1, $t0
	li $t0, 32
	la $t1, espace
	sw $t0, 0($t1)
	b __main_0
__main_1:
	li $t0, 3
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	move $t0, $s0
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	jal affiche_ligne
	addi $sp, $sp, 8
	move $t0, $s1
	move $a0, $t0
	li $v0, 11
	syscall
	move $t0, $s0
	li $t1, 1
	add $t0, $t0, $t1
	move $s0, $t0
__main_0:
	move $t0, $s0
	li $t1, 3
	li $t2, 1
	add $t1, $t1, $t2
	slt $t0, $t0, $t1
	bnez $t0, __main_1
	lw $s7, 0($sp)
	addi $sp, $sp, 4
	lw $s6, 0($sp)
	addi $sp, $sp, 4
	lw $s5, 0($sp)
	addi $sp, $sp, 4
	lw $s4, 0($sp)
	addi $sp, $sp, 4
	lw $s3, 0($sp)
	addi $sp, $sp, 4
	lw $s2, 0($sp)
	addi $sp, $sp, 4
	lw $s1, 0($sp)
	addi $sp, $sp, 4
	lw $s0, 0($sp)
	addi $sp, $sp, 4
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	li $t0, 0
	jr $ra
affiche_ligne:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	subi $sp, $sp, 4
	sw $s0, 0($sp)
	subi $sp, $sp, 4
	sw $s1, 0($sp)
	subi $sp, $sp, 4
	sw $s2, 0($sp)
	subi $sp, $sp, 4
	sw $s3, 0($sp)
	subi $sp, $sp, 4
	sw $s4, 0($sp)
	subi $sp, $sp, 4
	sw $s5, 0($sp)
	subi $sp, $sp, 4
	sw $s6, 0($sp)
	subi $sp, $sp, 4
	sw $s7, 0($sp)
	addi $sp, $sp, 0
#Load parameter i into register $s1
	lw $s1, 4($fp)
#Load parameter n into register $s2
	lw $s2, 8($fp)
#Load parameter j into register $s3
	lw $s3, 12($fp)
	li $t0, 0
	move $s3, $t0
	b __affiche_ligne_0
__affiche_ligne_1:
	move $t0, $s1
	move $t1, $s1
	mul $t0, $t0, $t1
	move $t1, $s3
	move $t2, $s3
	mul $t1, $t1, $t2
	add $t0, $t0, $t1
	move $t1, $s2
	move $t2, $s2
	mul $t1, $t1, $t2
	slt $t0, $t0, $t1
	bnez $t0, __affiche_ligne_2
	li $t0, 35
	move $a0, $t0
	li $v0, 11
	syscall
	b __affiche_ligne_3
__affiche_ligne_2:
	li $t0, 46
	move $a0, $t0
	li $v0, 11
	syscall
__affiche_ligne_3:
	move $t0, $s0
	move $a0, $t0
	li $v0, 11
	syscall
	move $t0, $s3
	li $t1, 1
	add $t0, $t0, $t1
	move $s3, $t0
__affiche_ligne_0:
	move $t0, $s3
	move $t1, $s2
	li $t2, 1
	add $t1, $t1, $t2
	slt $t0, $t0, $t1
	bnez $t0, __affiche_ligne_1
	lw $s7, 0($sp)
	addi $sp, $sp, 4
	lw $s6, 0($sp)
	addi $sp, $sp, 4
	lw $s5, 0($sp)
	addi $sp, $sp, 4
	lw $s4, 0($sp)
	addi $sp, $sp, 4
	lw $s3, 0($sp)
	addi $sp, $sp, 4
	lw $s2, 0($sp)
	addi $sp, $sp, 4
	lw $s1, 0($sp)
	addi $sp, $sp, 4
	lw $s0, 0($sp)
	addi $sp, $sp, 4
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	li $t0, 0
	jr $ra
#built-in atoi
atoi:
	li $v0, 0
atoi_loop:
	lbu $t0, 0($a0)
	beqz $t0, atoi_end
	addi $t0, $t0, -48
	bltz $t0, atoi_error
	bge $t0, 10, atoi_error
	mul $v0, $v0, 10
	add $v0, $v0, $t0
	addi $a0, $a0, 1
	b atoi_loop
atoi_error:
	li $v0, 10
	syscall
atoi_end:
	jr $ra
.data
retour:
	.word 0
espace:
	.word 0
