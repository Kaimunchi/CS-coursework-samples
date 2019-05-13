# MIPS Implementation of multiplication and exponentiation
# Brandon Ingli
# 25 March 2019

.data
prompt1:  .asciiz "Enter non-negative base: "
prompt2:  .asciiz "Enter non-negative exponent: "
newline:  .asciiz "\n"
exp_sym:  .asciiz " ^ "
equals:   .asciiz " = "
err_0:    .asciiz "Error: 0^0.\n"
          .align 2 #Make sure subsequent words line up correctly

#Begin Code
.text

# b -> $s0
# e -> $s1

main:
        #Prompt for base
        la $a0, prompt1
        li $v0, 4
        syscall

        #Read base
        li $v0, 5
        syscall
        move $s0, $v0

        #Prompt for exponent
        la $a0, prompt2
        li $v0, 4
        syscall

        #Read exponent
        li $v0, 5
        syscall
        move $s1, $v0

        #Print a new line
        la $a0, newline
        li $v0, 4
        syscall

        #Check for invalid case (0^0)
        bne $s0, $zero, main2
        bne $s1, $zero, main2
        #Print error message
        la $a0, err_0
        li $v0, 4
        syscall
        #Exit with -1 result
        li $v0, 17
        li $a0, -1
        syscall

main2:  #call raise(b,e)
        move $a0, $s0
        move $a1, $s1
        jal raise

        move $s2, $v0

        #Print result
        li $v0, 1
        move $a0, $s0
        syscall

        li $v0, 4
        la $a0, exp_sym
        syscall

        li $v0, 1
        move $a0, $s1
        syscall

        li $v0, 4
        la $a0, equals
        syscall

        li $v0, 1
        move $a0, $s2
        syscall

        li $v0, 4
        la $a0, newline
        syscall

        #exit
        li $v0, 10
        syscall

###### raise(b, e) = b ^ e
# Used $s0

raise:
        addi $sp, $sp, -8
        #save ra
        sw $ra, 4($sp)
        #save s-regs
        sw $s0, 0($sp)

        #Base Case: exponent is zero
        bne $a1, $zero, raise2
        li $v0, 1
        j raise_return
    
raise2:
        #recursive case 1: exponent is odd
        andi $t0, $a1, 0x1
        beq $t0, $zero, raise3
        #Save the base
        move $s0, $a0
        #Calculate raise(b, e-1)
        addi $a1, $a1, -1
        jal raise
        #Calculate mult(b, raise(b, e-1))
        move $a0, $s0
        move $a1, $v0
        jal multiply
        j raise_return

raise3:
        #recursive case 2: e is even
        #Call raise(b, e/2)
        srl $a1, $a1, 1
        jal raise
        #Call mult(temp, temp)
        move $a0, $v0
        move $a1, $v0
        jal multiply
        
raise_return:
        #cleanup stack and return
        lw $s0, 0($sp)
        lw $ra, 4($sp)
        addi $sp, $sp, 8
        jr $ra

###### multiply(a, b) = a * b
#Used $s0

multiply:
        addi $sp, $sp, -8
        #save ra
        sw $ra, 4($sp)
        #save s-regs
        sw $s0, 0($sp)

        #Base Case: a is zero
        bne $a0, $zero, multiply2
        move $v0, $zero
        j multiply_return

multiply2:
        #Recursive Case
        #Save b
        move $s0, $a1
        #Call mult(a-1, b)
        addi $a0, $a0, -1
        jal multiply
        # Add to b
        add $v0, $s0, $v0

multiply_return:
        #cleanup stack and return
        lw $s0, 0($sp)
        lw $ra, 4($sp)
        addi $sp, $sp, 8
        jr $ra




        


