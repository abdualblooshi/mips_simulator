int main() {
    int counter = 5; 
    int result = 1;

    Loop: 
    if (counter <= 0) {  // Equivalent to 'slt $t2, $zero, $t0' and 'beq $t2, $zero, Exit'
        goto Exit; 
    }

    result *= counter;  // Equivalent to 'add $t1, $t1, $t0'
    counter--;          // Equivalent to 'addi $t0, $t0, -1'

    goto Loop;          // Equivalent to 'j Loop'

    Exit:
    printf("The factorial of 5 is: %d\n", result); 

    return 0;
}
