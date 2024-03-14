int main() {
    int memory[10]; // simulate memory with an array
    int t0 = 0;     // index into the 'memory' array to represent $t0
    int t1;         // variable to represent $t1
    int t2;         // variable to represent $t2

    t1 = 10;              // equivalent to 'addi $t1, $zero, 10'
    memory[t0 + 1] = t1;  // equivalent to 'sw $t1, 4($t0)' 
    t2 = memory[t0 + 1];  // equivalent to 'lw $t2, 4($t0)' its +1 here but in MIPS it would be +4 because of the word size of 4 bytes

    return 0;   
}
