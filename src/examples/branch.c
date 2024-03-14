int main() {
    int t0 = 5; // addi $t0, $zero, 5
    int t1 = 4; // addi $t1, $zero, 4

    if (t0 != t1) { // beq $t0, $t1, else
        int t2 = 10; // addi $t2, $zero, 10
        int t3 = 20; // addi $t3, $zero, 20
    }

    int t4 = 10; // addi $t4, $zero, 10

    return 0;
}
