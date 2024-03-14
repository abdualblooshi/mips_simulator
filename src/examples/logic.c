int main() {
  int t0 = 1;
  int t1 = t0 << 2; // Shift left by 2 bits
  int t2 = 15;
  int t3 = 10;
  int t4 = t2 & t3; // Bitwise AND
  int t5 = t2 & 3; // Bitwise AND with immediate value
  int t6 = t2 | t3; // Bitwise OR
  int t7 = t3 | 5; // Bitwise OR with immediate value
  int t8 = ~(t2 | t3); // Bitwise NOR

  // Print the results
  printf("t0 = %d\n", t0);
  printf("t1 = %d\n", t1);
  printf("t2 = %d\n", t2);
  printf("t3 = %d\n", t3);
  printf("t4 = %d\n", t4);
  printf("t5 = %d\n", t5);
  printf("t6 = %d\n", t6);
  printf("t7 = %d\n", t7);
  printf("t8 = %d\n", t8);

  return 0;
}
