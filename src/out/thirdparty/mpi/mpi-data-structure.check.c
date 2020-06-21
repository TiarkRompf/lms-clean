/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/************* Functions **************/
void x1(DataStructure1 x2) {
  printf("a library function that asks for pointers as parameter");
}
/**************** Snippet ****************/
void Snippet(int x0) {
  DataStructure1 x3;
  DataStructure1 x4;
  x1(x3);
  x1(x4);
  printf("a int field %d", x3.fieldA);
}
/*****************************************
End of C Generated Code
*******************************************/
int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("usage: %s <arg>\n", argv[0]);
    return 0;
  }
  Snippet(atoi(argv[1]));
  return 0;
}
