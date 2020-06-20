/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/************* Functions **************/
void x2(DataStructure1 x3) {
  printf("a library function that asks for pointers as parameter");
}
/**************** Snippet ****************/
void Snippet(int x0) {
  DataStructure1 x1 = { 0 };
  x2(x1);
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
