/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/*********** Datastructures ***********/
struct Complex {
  double real;
  double image;
};
/**************** Snippet ****************/
double Snippet(int x0) {
  struct Complex x1 = { 0 };
  struct Complex* x2 = &x1;
  x2->real = 1.23;
  x2->image = 2.34;
  struct Complex x3[1] = { *x2 };
  struct Complex x4[1] = { *x3 };
  x4->real = 10.0;
  return x4->real + x2->image;
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
