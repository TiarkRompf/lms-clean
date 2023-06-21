/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/*********** Datastructures ***********/
struct Anon573948033 {
  double real;
  double image;
};
/**************** Snippet ****************/
double Snippet(int x0) {
  struct Anon573948033 x1 = { 0 };
  struct Anon573948033* x2 = &x1;
  x2->real = 1.23;
  x2->image = 2.34;
  return x2->real + x2->image;
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
