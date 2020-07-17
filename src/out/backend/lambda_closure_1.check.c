/*****************************************
Emitting C Generated Code
*******************************************/
#include <functional>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  int x1 = 100;
  std::function<int*(int)> x2 = [&](int x3) {
    int* x4 = (int*)malloc(x3 * sizeof(int));
    x4[0] = x1;
    x1 = 20;
    return x4;
  };
  int* x5 = x2(3);
  int x6 = x1;
  x5[1] = x6;
  printf("%d, %d, %d\n", x5[0], x6, x1);
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
