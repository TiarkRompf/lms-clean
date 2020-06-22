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
  int* x1 = (int*)malloc(10 * sizeof(int));
  int* x2 = (int*)malloc(5 * sizeof(int));
  std::function<void(int*)> x3 = [](int* x4) {
    x4[2] = 3;
  };
  std::function<std::function<void(int*)>(int*)> x5 = [=](int* x6) {
    x6[1] = 2;
    return x3;
  };
  std::function<std::function<std::function<void(int*)>(int*)>(int*)> x7 = [&](int* x8) {
    x8[0] = 1;
    return x5;
  };
  x7(x1)(x1)(x2);
  printf("%d, %d, %d\n", x1[0], x1[1], x2[2]);
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
