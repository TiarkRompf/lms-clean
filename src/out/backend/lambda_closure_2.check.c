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
  int* x1 = (x0 == 0 ? ({
    int x2 = 100;
    std::function<int*(int)> x3 = [=](int x4) {
      int* x5 = (int*)malloc(x4 * sizeof(int));
      x5[0] = x2;
      return x5;
    };
    x3;
  }) : ({
    int x6 = 40;
    std::function<int*(int)> x7 = [=](int x8) {
      int* x9 = (int*)malloc(x8 * sizeof(int));
      x9[0] = x6;
      return x9;
    };
    x7;
  }))(3);
  x1[1] = 70;
  printf("%d, %d\n", x1[0], 70);
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
