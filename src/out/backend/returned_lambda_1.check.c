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
  printf("%d", (x0 == 0 ? ({
    std::function<int(int*)> x2 = [](int* x3) {
      x3[0] = 8;
      return 1;
    };
    x2;
  }) : ({
    std::function<int(int*)> x4 = [](int* x5) {
      x5[0] = 9;
      return 2;
    };
    x4;
  }))(x1));
  printf(", %d\n", x1[0]);
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
