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
  printf("%d\n", (x0 == 0 ? ({
    std::function<int(int)> x1 = [](int x2) {
      return 1;
    };
    x1;
  }) : ({
    std::function<int(int)> x3 = [](int x4) {
      return 2;
    };
    x3;
  }))(x0));
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
