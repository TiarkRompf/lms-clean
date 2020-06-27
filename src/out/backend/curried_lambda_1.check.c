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
  int x1 = 0;
  int x2 = 1;
  std::function<std::function<int(int)>(int)> x3 = [&](int x4) {
    x1 = 1;
    std::function<int(int)> x5 = [&, x4](int x6) {
      x2 = 2;
      return x6 + x4;
    };
    return x5;
  };
  printf("%d", x3(x0)(x0));
  printf(", %d, %d\n", x1, x2);
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
