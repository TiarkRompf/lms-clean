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
  int x3 = 2;
  std::function<std::function<std::function<int(int)>(int)>(int)> x4 = [&](int x5) {
    x1 = 1;
    std::function<std::function<int(int)>(int)> x6 = [&, x5](int x7) {
      x2 = 2;
      int x8 = x5 + x7;
      std::function<int(int)> x9 = [&, x5, x7](int x10) {
        x3 = 3;
        return x8 + x10;
      };
      return x9;
    };
    return x6;
  };
  printf("%d", x4(x0)(x0)(x0));
  printf(", %d, %d, %d\n", x1, x2, x3);
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
