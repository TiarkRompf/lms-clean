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
  std::function<std::function<int(int)>(int)> x1 = [](int x2) {
    std::function<int(int)> x3 = [=](int x4) {
      return x2 + x4;
    };
    return x3;
  };
  printf("%d\n", x1(x0)(x0 + 1));
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
