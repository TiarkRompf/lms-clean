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
  std::function<int(int)> x3 = [&](int x4) {
    x1 = 1;
    return x4 + 1;
  };
  std::function<int(int)> x5 = [&](int x6) {
    x2 = 3;
    return x6 + 2;
  };
  int x7 = x3(x5(x0));
  printf("%d, %d, %d", x1, x2, x7);
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
