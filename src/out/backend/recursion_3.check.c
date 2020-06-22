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
  //# lambda forward is here!
  std::function<int(int)> x2 = [&](int x4) {
    return x4 > 0 ? x1 * x2(x4 - x0) : 1;
  };
  while (x1 < 10) {
    printf("%d, ", x2(x1));
    x1 = x1 + 1;
  }
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
