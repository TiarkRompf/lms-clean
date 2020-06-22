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
  std::function<void(int*)> x1 = [](int* x2) {
    x2[0] = 5;
  };
  int* x3 = (int*)malloc(1 * sizeof(int));
  x1(x3);
  printf("%d\n", x3[0]);
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
