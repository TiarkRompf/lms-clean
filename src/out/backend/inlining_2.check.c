/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int32_t x0) {
  int32_t x1 = x0 - 4;
  int32_t x2 = 0;
  while (x2 < x0) {
    printf("%d\n", x1);
    x2 = x2 + 1;
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
// output:
0
0
0
0
1
1
1
1
1
