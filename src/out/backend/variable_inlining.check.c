/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  int x1 = 0;
  while (x1 < x0) {
    int x2 = x1;
    x1 = x1 + 1;
    printf("%d\n", x2);
  }
  int x3 = 0;
  while (x3 < x0) {
    printf("%d\n", x3);
    x3 = x3 + 1;
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
1
2
3
0
1
2
3
