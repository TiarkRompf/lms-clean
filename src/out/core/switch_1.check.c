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
  switch (x0) {
    case 0:
    printf("zero %d\n", 0);
    break;
    case 1:
    case 2:
    case 3:
    x1 = x0;
    break;
    default:
    printf("%d\n", x0);
    break;
  }
  printf("Final value %d\n", x1);
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
// Output:
zero 0
Final value 0
Final value 2
5
Final value 0
