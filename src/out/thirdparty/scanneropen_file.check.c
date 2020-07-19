/*****************************************
Emitting C Generated Code
*******************************************/
#define _GNU_SOURCE
#include <stdlib.h>
#include <scanner_header.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  int x1 = open("/scratch-ml00/wang603/lms-umbrella/lms-clean/src/test/scala/lms/thirdparty/test_scanner.scala", 0);
  printf("file length is %ld\n", fsize(x1));
  close(x1);
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
