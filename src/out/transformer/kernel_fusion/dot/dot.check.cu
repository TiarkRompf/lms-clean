/*****************************************
Emitting C Generated Code
*******************************************/
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include "cudnn_header.h"
#include "nccl_header.h"
#include <string.h>
#include <cblas.h>
#include <stdlib.h>
#include "cuda_header.h"
#include <stdio.h>
#include <stdint.h>
#include "cublas_header.h"
#include <stdbool.h>
#include "mpi_header.h"
#include "scanner_header.h"
/**************** Snippet ****************/
void Snippet(int x0) {
  module([&]() {
    float x1 = tensor_zeros(<4d2x4d3xf32>, Split d0 at devices=[GPU(0), GPU(1)]);
    float x2 = tensor_zeros(<4d2x4d3xf32>, Split d0 at devices=[GPU(0), GPU(1)]);
    float x3 = tensor_add(<4d2x4d3xf32>, Split d0 at devices=[GPU(0), GPU(1)], x1, x2);
    return x3;
  })("loss");
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
