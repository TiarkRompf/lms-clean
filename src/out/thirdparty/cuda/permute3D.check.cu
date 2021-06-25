/*****************************************
Emitting C Generated Code
*******************************************/
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <string.h>
#include <stdlib.h>
#include "cuda_header.h"
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include "scanner_header.h"
/************* Functions **************/
__global__ void x4(int* x5, int* x6, int x7, int x8, int x9) {
  int x10 = blockIdx.z;
  // this is the permutation kernel for List(0, 2, 1)
  // arg0: 3D input tensor (dimZ x dimY x dimX)
  // arg1: 3D output tensor (dimZ x dimX x dimY)
  // arg2: dimZ of input
  // arg3: dimY of input
  // arg4: dimX of input
  // caller must use <<<dim3((dimX+31)/32, (dimY+31)/32, dimZ), dim3(32, 8, 1)>>>
  __shared__ int x11[1056];
  // read data from input array to shared memory
  int x12 = 0;
  int x13 = blockIdx.y * 32;
  int x14 = blockIdx.x * 32;
  while (x12 < 32) {
    int x15 = x12;
    int x16 = x10 + threadIdx.z;
    if (x16 < x7 && x13 + (threadIdx.y + x15) < x8 && x14 + threadIdx.x < x9) x11[1056 * threadIdx.z + 33 * (threadIdx.y + x15) + threadIdx.x] = x5[(x13 + (threadIdx.y + x15) + x16 * x8) * x9 + (x14 + threadIdx.x)];
    x12 = x12 + 8;
  }
  // sync threads
  __syncthreads();
  // write data from shared memory to output array
  int x17 = 0;
  while (x17 < 32) {
    int x18 = x17;
    int x19 = x10 + threadIdx.z;
    if (x19 < x7 && x14 + (threadIdx.y + x18) < x9 && x13 + threadIdx.x < x8) x6[(x14 + (threadIdx.y + x18) + x19 * x9) * x8 + (x13 + threadIdx.x)] = x11[1056 * threadIdx.z + 33 * threadIdx.x + (threadIdx.y + x18)];
    x17 = x17 + 8;
  }
}
__global__ void x24(int* x25, int* x26, int x27, int x28, int x29) {
  int x30 = blockIdx.y;
  // this is the permutation kernel for List(1, 2, 0)
  // arg0: 3D input tensor (dimZ x dimY x dimX)
  // arg1: 3D output tensor (dimY x dimX x dimZ)
  // arg2: dimZ of input
  // arg3: dimY of input
  // arg4: dimX of input
  // caller must use <<<dim3((dimX+31)/32, dimY, (dimZ+31)/32), dim3(32, 1, 8)>>>
  __shared__ int x31[1056];
  // read data from input array to shared memory
  int x32 = 0;
  int x33 = blockIdx.z * 32;
  int x34 = blockIdx.x * 32;
  while (x32 < 32) {
    int x35 = x32;
    int x36 = x33 + (threadIdx.z + x35);
    if (x36 < x27 && x30 + threadIdx.y < x28 && x34 + threadIdx.x < x29) x31[33 * (threadIdx.z + x35) + 33 * threadIdx.y + threadIdx.x] = x25[(x30 + threadIdx.y + x36 * x28) * x29 + (x34 + threadIdx.x)];
    x32 = x32 + 8;
  }
  // sync threads
  __syncthreads();
  // write data from shared memory to output array
  int x37 = 0;
  while (x37 < 32) {
    int x38 = x37;
    int x39 = x30 + threadIdx.y;
    if (x39 < x28 && x34 + (threadIdx.z + x38) < x29 && x33 + threadIdx.x < x27) x26[(x34 + (threadIdx.z + x38) + x39 * x29) * x27 + (x33 + threadIdx.x)] = x31[33 * threadIdx.x + 33 * threadIdx.y + (threadIdx.z + x38)];
    x37 = x37 + 8;
  }
}
__global__ void x44(int* x45, int* x46, int x47, int x48, int x49) {
  int x50 = blockIdx.y;
  // this is the permutation kernel for List(2, 1, 0)
  // arg0: 3D input tensor (dimZ x dimY x dimX)
  // arg1: 3D output tensor (dimX x dimY x dimZ)
  // arg2: dimZ of input
  // arg3: dimY of input
  // arg4: dimX of input
  // caller must use <<<dim3((dimX+31)/32, dimY, (dimZ+31)/32), dim3(32, 1, 8)>>>
  __shared__ int x51[1056];
  // read data from input array to shared memory
  int x52 = 0;
  int x53 = blockIdx.z * 32;
  int x54 = blockIdx.x * 32;
  while (x52 < 32) {
    int x55 = x52;
    int x56 = x53 + (threadIdx.z + x55);
    if (x56 < x47 && x50 + threadIdx.y < x48 && x54 + threadIdx.x < x49) x51[33 * (threadIdx.z + x55) + 33 * threadIdx.y + threadIdx.x] = x45[(x50 + threadIdx.y + x56 * x48) * x49 + (x54 + threadIdx.x)];
    x52 = x52 + 8;
  }
  // sync threads
  __syncthreads();
  // write data from shared memory to output array
  int x57 = 0;
  while (x57 < 32) {
    int x58 = x57;
    int x59 = x54 + (threadIdx.z + x58);
    if (x59 < x49 && x50 + threadIdx.y < x48 && x53 + threadIdx.x < x47) x46[(x50 + threadIdx.y + x59 * x48) * x47 + (x53 + threadIdx.x)] = x51[33 * threadIdx.x + 33 * threadIdx.y + (threadIdx.z + x58)];
    x57 = x57 + 8;
  }
}
__global__ void x64(int* x65, int* x66, int x67, int x68, int x69) {
  int x70 = blockIdx.z;
  // this is the permutation kernel for List(2, 0, 1)
  // arg0: 3D input tensor (dimZ x dimY x dimX)
  // arg1: 3D output tensor (dimX x dimZ x dimY)
  // arg2: dimZ of input
  // arg3: dimY of input
  // arg4: dimX of input
  // caller must use <<<dim3((dimX+31)/32, (dimY+31)/32, dimZ), dim3(32, 8, 1)>>>
  __shared__ int x71[1056];
  // read data from input array to shared memory
  int x72 = 0;
  int x73 = blockIdx.y * 32;
  int x74 = blockIdx.x * 32;
  while (x72 < 32) {
    int x75 = x72;
    int x76 = x70 + threadIdx.z;
    if (x76 < x67 && x73 + (threadIdx.y + x75) < x68 && x74 + threadIdx.x < x69) x71[1056 * threadIdx.z + 33 * (threadIdx.y + x75) + threadIdx.x] = x65[(x73 + (threadIdx.y + x75) + x76 * x68) * x69 + (x74 + threadIdx.x)];
    x72 = x72 + 8;
  }
  // sync threads
  __syncthreads();
  // write data from shared memory to output array
  int x77 = 0;
  while (x77 < 32) {
    int x78 = x77;
    int x79 = x74 + (threadIdx.y + x78);
    if (x79 < x69 && x70 + threadIdx.z < x67 && x73 + threadIdx.x < x68) x66[(x70 + threadIdx.z + x79 * x67) * x68 + (x73 + threadIdx.x)] = x71[1056 * threadIdx.z + 33 * threadIdx.x + (threadIdx.y + x78)];
    x77 = x77 + 8;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(165000 * sizeof(int));
  scan_ints("golden/permute3D_021/input.data", x1, 165000);
  int* x2 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(165000 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(165000 * sizeof(int)), cudaMemcpyHostToDevice));
  int* x3 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x3, (size_t)(165000 * sizeof(int))));
  x4<<<dim3(2, 2, 60), dim3(32, 8, 1)>>>(x2, x3, 60, 55, 50);
  int* x20 = (int*)malloc(165000 * sizeof(int));
  CUDA_CALL(cudaMemcpy(x20, x3, (size_t)(165000 * sizeof(int)), cudaMemcpyDeviceToHost));
  check_int_array("golden/permute3D_021/output.data", x20, 165000);
  int* x21 = (int*)malloc(165000 * sizeof(int));
  scan_ints("golden/permute3D_120/input.data", x21, 165000);
  int* x22 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x22, (size_t)(165000 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x22, x21, (size_t)(165000 * sizeof(int)), cudaMemcpyHostToDevice));
  int* x23 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x23, (size_t)(165000 * sizeof(int))));
  x24<<<dim3(2, 55, 2), dim3(32, 1, 8)>>>(x22, x23, 60, 55, 50);
  int* x40 = (int*)malloc(165000 * sizeof(int));
  CUDA_CALL(cudaMemcpy(x40, x23, (size_t)(165000 * sizeof(int)), cudaMemcpyDeviceToHost));
  check_int_array("golden/permute3D_120/output.data", x40, 165000);
  int* x41 = (int*)malloc(165000 * sizeof(int));
  scan_ints("golden/permute3D_210/input.data", x41, 165000);
  int* x42 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x42, (size_t)(165000 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x42, x41, (size_t)(165000 * sizeof(int)), cudaMemcpyHostToDevice));
  int* x43 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x43, (size_t)(165000 * sizeof(int))));
  x44<<<dim3(2, 55, 2), dim3(32, 1, 8)>>>(x42, x43, 60, 55, 50);
  int* x60 = (int*)malloc(165000 * sizeof(int));
  CUDA_CALL(cudaMemcpy(x60, x43, (size_t)(165000 * sizeof(int)), cudaMemcpyDeviceToHost));
  check_int_array("golden/permute3D_210/output.data", x60, 165000);
  int* x61 = (int*)malloc(165000 * sizeof(int));
  scan_ints("golden/permute3D_201/input.data", x61, 165000);
  int* x62 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x62, (size_t)(165000 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x62, x61, (size_t)(165000 * sizeof(int)), cudaMemcpyHostToDevice));
  int* x63 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x63, (size_t)(165000 * sizeof(int))));
  x64<<<dim3(2, 2, 60), dim3(32, 8, 1)>>>(x62, x63, 60, 55, 50);
  int* x80 = (int*)malloc(165000 * sizeof(int));
  CUDA_CALL(cudaMemcpy(x80, x63, (size_t)(165000 * sizeof(int)), cudaMemcpyDeviceToHost));
  check_int_array("golden/permute3D_201/output.data", x80, 165000);
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
