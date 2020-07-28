#define NVIDIA_WARP_SIZE 32 // this is typically 32 (for incl. 1080ti s)

#define CUDA_CALL(f) { \
  cudaError_t err = (f); \
  if (err != cudaSuccess) { \
    fprintf(stderr, "CUDA error occurred: %s (%s:%d)\n", \
            cudaGetErrorString(err), __FILE__, __LINE__); \
    exit(err); \
  } \
}

template <typename T>
__global__ void arrayFill(T* data, T value, int size) {
  int stride = gridDim.x * blockDim.x;
  int tid = threadIdx.x + blockIdx.x * blockDim.x;
  for (; tid < size; tid += stride)
    data[tid] = value;
}

template <typename T>
__global__ void clipAt(T* in, T bound, int size) {
  int stride = gridDim.x * blockDim.x;
  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  for (; tid < size; tid += stride) {
    if (in[tid] > bound) in[tid] = bound;
    if (in[tid] < -bound) in[tid] = -bound;
  }
}
