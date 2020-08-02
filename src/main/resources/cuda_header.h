#ifndef NVIDIA_WARP_SIZE
#define NVIDIA_WARP_SIZE 32 // this is typically 32 (for incl. 1080ti s)
#endif

#define CUDA_CALL(f)                                                           \
  {                                                                            \
    cudaError_t err = (f);                                                     \
    if (err != cudaSuccess) {                                                  \
      fprintf(stderr, "CUDA error occurred: %s (%s:%d)\n",                     \
              cudaGetErrorString(err), __FILE__, __LINE__);                    \
      exit(err);                                                               \
    }                                                                          \
  }

template <typename T> __global__ void arrayFill(T *data, T value, int size) {
  int stride = gridDim.x * blockDim.x;
  int tid = threadIdx.x + blockIdx.x * blockDim.x;
  for (; tid < size; tid += stride)
    data[tid] = value;
}

template <typename T> __global__ void clipAt(T *in, T bound, int size) {
  int stride = gridDim.x * blockDim.x;
  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  for (; tid < size; tid += stride) {
    if (in[tid] > bound)
      in[tid] = bound;
    if (in[tid] < -bound)
      in[tid] = -bound;
  }
}

template <typename T>
__global__ void hardTanh(T *in, T *out, T min_val, T max_val, int size) {
  int tid = threadIdx.x + blockIdx.x * blockDim.x;
  int stride = gridDim.x * blockDim.x;
  for (int i = tid; i < size; i += stride) {
    out[i] = in[i] < min_val ? min_val : (in[i] > max_val ? max_val : in[i]);
  }
}

template <typename T>
__global__ void hardTanh_grad(T *in_x, T *in_d, T *out_d, T min_val, T max_val,
                              int size, bool inplace) {
  int tid = threadIdx.x + blockIdx.x * blockDim.x;
  int stride = gridDim.x * blockDim.x;
  for (int i = tid; i < size; i += stride) {
    if (inplace) {
      if (in_x[i] < min_val || in_x[i] > max_val)
        in_d[i] = 0;
    } else {
      if (in_x[i] >= min_val && in_x[i] <= max_val)
        in_d[i] += out_d[i];
    }
  }
}

template <typename T>
__global__ void nllLoss(T *x, int x_stride, T *y, int *target) {
  // here the `tid` is the `batch index`
  // this kernel is run with num_of_batch threads
  // `x_stride` is just the stride of 0th dim of input
  int tid = threadIdx.x + blockIdx.x * blockDim.x;
  int offset = tid * x_stride + target[tid];
  y[tid] = -1 * x[offset];
}

template <typename T>
__global__ void nllLoss_grad(int x_stride, T *yGrad, int *target, T *xGrad) {
  int tid = threadIdx.x + blockIdx.x * blockDim.x;
  int offset = tid * x_stride + target[tid];
  xGrad[offset] += -1 * yGrad[tid];
}

// only for 4D tensor in and 3D tensor out
// This is the gradient of sum op.
// It simply `extend` the 3D grad to 4D at `dim`
template <typename T>
__global__ void sum_grad(T *in, int inSize0, int inSize1, int inSize2,
                         int inSize3, int nElement, T *out, int outStride0,
                         int outStride1, int outStride2, int dim) {
  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  int stride = gridDim.x * blockDim.x;
  for (int i = tid; i < nElement; i += stride) {
    // this is computing the dimensions from flat offset
    int inOff2 = i / inSize3;
    int inDim3 = i - inOff2 * inSize3;
    int inOff1 = inOff2 / inSize2;
    int inDim2 = inOff2 - inOff1 * inSize2;
    int inDim0 = inOff1 / inSize1;
    int inDim1 = inOff1 - inDim0 * inSize1;
    int outOff = 0;
    // this is computing the gradient offset
    if (dim == 0)
      outOff = inDim1 * outStride0 + inDim2 * outStride1 + inDim3 * outStride2;
    if (dim == 1)
      outOff = inDim0 * outStride0 + inDim2 * outStride1 + inDim3 * outStride2;
    if (dim == 2)
      outOff = inDim0 * outStride0 + inDim1 * outStride1 + inDim3 * outStride2;
    if (dim == 3)
      outOff = inDim0 * outStride0 + inDim1 * outStride1 + inDim2 * outStride2;
    // this is the inplace accumulation of gradient
    in[i] += out[outOff];
  }
}
