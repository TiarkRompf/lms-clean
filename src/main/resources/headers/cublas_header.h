#ifndef CUBLAS_HEADER_H
#define CUBLAS_HEADER_H

#include <cublas_v2.h>

#define CUBLAS_CALL(f)                                           \
  {                                                              \
    cublasStatus_t err = (f);                                    \
    if (err != CUBLAS_STATUS_SUCCESS) {                          \
      fprintf(stderr, "CUBLAS error occurred\n");                \
      exit(err);                                                 \
    }                                                            \
  }                                                              \


#endif