#include <cudnn.h>

#define CUDNNCHECK(cmd) do {                         \
  cudnnStatus_t status = cmd;                       \
  if( status != CUDNN_STATUS_SUCCESS ) {            \
    printf("Cudnn failure %s:%d '%s'\n",            \
        __FILE__,__LINE__,cudnnGetErrorString(cmd));   \
    exit(EXIT_FAILURE);                             \
  }                                                 \
} while(0)