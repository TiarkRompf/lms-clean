package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.thirdparty.array_computation.CudaOps

trait CUDNNDropoutTypeLess extends Dsl with CLibs with CUDNNBaseTypeLess {
  import BaseTypeLess._
  import PrimitiveTypeLess._
  import SIZE_TTypeLess._
  import CLibTypeLess._

  class CUDNN_DROPOUT_DESCRIPTOR(override val x: Backend.Exp) extends TOP(x)

  def CUDNN_CREATE_DROPOUT_DESCRIPTOR(convDesc: CUDNN_DROPOUT_DESCRIPTOR)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnCreateDropoutDescriptor", convDesc.x)(Seq(), Seq(0), Set[Int](0))

  def CUDNN_DESTROY_DROPOUT_DESCRIPTOR(convDesc: TOP)(implicit __pos: SourceContext) = 
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnDestroyDropoutDescriptor", convDesc.x)(Seq(), Seq(0), Set[Int]())
   
  def CUDNN_SET_DROPOUT_DESCRIPTOR(dropoutDesc: CUDNN_DROPOUT_DESCRIPTOR, handle: TOP, dropout: FLOAT, states: TOP,
                              stateSizeInBytes: SIZE_T, seed: INT)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnSetDropoutDescriptor", dropoutDesc.x, handle.x, dropout.x, states.x, 
      stateSizeInBytes.x, seed.x)(Seq(0,1,2,3,4,5), Seq(0,1,2,3), Set[Int]())

  
  def CUDNN_DROPOUT_GET_RESERVE_SPACE_SZ(xDesc: TOP, sizeInBytes: SIZE_T)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnDropoutGetReserveSpaceSize", xDesc.x, sizeInBytes.x)(Seq(0), Seq(1), Set[Int](1))
  
  def CUDNN_DROPOUT_GET_STATES_SZ(handle: TOP, sizeInBytes: SIZE_T)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnDropoutGetStatesSize", handle.x, sizeInBytes.x)(Seq(0), Seq(1), Set[Int](1))

  def CUDNN_DROPOUT_FWD(handle: TOP, dropoutDesc: CUDNN_DROPOUT_DESCRIPTOR, xdesc: TOP, x: TOP,
                          yDesc: TOP, y: TOP, reserveSpace: TOP, reserveSpaceSizeInBytes: SIZE_T)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT],"cudnnDropoutForward", handle.x, dropoutDesc.x, xdesc.x, x.x, yDesc.x, y.x, reserveSpace.x,
      reserveSpaceSizeInBytes.x)(Seq(0,1,2,3,4,7), Seq(5,6), Set[Int]())

  def CUDNN_DROPOUT_BWD(handle: TOP, dropoutDesc: CUDNN_DROPOUT_DESCRIPTOR, dyDesc: TOP, dy: TOP,
                          dxDesc: TOP, dx: TOP, reserveSpace: TOP, reserveSpaceSizeInBytes: SIZE_T)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT],"cudnnDropoutBackward", handle.x, dropoutDesc.x, dyDesc.x, dy.x, dxDesc.x, dx.x, reserveSpace.x,
      reserveSpaceSizeInBytes.x)(Seq(0,1,2,3,4,7), Seq(5,6), Set[Int]())
}

trait CUDNNDropoutOps extends CLibs with CudaOps with CUDNNBaseOps {

  class cudnnDropoutDescriptorT
  def cudnnDropoutDescriptor: Rep[cudnnDropoutDescriptorT] = newStruct[cudnnDropoutDescriptorT]("cudnnDropoutDescriptor_t")

  // cudnnStatus_t cudnnCreateDropoutDescriptor(cudnnDropoutDescriptor_t    *dropoutDesc)
  def cudnnCreateDropoutDescriptor(dropoutDesc: Rep[cudnnDropoutDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnCreateDropoutDescriptor", Unwrap(dropoutDesc))(Seq(), Seq(0), Set(0))

  // cudnnStatus_t cudnnStatus_t cudnnDestroyDropoutDescriptor(cudnnDropoutDescriptor_t dropoutDesc)
  def cudnnDestroyDropoutDescriptor(dropoutDesc: Rep[cudnnDropoutDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnDestroyDropoutDescriptor", Unwrap(dropoutDesc))(Seq(), Seq(0), Set())

  /*
  cudnnStatus_t cudnnSetDropoutDescriptor(
                                cudnnDropoutDescriptor_t    dropoutDesc,
                                cudnnHandle_t               handle,
                                float                       dropout,
                                void                       *states,
                                size_t                      stateSizeInBytes,
                                unsigned long long          seed)
  */
  def cudnnSetDropoutDescriptor(handle: Rep[cudnnHandleT], dropoutDesc: Rep[cudnnDropoutDescriptorT], dropout: Rep[Float], states: Rep[Array[_]],
                              stateSizeInBytes: Rep[SizeT], seed: Rep[Int]) =
    libFunction[cudnnStatusT]("cudnnSetDropoutDescriptor", Unwrap(dropoutDesc), Unwrap(handle), Unwrap(dropout), Unwrap(states), Unwrap(stateSizeInBytes),
      Unwrap(seed))(Seq(0,1,2,3,4,5), Seq(0,1,2,3), Set())
  

  /*
  cudnnStatus_t cudnnDropoutGetReserveSpaceSize(
                                cudnnTensorDescriptor_t     xDesc,
                                size_t                     *sizeInBytes)
  */
  def cudnnDropoutGetReserveSpaceSize(xDesc: Rep[cudnnTensorDescriptorT], sizeInBytes: Var[SizeT]) =
    libFunction[cudnnStatusT]("cudnnDropoutGetReserveSpaceSize", Unwrap(xDesc), UnwrapV(sizeInBytes))(Seq(0), Seq(1), Set(1))

  /*
  cudnnStatus_t cudnnDropoutGetStatesSize(
                                cudnnHandle_t       handle,
                                size_t             *sizeInBytes)
  */
  def cudnnDropoutGetStatesSize(handle: Rep[cudnnHandleT], sizeInBytes: Var[SizeT]) =
    libFunction[cudnnStatusT]("cudnnDropoutGetStatesSize", Unwrap(handle), UnwrapV(sizeInBytes))(Seq(0), Seq(1), Set(1))


  /*
  cudnnStatus_t cudnnDropoutForward(
                                cudnnHandle_t                       handle,
                                const cudnnDropoutDescriptor_t      dropoutDesc,
                                const cudnnTensorDescriptor_t       xdesc,
                                const void                         *x,
                                const cudnnTensorDescriptor_t       ydesc,
                                void                               *y,                  // pointer to data of the tensor described by the yDesc descriptor
                                void                               *reserveSpace,       // pointer to user-allocated GPU memory used by this function.
                                size_t                              reserveSpaceSizeInBytes)
  */
  def cudnnDropoutForward(handle: Rep[cudnnHandleT], dropoutDesc: Rep[cudnnDropoutDescriptorT], xdesc: Rep[cudnnTensorDescriptorT], x: Rep[Array[_]],
                          yDesc: Rep[cudnnTensorDescriptorT], y: Rep[Array[_]], reserveSpace: Rep[Array[_]], reserveSpaceSizeInBytes: Rep[SizeT]) =
    libFunction[cudnnStatusT]("cudnnDropoutForward", Unwrap(handle), Unwrap(dropoutDesc), Unwrap(xdesc), Unwrap(x), Unwrap(yDesc), Unwrap(y), Unwrap(reserveSpace),
      Unwrap(reserveSpaceSizeInBytes))(Seq(0,1,2,3,4,7), Seq(5,6), Set())

  /*
  cudnnStatus_t cudnnDropoutBackward(
                                cudnnHandle_t                   handle,
                                const cudnnDropoutDescriptor_t  dropoutDesc,
                                const cudnnTensorDescriptor_t   dydesc,
                                const void                     *dy,
                                const cudnnTensorDescriptor_t   dxdesc,
                                void                           *dx,
                                void                           *reserveSpace,
                                size_t                          reserveSpaceSizeInBytes)
  */
  def cudnnDropoutBackward(handle: Rep[cudnnHandleT], dropoutDesc: Rep[cudnnDropoutDescriptorT], dyDesc: Rep[cudnnTensorDescriptorT], dy: Rep[Array[_]],
                          dxDesc: Rep[cudnnTensorDescriptorT], dx: Rep[Array[_]], reserveSpace: Rep[Array[_]], reserveSpaceSizeInBytes: Rep[Int]) =
    libFunction[cudnnStatusT]("cudnnDropoutBackward", Unwrap(handle), Unwrap(dropoutDesc), Unwrap(dyDesc), Unwrap(dy), Unwrap(dxDesc), Unwrap(dx), Unwrap(reserveSpace),
      Unwrap(reserveSpaceSizeInBytes))(Seq(0,1,2,3,4,6,7), Seq(5), Set())

}