// package lms.thirdparty.array_computation

// import lms.core._
// import lms.util._
// import lms.core.stub._
// import lms.core.Backend._
// import lms.core.virtualize
// import lms.core.utils.time
// import lms.macros.{SourceContext, RefinedManifest}
// import lms.collection.mutable.ArrayOps
// import lms.thirdparty.{CBLASOps, CudaOps}

// /**
//  * This frontend is used for Tensor Computation by Devices (CPU or GPU)
//  * However, it is implemented at array level because many tensor computation libraries directly
//  *     works with arrays.
//  *
//  * We are also trying to present two styles of frontents: the typeless and the typed
//  *    The typeless frontend is easier to use in IR transformation.
//  *    The typed frontend is more intuitive to use directly by the DSL user.
//  * Likely the typed frontend will just be a shallow wrapper of the typeless frontend.
//  */
// trait ArrayComputationOps extends Dsl with ArrayCPUOps with CudaOps {

//   def ARRAY_ADD(a: ARRAY, b: ARRAY, res: ARRAY, size: INT, device: Device)(implicit __pos: SourceContext): Rep[Unit ] = device match {
//     case CPU(_) => ARRAY_ADD(a, b, res, size) // calling the CPU version
//     case GPU(_) => CUDA_ARRAY_ADD(a, b, res, size); () // calling the GPU version
//   }

// }

