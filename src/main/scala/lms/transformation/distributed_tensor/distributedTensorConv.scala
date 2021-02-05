package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}
import lms.thirdparty.{CUDNNTypeLess, CUDNNOps}
import lms.transformation.util.ConvParam

import Backend._


trait FixedSizeDistributedTensorConvTypeLess extends FixedSizeDistributedTensorMutationTypeLess with ConvParam {

  def ConvBackwardData(weight: TENSOR, filter: TENSOR, doutput: TENSOR, params: ConvParam, anno: Anno, __pos: SourceContext): TENSOR = {
    assert(weight.et == filter.et && filter.et == doutput.et)
    val res_tt = weight.tensor_type
    (new TENSOR(Adapter.g.reflectRead("tensor_conv_bwd_data", C(res_tt), C(anno), 
      weight.x, filter.x, doutput.x, C(params))(weight.x, filter.x, doutput.x)).withSrcType(__pos, weight.et))
  }

  def ConvBackwardFilter(weight: TENSOR, filter: TENSOR, doutput: TENSOR, params: ConvParam, anno: Anno, __pos: SourceContext): TENSOR = {
    assert(weight.et == filter.et && filter.et == doutput.et)
    val res_tt = filter.tensor_type
    (new TENSOR(Adapter.g.reflectRead("tensor_conv_bwd_filter", C(res_tt), C(anno), 
      weight.x, filter.x, doutput.x, C(params))(weight.x, filter.x, doutput.x)).withSrcType(__pos, weight.et))
  }

  override def mergable_dims(node: Node) = node match {
    // constraints: 
    // input.channels = filter.input_channels
    // output.channels = filter.output_channels
    // NHWC
    case Node(s, "tensor_conv", Backend.Const(anno:Anno)::(a:Backend.Sym)::(b:Backend.Sym)::Backend.Const(params:ConvParam)::_, _) =>
      val input_type = (new TENSOR(a, useOldMetadata=true)).tensor_type.shape
      val filter_type = (new TENSOR(b, useOldMetadata=true)).tensor_type.shape
      val output_type = (new TENSOR(s, useOldMetadata=true)).tensor_type.shape
      val inputC = input_type(3).dim
      val outputC = output_type(3).dim
      val filterCout = filter_type(0).dim
      val filterCin = filter_type(3).dim
      List((inputC, filterCin), (outputC, filterCout))

      // (x_type.shape zip y_type.shape).toList map { case (a:Size, b:Size) => (a.dim, b.dim) }
    case _ => super.mergable_dims(node)
  }

  override def aircopCollect(node: Node, forwardNodes: mutable.ArrayBuffer[Node],
    weightNodes: mutable.ArrayBuffer[Node], backwardNodes: mutable.ArrayBuffer[()=>Unit],
    gradMap: mutable.HashMap[Backend.Sym, TENSOR],
    momentumMap: mutable.HashMap[Backend.Sym, TENSOR],
    transform: Backend.Exp => Backend.Exp) = node match {
      case Node(s, "tensor_conv", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::(b:Backend.Sym)::Backend.Const(params:ConvParam)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        // save forward op in forwardNodes
        forwardNodes += node
        // save backward op in backwardNodes
        val x = new TENSOR(transform(a))      // weight
        val y = new TENSOR(transform(b))      // filter

        (() => {
          val a_grad = ConvBackwardData(x, y, gradMap(s), params, anno, pos)
          Accumulate(gradMap(a), a_grad, anno); ()
        }) +=: backwardNodes
        (() => {
          val b_grad = ConvBackwardFilter(x, y, gradMap(s), params, anno, pos)
          Accumulate(gradMap(b), b_grad, anno); ()
        }) +=: backwardNodes

      case _ => super.aircopCollect(node, forwardNodes, weightNodes, backwardNodes, gradMap, momentumMap, transform)
    }
}
