package lms.util

import lms.core.stub.{Adapter, Base}

import scala.collection.mutable.HashSet

import java.io.PrintWriter

trait Timing extends Base {
  def timeGeneratedCode[A: Manifest](f: => Rep[A], msg: Rep[String] = unit("")): Rep[A] = {
    val ff = Adapter.g.reify(Unwrap(f))
    Wrap[A](Adapter.g.reflectEffect("timeGenerated", Unwrap(msg), ff)(Adapter.g.getEffKeys(ff):_*))
  }
  def timestamp: Rep[Long] = Wrap[Long](Adapter.g.reflectEffect("timestamp")(Adapter.CTRL))
}

// trait CGenTiming extends CGenBase with GenericNestedCodegen {
//   val IR: TimingExp
//   import IR._
//
//   override def lowerNode[T:Manifest](sym: Sym[T], rhs: Def[T]) = rhs match {
//     case TimeGeneratedCode(start, end, f, msg) => {
//       LIRTraversal(f)
//       sym.atPhase(LIRLowering) {
//         reflectEffect(TimeGeneratedCode(start, end, LIRLowering(f), msg)).asInstanceOf[Exp[T]]
//       }
//     }
//     case _ => super.lowerNode(sym, rhs)
//   }
//
//   override def headerSet = super.headerSet ++ localSet
//
//   // override def emitFunctions(out: PrintWriter) = {
//   //   out.println(
//   //     """|int timeval_subtract(struct timeval *result, struct timeval *t2, struct timeval *t1) {
//   //       |long int diff = (t2->tv_usec + 1000000 * t2->tv_sec) - (t1->tv_usec + 1000000 * t1->tv_sec);
//   //       |result->tv_sec = diff / 1000000;
//   //       |result->tv_usec = diff % 1000000;
//   //       |return (diff<0);
//   //     |}""".stripMargin)
//   //   super.emitFunctions(out)
//   // }
//
//   override def emitNode(sym: Sym[Any], rhs: Def[Any]) =  {
//     rhs match {
//       case t@TimeGeneratedCode(start, end, f, Const(msg)) =>
//         gen"struct timeval t_$start, t_$end;"
//         gen"gettimeofday(&t_$start, NULL);"
//         emitBlock(f)
//         gen"gettimeofday(&t_$end, NULL);"
//         emitValDef(t.diff, src"t_$end.tv_usec + 1000000L * t_$end.tv_sec - (t_$start.tv_usec + 1000000L * t_$start.tv_sec)")
//         gen"""fprintf(stderr,"$msg: Generated Code Profiling Info: Operation completed in %ld milliseconds\n", ${t.diff}/1000L);"""
//       case t@Timestamp() =>
//         gen"struct timeval ${t.t};"
//         gen"gettimeofday(&${t.t}, NULL);"
//         emitValDef(sym, src"${t.t}.tv_usec + 1000000L * ${t.t}.tv_sec")
//       case _ => super.emitNode(sym, rhs)
//     }
//   }
// }
