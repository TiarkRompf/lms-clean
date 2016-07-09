/**
Auto-Generating the IR
======================

*/

package scala.lms

import scala.annotation.implicitNotFound

class AutoTest extends TutorialFunSuite {
  val under = "auto-"

  test("one") {
    val res = utils.captureOut {

      case class Exp(data:String)

      abstract class Typ[T] {
        def encode(x:T): Exp
        def decode(x:Exp): T
      }
      def typ[T:Typ] = implicitly[Typ[T]]

      case class Thunk(f: () => Exp) {
        override def toString = "<"+f()+">"
      }

      def reflect[T:Typ](s:String,xs: Thunk*):T = typ[T].decode(Exp(s+","+xs.mkString(",")))
      def ref[T:Typ](f: => T): Thunk = Thunk(() => typ[T].encode(f))

      case class Rewrite[T:Typ](a:T, b:T)

      def lower[A:Typ,B:Typ,C:Typ](f: (A,B) => Rewrite[C]): Unit = {
        val a = typ[A].decode(Exp("?A"))
        val b = typ[B].decode(Exp("?B"))
        val rw = f(a,b)
        val u = typ[C].encode(rw.a)
        val v = typ[C].encode(rw.b)
        println("lower: " + u + "===>" + v)
      }


      // ----

      case class MyInt(s:String)

      implicit def intTyp = new Typ[MyInt] {
        def encode(x:MyInt): Exp = Exp(x.s)
        def decode(x:Exp): MyInt = MyInt(x.data)  
      }

      implicit def intX(x:Int) = MyInt(x.toString)
      

      @ir def test1(x: MyInt, y: => MyInt): MyInt

      @ir def test2(x: MyInt, y: MyInt): MyInt = 666


      println(test1(3,7))

      println(test2(3,7))

      println(test2_next(3,7))

    }
    check("one", res)
  }

  test("two") {

    trait IRobot {
      def foo
    }

/*
public class ProxyTest {
    public static void main(String args[]) {
        IRobot robot = (IRobot) java.lang.reflect.Proxy.newProxyInstance(
                IRobot.class.getClassLoader(),
                new java.lang.Class[] { IRobot.class },
                new java.lang.reflect.InvocationHandler() {

            @Override
            public Object invoke(Object proxy, java.lang.reflect.Method method, Object[] args) throws java.lang.Throwable {
                String method_name = method.getName();
                Class<?>[] classes = method.getParameterTypes();

                if (method_name.equals("Name")) {
                    if (args == null) {
                        return "Mr IRobot";
                    } else {
                        return args[0] + " IRobot";
                    }
                } else if (method_name.equals("Talk")) {
                    switch (classes.length) {
                        case 0:
                            System.out.println("Hello");
                            break;
                        case 1:
                            if (classes[0] == int.class) {
                                System.out.println("Hi. Int: " + args[0]);
                            } else {
                                System.out.println("Hi. String: " + args[0]);
                            }
                            break;
                        case 2:
                            if (classes[0] == String.class) {
                                System.out.println("Hi. String: " + args[0] + ". Int: " + args[1]);
                            } else {
                                if (classes[1] == String.class) {
                                    System.out.println("Hi. int: " + args[0] + ". String: " + args[1]);
                                } else {
                                    System.out.println("Hi. int: " + args[0] + ". Int: " + args[1]);
                                }
                            }
                            break;
                    }
                }
                return null;
            }
        });

        System.out.println(robot.Name());
        System.out.println(robot.Name("Dr"));
        robot.Talk();
        robot.Talk("stuff");
        robot.Talk(100);
        robot.Talk("stuff", 200);
        robot.Talk(300, 400);
        robot.Talk(500, "stuff");
    }

*/
  }
}