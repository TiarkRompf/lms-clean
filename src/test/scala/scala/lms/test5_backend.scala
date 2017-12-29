package scala.lms

import scala.annotation.implicitNotFound

class BackendTest extends TutorialFunSuite {
  val under = "backend-"

  test("01") {
    checkOut("01","scala", {

      val e = new Example
      e.test()

    })
  }

}