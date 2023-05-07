package com.github.unthingable
import org.specs2.*
import org.specs2.mutable.SpecificationWithJUnit

class UtilTest extends SpecificationWithJUnit:
  import Util.*

  "step rounding" >> {
    3.next(2) must equalTo(4)
    3.prev(2) must equalTo(2)
    3.next(3) must equalTo(3)
    3.prev(3) must equalTo(3)

    2.3.next(2.5) must equalTo(2.5)
    2.7.next(2.5) must equalTo(5.0)
    2.5.next(2.5) must equalTo(2.5)
    2.5.prev(2.5) must equalTo(2.5)
    2.3.prev(2.5) must equalTo(0.0)
  }
