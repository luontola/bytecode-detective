// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.specs._
import org.specs.runner._

object allSpecs extends Specification {
  "Interpreting bytecode instructions" isSpecifiedBy (
          new interpretingSpec,
          new interpretingLocalVariableInsnSpec,
          new interpretingStackInsnSpec
          )
}

class allSuite extends JUnit4(allSpecs)
