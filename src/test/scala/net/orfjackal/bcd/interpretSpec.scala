// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.specs._
object interpretSpec extends Specification {
  "When no commands have been executed" >> {
    "the stack is empty" >> {
      var c = new MethodContext()
      c.stack must_== List()
    }
    "the locals are empty" >> {
      var c = new MethodContext()
      c.locals must_== Map.empty
    }
  }

  "Unsupported bytecode V1_5 commands" >> {
    // See ASM Guide 3.0, Appendix 2. 
    // Use the org.objectweb.asm.commons.JSRInlinerAdapter class
    // to remove JSR and RET instructions in order to simplify code analysis.
    "JSR" >> { // Jump to SubRoutine
      val c = new MethodContext()
      c.execute(new JumpInsnNode(Opcodes.JSR, new LabelNode())) must throwA[IllegalArgumentException]
    }
    "RET" >> { // RETurn from subroutine
      val c = new MethodContext()
      c.execute(new VarInsnNode(Opcodes.RET, 0)) must throwA[IllegalArgumentException]
    }
  }
}
