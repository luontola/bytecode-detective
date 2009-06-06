// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.specs._
import org.specs.runner._

class interpretingBytecodeInstructionsSpec extends Specification {
  "When no commands have been executed" should {
    "the stack is empty" in {
      var c = new MethodContext
      c.stack must_== List()
    }
  }

  "Loading local variables" should {
    def stackAfter(insn: AbstractInsnNode) = {
      val c = new MethodContext()
      c.execute(insn).stack
    }
    "ILOAD" in {
      stackAfter(new VarInsnNode(Opcodes.ILOAD, 0)) must_== List(KnownType(classOf[Int]))
    }
    "LLOAD" in {
      stackAfter(new VarInsnNode(Opcodes.LLOAD, 0)) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "FLOAD" in {
      stackAfter(new VarInsnNode(Opcodes.FLOAD, 0)) must_== List(KnownType(classOf[Float]))
    }
    "DLOAD" in {
      stackAfter(new VarInsnNode(Opcodes.DLOAD, 0)) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }
    "ALOAD" in {
      stackAfter(new VarInsnNode(Opcodes.ALOAD, 0)) must_== List(KnownType(classOf[Object]))
    }
  }

  "Storing local variables" should {
    def stackAfter(insn: AbstractInsnNode) = {
      val stackBefore = List(UnknownValue(), UnknownValue())
      val c = new MethodContext(stackBefore)
      c.execute(insn).stack
    }
    "ISTORE" in {
      stackAfter(new VarInsnNode(Opcodes.ISTORE, 0)) must_== List(UnknownValue())
    }
    "LSTORE" in {
      stackAfter(new VarInsnNode(Opcodes.LSTORE, 0)) must_== List()
    }
    "FSTORE" in {
      stackAfter(new VarInsnNode(Opcodes.FSTORE, 0)) must_== List(UnknownValue())
    }
    "DSTORE" in {
      stackAfter(new VarInsnNode(Opcodes.DSTORE, 0)) must_== List()
    }
    "ASTORE" in {
      stackAfter(new VarInsnNode(Opcodes.ASTORE, 0)) must_== List(UnknownValue())
    }
  }

  "Unsupported bytecode V1_5 commands" should {
    // See ASM Guide 3.0, Appendix 2. 
    // Use the org.objectweb.asm.commons.JSRInlinerAdapter class
    // to remove JSR and RET instructions in order to simplify code analysis.
    "JSR" in { // Jump to SubRoutine
      val c = new MethodContext()
      c.execute(new JumpInsnNode(Opcodes.JSR, new LabelNode())) must throwA[IllegalArgumentException]
    }
    "RET" in { // RETurn from subroutine
      val c = new MethodContext()
      c.execute(new VarInsnNode(Opcodes.RET, 0)) must throwA[IllegalArgumentException]
    }
  }
}
