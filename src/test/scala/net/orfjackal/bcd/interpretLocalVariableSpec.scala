// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.specs._
import org.specs.runner._

object interpretLocalVariableSpec extends Specification {
  "Loading unknown local variables" should {
    def exec(insn: AbstractInsnNode) = {
      val stack = List()
      val locals = Map(
        0 -> UnknownValue(),
        1 -> UnknownValue())
      val c = new MethodContext(stack, locals)
      c.execute(insn)
    }
    "ILOAD" in {
      val c = exec(new VarInsnNode(Opcodes.ILOAD, 0))
      c.stack must_== List(KnownType(classOf[Int]))
      c.locals must_== Map(
        0 -> KnownType(classOf[Int]),
        1 -> UnknownValue())
    }
    "LLOAD" in {
      val c = exec(new VarInsnNode(Opcodes.LLOAD, 0))
      c.stack must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
      c.locals must_== Map(
        0 -> KnownType(classOf[Long]),
        1 -> KnownType(classOf[Long]))
    }
    "FLOAD" in {
      val c = exec(new VarInsnNode(Opcodes.FLOAD, 0))
      c.stack must_== List(KnownType(classOf[Float]))
      c.locals must_== Map(
        0 -> KnownType(classOf[Float]),
        1 -> UnknownValue())
    }
    "DLOAD" in {
      val c = exec(new VarInsnNode(Opcodes.DLOAD, 0))
      c.stack must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
      c.locals must_== Map(
        0 -> KnownType(classOf[Double]),
        1 -> KnownType(classOf[Double]))
    }
    "ALOAD" in {
      val c = exec(new VarInsnNode(Opcodes.ALOAD, 0))
      c.stack must_== List(KnownType(classOf[Object]))
      c.locals must_== Map(
        0 -> KnownType(classOf[Object]),
        1 -> UnknownValue())
    }
  }

  "Loading known local variables" should {
    // In reality, the system will store each 64-bit value (long, double) duplicated
    // to each local variable slot, so that the interpreter would be simpler, but
    // in this test we use different values, so that we could test that their order
    // is not accidentally swapped.
    def exec(insn: AbstractInsnNode) = {
      val stack = List()
      val locals = Map[Int, Value](
        0 -> KnownValue(10, classOf[Int]),
        1 -> KnownValue(11L, classOf[Long]),
        2 -> KnownValue(12L, classOf[Long]),
        3 -> KnownValue(1.3F, classOf[Float]),
        4 -> KnownValue(1.4, classOf[Double]),
        5 -> KnownValue(1.5, classOf[Double]),
        6 -> KnownRef("x", classOf[String]))
      val c = new MethodContext(stack, locals)
      val c2 = c.execute(insn)
      c2.locals must_== locals
      c2
    }
    def stackAfter(insn: AbstractInsnNode) = {
      exec(insn).stack
    }
    "ILOAD" in {
      stackAfter(new VarInsnNode(Opcodes.ILOAD, 0)) must_== List(KnownValue(10, classOf[Int]))
    }
    "LLOAD" in {
      stackAfter(new VarInsnNode(Opcodes.LLOAD, 1)) must_== List(KnownValue(11L, classOf[Long]), KnownValue(12L, classOf[Long]))
    }
    "FLOAD" in {
      stackAfter(new VarInsnNode(Opcodes.FLOAD, 3)) must_== List(KnownValue(1.3F, classOf[Float]))
    }
    "DLOAD" in {
      stackAfter(new VarInsnNode(Opcodes.DLOAD, 4)) must_== List(KnownValue(1.4, classOf[Double]), KnownValue(1.5, classOf[Double]))
    }
    "ALOAD" in {
      stackAfter(new VarInsnNode(Opcodes.ALOAD, 6)) must_== List(KnownRef("x", classOf[String]))
    }
  }

  "Storing unknown local variables" should {
    def exec(insn: AbstractInsnNode) = {
      val stack = List(UnknownValue(), UnknownValue())
      val locals = Map[Int, Value]()
      val c = new MethodContext(stack, locals)
      c.execute(insn)
    }
    "ISTORE" in {
      val c = exec(new VarInsnNode(Opcodes.ISTORE, 0))
      c.stack must_== List(UnknownValue())
      c.locals must_== Map(
        0 -> KnownType(classOf[Int]))
    }
    "LSTORE" in {
      val c = exec(new VarInsnNode(Opcodes.LSTORE, 0))
      c.stack must_== List()
      c.locals must_== Map(
        0 -> KnownType(classOf[Long]),
        1 -> KnownType(classOf[Long]))
    }
    "FSTORE" in {
      val c = exec(new VarInsnNode(Opcodes.FSTORE, 0))
      c.stack must_== List(UnknownValue())
      c.locals must_== Map(
        0 -> KnownType(classOf[Float]))
    }
    "DSTORE" in {
      val c = exec(new VarInsnNode(Opcodes.DSTORE, 0))
      c.stack must_== List()
      c.locals must_== Map(
        0 -> KnownType(classOf[Double]),
        1 -> KnownType(classOf[Double]))
    }
    "ASTORE" in {
      val c = exec(new VarInsnNode(Opcodes.ASTORE, 0))
      c.stack must_== List(UnknownValue())
      c.locals must_== Map(
        0 -> KnownType(classOf[Object]))
    }
  }

  "Storing known local variables" should {
    def exec(insn: AbstractInsnNode, stack: List[Value]) = {
      val locals = Map[Int, Value]()
      val c = new MethodContext(stack, locals)
      c.execute(insn)
    }
    "ISTORE" in {
      val stack = List(KnownValue(10, classOf[Int]))
      val c = exec(new VarInsnNode(Opcodes.ISTORE, 0), stack)
      c.stack must_== List()
      c.locals must_== Map(
        0 -> KnownValue(10, classOf[Int]))
    }
    "LSTORE" in {
      val stack = List(KnownValue(11L, classOf[Long]), KnownValue(12L, classOf[Long]))
      val c = exec(new VarInsnNode(Opcodes.LSTORE, 0), stack)
      c.stack must_== List()
      c.locals must_== Map(
        0 -> KnownValue(11L, classOf[Long]),
        1 -> KnownValue(12L, classOf[Long]))
    }
    "FSTORE" in {
      val stack = List(KnownValue(1.3F, classOf[Float]))
      val c = exec(new VarInsnNode(Opcodes.FSTORE, 0), stack)
      c.stack must_== List()
      c.locals must_== Map(
        0 -> KnownValue(1.3F, classOf[Float]))
    }
    "DSTORE" in {
      val stack = List(KnownValue(1.4, classOf[Double]), KnownValue(1.5, classOf[Double]))
      val c = exec(new VarInsnNode(Opcodes.DSTORE, 0), stack)
      c.stack must_== List()
      c.locals must_== Map(
        0 -> KnownValue(1.4, classOf[Double]),
        1 -> KnownValue(1.5, classOf[Double]))
    }
    "ASTORE" in {
      val stack = List(KnownRef("x", classOf[String]))
      val c = exec(new VarInsnNode(Opcodes.ASTORE, 0), stack)
      c.stack must_== List()
      c.locals must_== Map(
        0 -> KnownRef("x", classOf[String]))
    }
  }

  "Increment integer in local variable" should {
    def exec(insn: AbstractInsnNode) = {
      val stack = List()
      val locals = Map[Int, Value](
        0 -> KnownValue(10, classOf[Int]))
      val c = new MethodContext(stack, locals)
      c.execute(insn)
    }
    "IINC" in {
      val c = exec(new IincInsnNode(0, 1))
      c.stack must_== List()
      c.locals must_== Map(
        0 -> KnownType(classOf[Int]))
    }
  }
}
