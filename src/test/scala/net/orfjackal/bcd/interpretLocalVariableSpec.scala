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
    "ALOAD" in {
      val c = exec(new VarInsnNode(Opcodes.ALOAD, 0))
      c.stack must_== List(KnownType(classOf[Object]))
      c.locals must_== Map(
        0 -> KnownType(classOf[Object]),
        1 -> UnknownValue())
    }
  }

  "Loading known local variables" should {
    def exec(insn: AbstractInsnNode) = {
      val stack = List()
      val locals = Map[Int, Value](
        0 -> KnownValue(40, classOf[Int]),
        1 -> KnownValue(50L, classOf[Long]),
        2 -> KnownValue(60L, classOf[Long]),
        3 -> KnownRef("x", classOf[String]))
      val c = new MethodContext(stack, locals)
      c.execute(insn)
    }
    "ILOAD" in {
      val c = exec(new VarInsnNode(Opcodes.ILOAD, 0))
      c.stack must_== List(KnownValue(40, classOf[Int]))
      c.locals must_== Map(
        0 -> KnownValue(40, classOf[Int]),
        1 -> KnownValue(50L, classOf[Long]),
        2 -> KnownValue(60L, classOf[Long]),
        3 -> KnownRef("x", classOf[String]))
    }
    "LLOAD" in {
      val c = exec(new VarInsnNode(Opcodes.LLOAD, 1))
      c.stack must_== List(KnownValue(50L, classOf[Long]), KnownValue(60L, classOf[Long]))
      c.locals must_== Map(
        0 -> KnownValue(40, classOf[Int]),
        1 -> KnownValue(50L, classOf[Long]),
        2 -> KnownValue(60L, classOf[Long]),
        3 -> KnownRef("x", classOf[String]))
    }
    "ALOAD" in {
      val c = exec(new VarInsnNode(Opcodes.ALOAD, 3))
      c.stack must_== List(KnownRef("x", classOf[String]))
      c.locals must_== Map(
        0 -> KnownValue(40, classOf[Int]),
        1 -> KnownValue(50L, classOf[Long]),
        2 -> KnownValue(60L, classOf[Long]),
        3 -> KnownRef("x", classOf[String]))
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
      val stack = List(KnownValue(40, classOf[Int]))
      val c = exec(new VarInsnNode(Opcodes.ISTORE, 0), stack)
      c.stack must_== List()
      c.locals must_== Map(
        0 -> KnownValue(40, classOf[Int]))
    }
    "LSTORE" in {
      val stack = List(KnownValue(50L, classOf[Long]), KnownValue(60L, classOf[Long]))
      val c = exec(new VarInsnNode(Opcodes.LSTORE, 0), stack)
      c.stack must_== List()
      c.locals must_== Map(
        0 -> KnownValue(50L, classOf[Long]),
        1 -> KnownValue(60L, classOf[Long]))
    }
    "ASTORE" in {
      val stack = List(KnownRef("x", classOf[String]))
      val c = exec(new VarInsnNode(Opcodes.ASTORE, 0), stack)
      c.stack must_== List()
      c.locals must_== Map(
        0 -> KnownRef("x", classOf[String]))
    }
  }
}
