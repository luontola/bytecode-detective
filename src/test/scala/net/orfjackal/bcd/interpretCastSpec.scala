// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.specs._
import org.specs.runner._

object interpretCastSpec extends Specification {
  "Casting primitive types" should {
    val ORIG_STACK_SIZE = 2
    def exec(insn: AbstractInsnNode) = {
      val stack = List(UnknownValue(), UnknownValue())
      val c = new MethodContext(stack, Map.empty)
      c.stack.size must_== ORIG_STACK_SIZE
      c.execute(insn)
    }

    "I2B" in {
      val c = exec(new InsnNode(Opcodes.I2B))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[Byte])
    }
    "I2C" in {
      val c = exec(new InsnNode(Opcodes.I2C))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[Char])
    }
    "I2S" in {
      val c = exec(new InsnNode(Opcodes.I2S))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[Short])
    }

    "L2I" in {
      val c = exec(new InsnNode(Opcodes.L2I))
      c.stack.size must_== ORIG_STACK_SIZE - 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "F2I" in {
      val c = exec(new InsnNode(Opcodes.F2I))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[Int])
    }
    "D2I" in {
      val c = exec(new InsnNode(Opcodes.D2I))
      c.stack.size must_== ORIG_STACK_SIZE - 1
      c.stack.head must_== KnownType(classOf[Int])
    }

    "I2L" in {
      val c = exec(new InsnNode(Opcodes.I2L))
      c.stack.size must_== ORIG_STACK_SIZE + 1
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "F2L" in {
      val c = exec(new InsnNode(Opcodes.F2L))
      c.stack.size must_== ORIG_STACK_SIZE + 1
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "D2L" in {
      val c = exec(new InsnNode(Opcodes.D2L))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }

    "I2F" in {
      val c = exec(new InsnNode(Opcodes.I2F))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[Float])
    }
    "L2F" in {
      val c = exec(new InsnNode(Opcodes.L2F))
      c.stack.size must_== ORIG_STACK_SIZE - 1
      c.stack.head must_== KnownType(classOf[Float])
    }
    "D2F" in {
      val c = exec(new InsnNode(Opcodes.D2F))
      c.stack.size must_== ORIG_STACK_SIZE - 1
      c.stack.head must_== KnownType(classOf[Float])
    }

    "I2D" in {
      val c = exec(new InsnNode(Opcodes.I2D))
      c.stack.size must_== ORIG_STACK_SIZE + 1
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }
    "L2D" in {
      val c = exec(new InsnNode(Opcodes.L2D))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }
    "F2D" in {
      val c = exec(new InsnNode(Opcodes.F2D))
      c.stack.size must_== ORIG_STACK_SIZE + 1
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }
  }

  "Casting objects" should {
    val ORIG_STACK_SIZE = 1
    def exec(arg: Value, insn: AbstractInsnNode) = {
      val stack = List(arg)
      val c = new MethodContext(stack, Map.empty)
      c.stack.size must_== ORIG_STACK_SIZE
      c.execute(insn)
    }

    "CHECKCAST unknown" in {
      val c = exec(UnknownValue(), new TypeInsnNode(Opcodes.CHECKCAST, "java/lang/String"))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[String])
    }
    "CHECKCAST known type" in {
      val c = exec(KnownType(classOf[String]), new TypeInsnNode(Opcodes.CHECKCAST, "java/lang/String"))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[String])
    }
    "CHECKCAST known ref" in {
      val c = exec(KnownRef("x", classOf[String]), new TypeInsnNode(Opcodes.CHECKCAST, "java/lang/String"))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownRef("x", classOf[String])
    }
  }
}
