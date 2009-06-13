// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.specs._
import org.specs.runner._

object interpretArithmeticLogicSpec extends Specification {
  "Executing arithmetic and logic on unknown values" should {
    val ORIG_STACK_SIZE = 4
    def exec(insn: AbstractInsnNode) = {
      val stack = List(UnknownValue(), UnknownValue(), UnknownValue(), UnknownValue())
      val c = new MethodContext(stack, Map.empty)
      c.stack.size must_== ORIG_STACK_SIZE
      c.execute(insn)
    }

    "IADD" in {
      val c = exec(new InsnNode(Opcodes.IADD))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LADD" in {
      val c = exec(new InsnNode(Opcodes.LADD))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "FADD" in {
      val c = exec(new InsnNode(Opcodes.FADD))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Float])
    }
    "DADD" in {
      val c = exec(new InsnNode(Opcodes.DADD))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }

    "ISUB" in {
      val c = exec(new InsnNode(Opcodes.ISUB))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LSUB" in {
      val c = exec(new InsnNode(Opcodes.LSUB))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "FSUB" in {
      val c = exec(new InsnNode(Opcodes.FSUB))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Float])
    }
    "DSUB" in {
      val c = exec(new InsnNode(Opcodes.DSUB))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }

    "IMUL" in {
      val c = exec(new InsnNode(Opcodes.IMUL))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LMUL" in {
      val c = exec(new InsnNode(Opcodes.LMUL))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "FMUL" in {
      val c = exec(new InsnNode(Opcodes.FMUL))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Float])
    }
    "DMUL" in {
      val c = exec(new InsnNode(Opcodes.DMUL))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }

    "IDIV" in {
      val c = exec(new InsnNode(Opcodes.IDIV))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LDIV" in {
      val c = exec(new InsnNode(Opcodes.LDIV))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "FDIV" in {
      val c = exec(new InsnNode(Opcodes.FDIV))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Float])
    }
    "DDIV" in {
      val c = exec(new InsnNode(Opcodes.DDIV))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }

    "IREM" in {
      val c = exec(new InsnNode(Opcodes.IREM))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LREM" in {
      val c = exec(new InsnNode(Opcodes.LREM))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "FREM" in {
      val c = exec(new InsnNode(Opcodes.FREM))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Float])
    }
    "DREM" in {
      val c = exec(new InsnNode(Opcodes.DREM))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }

    "INEG" in {
      val c = exec(new InsnNode(Opcodes.INEG))
      c.stack.size must_== ORIG_STACK_SIZE - 1 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LNEG" in {
      val c = exec(new InsnNode(Opcodes.LNEG))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "FNEG" in {
      val c = exec(new InsnNode(Opcodes.FNEG))
      c.stack.size must_== ORIG_STACK_SIZE - 1 + 1
      c.stack.head must_== KnownType(classOf[Float])
    }
    "DNEG" in {
      val c = exec(new InsnNode(Opcodes.DNEG))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }

    "ISHL" in {
      val c = exec(new InsnNode(Opcodes.ISHL))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LSHL" in {
      val c = exec(new InsnNode(Opcodes.LSHL))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "ISHR" in {
      val c = exec(new InsnNode(Opcodes.ISHR))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LSHR" in {
      val c = exec(new InsnNode(Opcodes.LSHR))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "IUSHR" in {
      val c = exec(new InsnNode(Opcodes.IUSHR))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LUSHR" in {
      val c = exec(new InsnNode(Opcodes.LUSHR))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }

    "IAND" in {
      val c = exec(new InsnNode(Opcodes.IAND))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LAND" in {
      val c = exec(new InsnNode(Opcodes.LAND))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "IOR" in {
      val c = exec(new InsnNode(Opcodes.IOR))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LOR" in {
      val c = exec(new InsnNode(Opcodes.LOR))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "IXOR" in {
      val c = exec(new InsnNode(Opcodes.IXOR))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LXOR" in {
      val c = exec(new InsnNode(Opcodes.LXOR))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }

    "LCMP" in {
      val c = exec(new InsnNode(Opcodes.LCMP))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "FCMPL" in {
      val c = exec(new InsnNode(Opcodes.FCMPL))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "FCMPG" in {
      val c = exec(new InsnNode(Opcodes.FCMPG))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "DCMPL" in {
      val c = exec(new InsnNode(Opcodes.DCMPL))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "DCMPG" in {
      val c = exec(new InsnNode(Opcodes.DCMPG))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
  }
}
