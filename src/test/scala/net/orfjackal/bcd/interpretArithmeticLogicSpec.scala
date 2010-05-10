// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.specs._
object interpretArithmeticLogicSpec extends Specification {
  "Executing arithmetic and logic on unknown values" >> {
    val ORIG_STACK_SIZE = 4
    def exec(insn: AbstractInsnNode) = {
      val stack = List(UnknownValue(), UnknownValue(), UnknownValue(), UnknownValue())
      val c = new MethodContext(stack, Map.empty)
      c.stack.size must_== ORIG_STACK_SIZE
      c.execute(insn)
    }

    "IADD" >> {
      val c = exec(new InsnNode(Opcodes.IADD))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LADD" >> {
      val c = exec(new InsnNode(Opcodes.LADD))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "FADD" >> {
      val c = exec(new InsnNode(Opcodes.FADD))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Float])
    }
    "DADD" >> {
      val c = exec(new InsnNode(Opcodes.DADD))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }

    "ISUB" >> {
      val c = exec(new InsnNode(Opcodes.ISUB))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LSUB" >> {
      val c = exec(new InsnNode(Opcodes.LSUB))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "FSUB" >> {
      val c = exec(new InsnNode(Opcodes.FSUB))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Float])
    }
    "DSUB" >> {
      val c = exec(new InsnNode(Opcodes.DSUB))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }

    "IMUL" >> {
      val c = exec(new InsnNode(Opcodes.IMUL))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LMUL" >> {
      val c = exec(new InsnNode(Opcodes.LMUL))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "FMUL" >> {
      val c = exec(new InsnNode(Opcodes.FMUL))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Float])
    }
    "DMUL" >> {
      val c = exec(new InsnNode(Opcodes.DMUL))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }

    "IDIV" >> {
      val c = exec(new InsnNode(Opcodes.IDIV))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LDIV" >> {
      val c = exec(new InsnNode(Opcodes.LDIV))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "FDIV" >> {
      val c = exec(new InsnNode(Opcodes.FDIV))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Float])
    }
    "DDIV" >> {
      val c = exec(new InsnNode(Opcodes.DDIV))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }

    "IREM" >> {
      val c = exec(new InsnNode(Opcodes.IREM))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LREM" >> {
      val c = exec(new InsnNode(Opcodes.LREM))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "FREM" >> {
      val c = exec(new InsnNode(Opcodes.FREM))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Float])
    }
    "DREM" >> {
      val c = exec(new InsnNode(Opcodes.DREM))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }

    "INEG" >> {
      val c = exec(new InsnNode(Opcodes.INEG))
      c.stack.size must_== ORIG_STACK_SIZE - 1 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LNEG" >> {
      val c = exec(new InsnNode(Opcodes.LNEG))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "FNEG" >> {
      val c = exec(new InsnNode(Opcodes.FNEG))
      c.stack.size must_== ORIG_STACK_SIZE - 1 + 1
      c.stack.head must_== KnownType(classOf[Float])
    }
    "DNEG" >> {
      val c = exec(new InsnNode(Opcodes.DNEG))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }

    "ISHL" >> {
      val c = exec(new InsnNode(Opcodes.ISHL))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LSHL" >> {
      val c = exec(new InsnNode(Opcodes.LSHL))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "ISHR" >> {
      val c = exec(new InsnNode(Opcodes.ISHR))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LSHR" >> {
      val c = exec(new InsnNode(Opcodes.LSHR))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "IUSHR" >> {
      val c = exec(new InsnNode(Opcodes.IUSHR))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LUSHR" >> {
      val c = exec(new InsnNode(Opcodes.LUSHR))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }

    "IAND" >> {
      val c = exec(new InsnNode(Opcodes.IAND))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LAND" >> {
      val c = exec(new InsnNode(Opcodes.LAND))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "IOR" >> {
      val c = exec(new InsnNode(Opcodes.IOR))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LOR" >> {
      val c = exec(new InsnNode(Opcodes.LOR))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "IXOR" >> {
      val c = exec(new InsnNode(Opcodes.IXOR))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "LXOR" >> {
      val c = exec(new InsnNode(Opcodes.LXOR))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }

    "LCMP" >> {
      val c = exec(new InsnNode(Opcodes.LCMP))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "FCMPL" >> {
      val c = exec(new InsnNode(Opcodes.FCMPL))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "FCMPG" >> {
      val c = exec(new InsnNode(Opcodes.FCMPG))
      c.stack.size must_== ORIG_STACK_SIZE - 2 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "DCMPL" >> {
      val c = exec(new InsnNode(Opcodes.DCMPL))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
    "DCMPG" >> {
      val c = exec(new InsnNode(Opcodes.DCMPG))
      c.stack.size must_== ORIG_STACK_SIZE - 4 + 1
      c.stack.head must_== KnownType(classOf[Int])
    }
  }
}
