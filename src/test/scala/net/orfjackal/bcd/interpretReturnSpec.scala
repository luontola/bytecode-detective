// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.specs._
import org.specs.runner._

object interpretReturnSpec extends Specification {
  def exec(stack: List[Value], insn: AbstractInsnNode) = {
    val list = new InsnList()
    list.add(insn)
    list.add(new InsnNode(Opcodes.NOP))
    val c = new MethodContext(stack, Map.empty)
    c.execute(insn)
  }

  "Returning" should {
    "IRETURN" in {
      val params = List(KnownType(classOf[Int]))
      val c = exec(params, new InsnNode(Opcodes.IRETURN))
      c.stack must_== Nil
      c.nextInstructions must_== Set.empty
    }
    "LRETURN" in {
      val params = List(KnownType(classOf[Long]), KnownType(classOf[Long]))
      val c = exec(params, new InsnNode(Opcodes.LRETURN))
      c.stack must_== Nil
      c.nextInstructions must_== Set.empty
    }
    "FRETURN" in {
      val params = List(KnownType(classOf[Float]))
      val c = exec(params, new InsnNode(Opcodes.FRETURN))
      c.stack must_== Nil
      c.nextInstructions must_== Set.empty
    }
    "DRETURN" in {
      val params = List(KnownType(classOf[Double]), KnownType(classOf[Double]))
      val c = exec(params, new InsnNode(Opcodes.DRETURN))
      c.stack must_== Nil
      c.nextInstructions must_== Set.empty
    }
    "ARETURN" in {
      val params = List(KnownType(classOf[String]))
      val c = exec(params, new InsnNode(Opcodes.ARETURN))
      c.stack must_== Nil
      c.nextInstructions must_== Set.empty
    }
    "RETURN" in {
      val params = Nil
      val c = exec(params, new InsnNode(Opcodes.RETURN))
      c.stack must_== Nil
      c.nextInstructions must_== Set.empty
    }
    "ATHROW" in {
      val params = List(KnownType(classOf[Throwable]))
      val c = exec(params, new InsnNode(Opcodes.ATHROW))
      c.stack must_== Nil
      c.nextInstructions must_== Set.empty
    }
  }
}
