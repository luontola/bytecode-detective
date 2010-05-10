// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.specs._
object interpretConstantSpec extends Specification {
  "Loading constants" >> {
    def exec(insn: AbstractInsnNode) = {
      val c = new MethodContext()
      c.execute(insn)
    }
    def stackAfter(insn: AbstractInsnNode) = {
      val c = exec(insn)
      c.locals must_== Map.empty
      c.stack
    }
    "ACONST_NULL" >> {
      stackAfter(new InsnNode(Opcodes.ACONST_NULL)) must_== List(KnownRef(null, classOf[Object]))
    }
    "ICONST_M1" >> {
      stackAfter(new InsnNode(Opcodes.ICONST_M1)) must_== List(KnownValue(-1, classOf[Int]))
    }
    "ICONST_0" >> {
      stackAfter(new InsnNode(Opcodes.ICONST_0)) must_== List(KnownValue(0, classOf[Int]))
    }
    "ICONST_1" >> {
      stackAfter(new InsnNode(Opcodes.ICONST_1)) must_== List(KnownValue(1, classOf[Int]))
    }
    "ICONST_2" >> {
      stackAfter(new InsnNode(Opcodes.ICONST_2)) must_== List(KnownValue(2, classOf[Int]))
    }
    "ICONST_3" >> {
      stackAfter(new InsnNode(Opcodes.ICONST_3)) must_== List(KnownValue(3, classOf[Int]))
    }
    "ICONST_4" >> {
      stackAfter(new InsnNode(Opcodes.ICONST_4)) must_== List(KnownValue(4, classOf[Int]))
    }
    "ICONST_5" >> {
      stackAfter(new InsnNode(Opcodes.ICONST_5)) must_== List(KnownValue(5, classOf[Int]))
    }
    "LCONST_0" >> {
      stackAfter(new InsnNode(Opcodes.LCONST_0)) must_== List(KnownValue(0L, classOf[Long]), KnownValue(0L, classOf[Long]))
    }
    "LCONST_1" >> {
      stackAfter(new InsnNode(Opcodes.LCONST_1)) must_== List(KnownValue(1L, classOf[Long]), KnownValue(1L, classOf[Long]))
    }
    "FCONST_0" >> {
      stackAfter(new InsnNode(Opcodes.FCONST_0)) must_== List(KnownValue(0.0F, classOf[Float]))
    }
    "FCONST_1" >> {
      stackAfter(new InsnNode(Opcodes.FCONST_1)) must_== List(KnownValue(1.0F, classOf[Float]))
    }
    "FCONST_2" >> {
      stackAfter(new InsnNode(Opcodes.FCONST_2)) must_== List(KnownValue(2.0F, classOf[Float]))
    }
    "DCONST_0" >> {
      stackAfter(new InsnNode(Opcodes.DCONST_0)) must_== List(KnownValue(0.0, classOf[Double]), KnownValue(0.0, classOf[Double]))
    }
    "DCONST_1" >> {
      stackAfter(new InsnNode(Opcodes.DCONST_1)) must_== List(KnownValue(1.0, classOf[Double]), KnownValue(1.0, classOf[Double]))
    }
    "BIPUSH" >> {
      stackAfter(new IntInsnNode(Opcodes.BIPUSH, 10)) must_== List(KnownValue(10.asInstanceOf[Byte], classOf[Byte]))
    }
    "SIPUSH" >> {
      stackAfter(new IntInsnNode(Opcodes.SIPUSH, 20)) must_== List(KnownValue(20.asInstanceOf[Short], classOf[Short]))
    }
    "LDC int" >> {
      val v = java.lang.Integer.MAX_VALUE
      stackAfter(new LdcInsnNode(v)) must_== List(KnownValue(v, classOf[Int]))
    }
    "LDC float" >> {
      val v = java.lang.Float.MAX_VALUE
      stackAfter(new LdcInsnNode(v)) must_== List(KnownValue(v, classOf[Float]))
    }
    "LDC long" >> {
      val v = java.lang.Long.MAX_VALUE
      stackAfter(new LdcInsnNode(v)) must_== List(KnownValue(v, classOf[Long]), KnownValue(v, classOf[Long]))
    }
    "LDC double" >> {
      val v = java.lang.Double.MAX_VALUE
      stackAfter(new LdcInsnNode(v)) must_== List(KnownValue(v, classOf[Double]), KnownValue(v, classOf[Double]))
    }
    "LDC String" >> {
      val v = "str"
      stackAfter(new LdcInsnNode(v)) must_== List(KnownRef(v, classOf[String]))
    }
    "LDC Type" >> {
      val v = Type.getType("Ljava/lang/String;")
      stackAfter(new LdcInsnNode(v)) must_== List(KnownRef(classOf[String], classOf[Class[String]]))
    }
  }
}
