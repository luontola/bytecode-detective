// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.specs._
import org.specs.runner._

object interpretArraySpec extends Specification {
  def exec(stack: List[Value], insn: AbstractInsnNode) = {
    val c = new MethodContext(stack, Map.empty)
    c.execute(insn)
  }

  "Creating and operating arrays" should {
    val arrayLengthParam = List(KnownValue(10, classOf[Int]))
    "NEWARRAY boolean" in {
      val c = exec(arrayLengthParam, new IntInsnNode(Opcodes.NEWARRAY, Opcodes.T_BOOLEAN))
      c.stack must_== List(KnownType(classOf[Array[Boolean]]))
    }
    "NEWARRAY char" in {
      val c = exec(arrayLengthParam, new IntInsnNode(Opcodes.NEWARRAY, Opcodes.T_CHAR))
      c.stack must_== List(KnownType(classOf[Array[Char]]))
    }
    "NEWARRAY float" in {
      val c = exec(arrayLengthParam, new IntInsnNode(Opcodes.NEWARRAY, Opcodes.T_FLOAT))
      c.stack must_== List(KnownType(classOf[Array[Float]]))
    }
    "NEWARRAY double" in {
      val c = exec(arrayLengthParam, new IntInsnNode(Opcodes.NEWARRAY, Opcodes.T_DOUBLE))
      c.stack must_== List(KnownType(classOf[Array[Double]]))
    }
    "NEWARRAY byte" in {
      val c = exec(arrayLengthParam, new IntInsnNode(Opcodes.NEWARRAY, Opcodes.T_BYTE))
      c.stack must_== List(KnownType(classOf[Array[Byte]]))
    }
    "NEWARRAY short" in {
      val c = exec(arrayLengthParam, new IntInsnNode(Opcodes.NEWARRAY, Opcodes.T_SHORT))
      c.stack must_== List(KnownType(classOf[Array[Short]]))
    }
    "NEWARRAY int" in {
      val c = exec(arrayLengthParam, new IntInsnNode(Opcodes.NEWARRAY, Opcodes.T_INT))
      c.stack must_== List(KnownType(classOf[Array[Int]]))
    }
    "NEWARRAY long" in {
      val c = exec(arrayLengthParam, new IntInsnNode(Opcodes.NEWARRAY, Opcodes.T_LONG))
      c.stack must_== List(KnownType(classOf[Array[Long]]))
    }
    "ANEWARRAY" in {
      val c = exec(arrayLengthParam, new TypeInsnNode(Opcodes.ANEWARRAY, "java/lang/String"))
      c.stack must_== List(KnownType(classOf[Array[String]]))
    }
    "MULTIANEWARRAY" in {
      val params = List(KnownValue(10, classOf[Int]), KnownValue(20, classOf[Int]))
      val c = exec(params, new MultiANewArrayInsnNode("java/lang/String", 2))
      c.stack must_== List(KnownType(classOf[Array[Array[String]]]))
    }
    "ARRAYLENGTH" in {
      val params = List(KnownType(classOf[Array[String]]))
      val c = exec(params, new InsnNode(Opcodes.ARRAYLENGTH))
      c.stack must_== List(KnownType(classOf[Int]))
    }
  }

  "Loading from arrays" should {
    "BALOAD" in {
      val params = List(KnownType(classOf[Int]), KnownType(classOf[Array[Byte]]))
      val c = exec(params, new InsnNode(Opcodes.BALOAD))
      c.stack must_== List(KnownType(classOf[Byte]))
    }
    "CALOAD" in {
      val params = List(KnownType(classOf[Int]), KnownType(classOf[Array[Char]]))
      val c = exec(params, new InsnNode(Opcodes.CALOAD))
      c.stack must_== List(KnownType(classOf[Char]))
    }
    "SALOAD" in {
      val params = List(KnownType(classOf[Int]), KnownType(classOf[Array[Short]]))
      val c = exec(params, new InsnNode(Opcodes.SALOAD))
      c.stack must_== List(KnownType(classOf[Short]))
    }
    "IALOAD" in {
      val params = List(KnownType(classOf[Int]), KnownType(classOf[Array[Int]]))
      val c = exec(params, new InsnNode(Opcodes.IALOAD))
      c.stack must_== List(KnownType(classOf[Int]))
    }
    "LALOAD" in {
      val params = List(KnownType(classOf[Int]), KnownType(classOf[Array[Long]]))
      val c = exec(params, new InsnNode(Opcodes.LALOAD))
      c.stack must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "FALOAD" in {
      val params = List(KnownType(classOf[Int]), KnownType(classOf[Array[Float]]))
      val c = exec(params, new InsnNode(Opcodes.FALOAD))
      c.stack must_== List(KnownType(classOf[Float]))
    }
    "DALOAD" in {
      val params = List(KnownType(classOf[Int]), KnownType(classOf[Array[Double]]))
      val c = exec(params, new InsnNode(Opcodes.DALOAD))
      c.stack must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }
    "AALOAD unknown array type" in {
      val params = List(KnownType(classOf[Int]), UnknownValue())
      val c = exec(params, new InsnNode(Opcodes.AALOAD))
      c.stack must_== List(KnownType(classOf[Object]))
    }
    "AALOAD known array type" in {
      val params = List(KnownType(classOf[Int]), KnownType(classOf[Array[String]]))
      val c = exec(params, new InsnNode(Opcodes.AALOAD))
      c.stack must_== List(KnownType(classOf[String]))
    }
    "AALOAD known array value" in {
      val params = List(KnownType(classOf[Int]), KnownRef(Array[String]("x"), classOf[Array[String]]))
      val c = exec(params, new InsnNode(Opcodes.AALOAD))
      c.stack must_== List(KnownType(classOf[String]))
    }
  }

  "Storing to arrays" should {
    "BASTORE" in {
      val params = List(KnownType(classOf[Byte]), KnownType(classOf[Int]), KnownType(classOf[Array[Byte]]))
      val c = exec(params, new InsnNode(Opcodes.BASTORE))
      c.stack must_== Nil
    }
    "CASTORE" in {
      val params = List(KnownType(classOf[Char]), KnownType(classOf[Int]), KnownType(classOf[Array[Char]]))
      val c = exec(params, new InsnNode(Opcodes.CASTORE))
      c.stack must_== Nil
    }
    "SASTORE" in {
      val params = List(KnownType(classOf[Short]), KnownType(classOf[Int]), KnownType(classOf[Array[Short]]))
      val c = exec(params, new InsnNode(Opcodes.SASTORE))
      c.stack must_== Nil
    }
    "IASTORE" in {
      val params = List(KnownType(classOf[Int]), KnownType(classOf[Int]), KnownType(classOf[Array[Int]]))
      val c = exec(params, new InsnNode(Opcodes.IASTORE))
      c.stack must_== Nil
    }
    "LASTORE" in {
      val params = List(KnownType(classOf[Long]), KnownType(classOf[Long]), KnownType(classOf[Int]), KnownType(classOf[Array[Long]]))
      val c = exec(params, new InsnNode(Opcodes.LASTORE))
      c.stack must_== Nil
    }
    "FASTORE" in {
      val params = List(KnownType(classOf[Float]), KnownType(classOf[Int]), KnownType(classOf[Array[Float]]))
      val c = exec(params, new InsnNode(Opcodes.FASTORE))
      c.stack must_== Nil
    }
    "DASTORE" in {
      val params = List(KnownType(classOf[Double]), KnownType(classOf[Double]), KnownType(classOf[Int]), KnownType(classOf[Array[Double]]))
      val c = exec(params, new InsnNode(Opcodes.DASTORE))
      c.stack must_== Nil
    }
    "AASTORE" in {
      val params = List(KnownType(classOf[String]), KnownType(classOf[Int]), KnownType(classOf[Array[String]]))
      val c = exec(params, new InsnNode(Opcodes.AASTORE))
      c.stack must_== Nil
    }
  }
}
