// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.specs._
import org.specs.runner._

object interpretObjectFieldMethodSpec extends Specification {
  "Operating objects" should {
    val ORIG_STACK_SIZE = 1
    def exec(insn: AbstractInsnNode) = {
      val stack = List(UnknownValue())
      val c = new MethodContext(stack, Map.empty)
      c.stack.size must_== ORIG_STACK_SIZE
      c.execute(insn)
    }

    "NEW" in {
      val c = exec(new TypeInsnNode(Opcodes.NEW, "java/lang/String"))
      c.stack.size must_== ORIG_STACK_SIZE + 1
      c.stack.head must_== KnownType(classOf[String])
    }
    "INSTANCEOF" in {
      val c = exec(new TypeInsnNode(Opcodes.INSTANCEOF, "java/lang/String"))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[Boolean])
    }
    "MONITORENTER" in {
      val c = exec(new InsnNode(Opcodes.MONITORENTER))
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "MONITOREXIT" in {
      val c = exec(new InsnNode(Opcodes.MONITOREXIT))
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
  }

  "Getting values from fields" should {
    val ORIG_STACK_SIZE = 1
    def exec(insn: AbstractInsnNode) = {
      val stack = List(UnknownValue())
      val c = new MethodContext(stack, Map.empty)
      c.stack.size must_== ORIG_STACK_SIZE
      c.execute(insn)
    }
    val ownerType = Type.getType(classOf[String])
    val fieldName = "someField"

    "GETFIELD object" in {
      val fieldType = Type.getType(classOf[String])
      val c = exec(new FieldInsnNode(Opcodes.GETFIELD, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[String])
    }
    "GETFIELD array" in {
      val fieldType = Type.getType(classOf[Array[String]])
      val c = exec(new FieldInsnNode(Opcodes.GETFIELD, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[Array[String]])
    }
    "GETFIELD int" in {
      val fieldType = Type.INT_TYPE
      val c = exec(new FieldInsnNode(Opcodes.GETFIELD, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[Int])
    }
    "GETFIELD long" in {
      val fieldType = Type.LONG_TYPE
      val c = exec(new FieldInsnNode(Opcodes.GETFIELD, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE + 1
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "GETFIELD double" in {
      val fieldType = Type.DOUBLE_TYPE
      val c = exec(new FieldInsnNode(Opcodes.GETFIELD, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE + 1
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }

    "GETSTATIC" in {
      val fieldType = Type.getType(classOf[String])
      val c = exec(new FieldInsnNode(Opcodes.GETSTATIC, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE + 1
      c.stack.head must_== KnownType(classOf[String])
    }
    "GETSTATIC long/double" in {
      val fieldType = Type.LONG_TYPE
      val c = exec(new FieldInsnNode(Opcodes.GETSTATIC, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
  }

  "Putting values to fields" should {
    val ORIG_STACK_SIZE = 3
    def exec(insn: AbstractInsnNode) = {
      val stack = List(UnknownValue(), UnknownValue(), UnknownValue())
      val c = new MethodContext(stack, Map.empty)
      c.stack.size must_== ORIG_STACK_SIZE
      c.execute(insn)
    }
    val ownerType = Type.getType(classOf[String])
    val fieldName = "someField"

    "PUTFIELD" in {
      val fieldType = Type.getType(classOf[String])
      val c = exec(new FieldInsnNode(Opcodes.PUTFIELD, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
    "PUTFIELD long/double" in {
      val fieldType = Type.getType(classOf[Long])
      val c = exec(new FieldInsnNode(Opcodes.PUTFIELD, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE - 3
    }

    "PUTSTATIC" in {
      val fieldType = Type.getType(classOf[String])
      val c = exec(new FieldInsnNode(Opcodes.PUTSTATIC, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "PUTSTATIC long/double" in {
      val fieldType = Type.getType(classOf[Long])
      val c = exec(new FieldInsnNode(Opcodes.PUTSTATIC, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
  }

  "Invoking methods" should {
    val ORIG_STACK_SIZE = 3
    def exec(insn: AbstractInsnNode) = {
      val stack = List(UnknownValue(), UnknownValue(), UnknownValue())
      val c = new MethodContext(stack, Map.empty)
      c.stack.size must_== ORIG_STACK_SIZE
      c.execute(insn)
    }
    val ownerType = Type.getType(classOf[String])
    val methodName = "someMethod"

    "INVOKEVIRTUAL (0 args, return void)" in {
      val c = exec(new MethodInsnNode(Opcodes.INVOKEVIRTUAL, ownerType.getInternalName, methodName, "()V"))
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "INVOKEVIRTUAL (1 int arg, return void)" in {
      val c = exec(new MethodInsnNode(Opcodes.INVOKEVIRTUAL, ownerType.getInternalName, methodName, "(I)V"))
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
    "INVOKEVIRTUAL (1 long arg, return void)" in {
      val c = exec(new MethodInsnNode(Opcodes.INVOKEVIRTUAL, ownerType.getInternalName, methodName, "(J)V"))
      c.stack.size must_== ORIG_STACK_SIZE - 3
    }
    "INVOKEVIRTUAL (2 int args, return void)" in {
      val c = exec(new MethodInsnNode(Opcodes.INVOKEVIRTUAL, ownerType.getInternalName, methodName, "(II)V"))
      c.stack.size must_== ORIG_STACK_SIZE - 3
    }
    "INVOKEVIRTUAL (0 args, return int)" in {
      val c = exec(new MethodInsnNode(Opcodes.INVOKEVIRTUAL, ownerType.getInternalName, methodName, "()I"))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[Int])
    }
    "INVOKEVIRTUAL (0 args, return long)" in {
      val c = exec(new MethodInsnNode(Opcodes.INVOKEVIRTUAL, ownerType.getInternalName, methodName, "()J"))
      c.stack.size must_== ORIG_STACK_SIZE + 1
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }

    "INVOKESPECIAL (1 args, return void)" in {
      val c = exec(new MethodInsnNode(Opcodes.INVOKESPECIAL, ownerType.getInternalName, methodName, "(I)V"))
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
    "INVOKESTATIC (1 args, return void)" in {
      val c = exec(new MethodInsnNode(Opcodes.INVOKESTATIC, ownerType.getInternalName, methodName, "(I)V"))
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "INVOKEINTERFACE (1 args, return void)" in {
      val c = exec(new MethodInsnNode(Opcodes.INVOKEINTERFACE, ownerType.getInternalName, methodName, "(I)V"))
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
  }
}
