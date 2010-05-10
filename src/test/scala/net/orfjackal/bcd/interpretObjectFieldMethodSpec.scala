// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.specs._
object interpretObjectFieldMethodSpec extends Specification {
  "Operating objects" >> {
    val ORIG_STACK_SIZE = 1
    def exec(insn: AbstractInsnNode) = {
      val stack = List(UnknownValue())
      val c = new MethodContext(stack, Map.empty)
      c.stack.size must_== ORIG_STACK_SIZE
      c.execute(insn)
    }

    "NEW" >> {
      val c = exec(new TypeInsnNode(Opcodes.NEW, "java/lang/String"))
      c.stack.size must_== ORIG_STACK_SIZE + 1
      c.stack.head must_== KnownType(classOf[String])
    }
    "INSTANCEOF" >> {
      val c = exec(new TypeInsnNode(Opcodes.INSTANCEOF, "java/lang/String"))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[Boolean])
    }
    "MONITORENTER" >> {
      val c = exec(new InsnNode(Opcodes.MONITORENTER))
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "MONITOREXIT" >> {
      val c = exec(new InsnNode(Opcodes.MONITOREXIT))
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
  }

  "Getting values from fields" >> {
    val ORIG_STACK_SIZE = 1
    def exec(insn: AbstractInsnNode) = {
      val stack = List(UnknownValue())
      val c = new MethodContext(stack, Map.empty)
      c.stack.size must_== ORIG_STACK_SIZE
      c.execute(insn)
    }
    val ownerType = Type.getType(classOf[String])
    val fieldName = "someField"

    "GETFIELD object" >> {
      val fieldType = Type.getType(classOf[String])
      val c = exec(new FieldInsnNode(Opcodes.GETFIELD, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[String])
    }
    "GETFIELD array" >> {
      val fieldType = Type.getType(classOf[Array[String]])
      val c = exec(new FieldInsnNode(Opcodes.GETFIELD, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[Array[String]])
    }
    "GETFIELD int" >> {
      val fieldType = Type.INT_TYPE
      val c = exec(new FieldInsnNode(Opcodes.GETFIELD, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[Int])
    }
    "GETFIELD long" >> {
      val fieldType = Type.LONG_TYPE
      val c = exec(new FieldInsnNode(Opcodes.GETFIELD, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE + 1
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
    "GETFIELD double" >> {
      val fieldType = Type.DOUBLE_TYPE
      val c = exec(new FieldInsnNode(Opcodes.GETFIELD, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE + 1
      c.stack.take(2) must_== List(KnownType(classOf[Double]), KnownType(classOf[Double]))
    }

    "GETSTATIC" >> {
      val fieldType = Type.getType(classOf[String])
      val c = exec(new FieldInsnNode(Opcodes.GETSTATIC, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE + 1
      c.stack.head must_== KnownType(classOf[String])
    }
    "GETSTATIC long/double" >> {
      val fieldType = Type.LONG_TYPE
      val c = exec(new FieldInsnNode(Opcodes.GETSTATIC, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE + 2
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }
  }

  "Putting values to fields" >> {
    val ORIG_STACK_SIZE = 3
    def exec(insn: AbstractInsnNode) = {
      val stack = List(UnknownValue(), UnknownValue(), UnknownValue())
      val c = new MethodContext(stack, Map.empty)
      c.stack.size must_== ORIG_STACK_SIZE
      c.execute(insn)
    }
    val ownerType = Type.getType(classOf[String])
    val fieldName = "someField"

    "PUTFIELD" >> {
      val fieldType = Type.getType(classOf[String])
      val c = exec(new FieldInsnNode(Opcodes.PUTFIELD, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
    "PUTFIELD long/double" >> {
      val fieldType = Type.getType(classOf[Long])
      val c = exec(new FieldInsnNode(Opcodes.PUTFIELD, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE - 3
    }

    "PUTSTATIC" >> {
      val fieldType = Type.getType(classOf[String])
      val c = exec(new FieldInsnNode(Opcodes.PUTSTATIC, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "PUTSTATIC long/double" >> {
      val fieldType = Type.getType(classOf[Long])
      val c = exec(new FieldInsnNode(Opcodes.PUTSTATIC, ownerType.getInternalName, fieldName, fieldType.getDescriptor))
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
  }

  "Invoking methods" >> {
    val ORIG_STACK_SIZE = 3
    def exec(insn: AbstractInsnNode) = {
      val stack = List(UnknownValue(), UnknownValue(), UnknownValue())
      val c = new MethodContext(stack, Map.empty)
      c.stack.size must_== ORIG_STACK_SIZE
      c.execute(insn)
    }
    val ownerType = Type.getType(classOf[String])
    val methodName = "someMethod"

    "INVOKEVIRTUAL (0 args, return void)" >> {
      val c = exec(new MethodInsnNode(Opcodes.INVOKEVIRTUAL, ownerType.getInternalName, methodName, "()V"))
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "INVOKEVIRTUAL (1 int arg, return void)" >> {
      val c = exec(new MethodInsnNode(Opcodes.INVOKEVIRTUAL, ownerType.getInternalName, methodName, "(I)V"))
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
    "INVOKEVIRTUAL (1 long arg, return void)" >> {
      val c = exec(new MethodInsnNode(Opcodes.INVOKEVIRTUAL, ownerType.getInternalName, methodName, "(J)V"))
      c.stack.size must_== ORIG_STACK_SIZE - 3
    }
    "INVOKEVIRTUAL (2 int args, return void)" >> {
      val c = exec(new MethodInsnNode(Opcodes.INVOKEVIRTUAL, ownerType.getInternalName, methodName, "(II)V"))
      c.stack.size must_== ORIG_STACK_SIZE - 3
    }
    "INVOKEVIRTUAL (0 args, return int)" >> {
      val c = exec(new MethodInsnNode(Opcodes.INVOKEVIRTUAL, ownerType.getInternalName, methodName, "()I"))
      c.stack.size must_== ORIG_STACK_SIZE
      c.stack.head must_== KnownType(classOf[Int])
    }
    "INVOKEVIRTUAL (0 args, return long)" >> {
      val c = exec(new MethodInsnNode(Opcodes.INVOKEVIRTUAL, ownerType.getInternalName, methodName, "()J"))
      c.stack.size must_== ORIG_STACK_SIZE + 1
      c.stack.take(2) must_== List(KnownType(classOf[Long]), KnownType(classOf[Long]))
    }

    "INVOKESPECIAL (1 args, return void)" >> {
      val c = exec(new MethodInsnNode(Opcodes.INVOKESPECIAL, ownerType.getInternalName, methodName, "(I)V"))
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
    "INVOKESTATIC (1 args, return void)" >> {
      val c = exec(new MethodInsnNode(Opcodes.INVOKESTATIC, ownerType.getInternalName, methodName, "(I)V"))
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "INVOKEINTERFACE (1 args, return void)" >> {
      val c = exec(new MethodInsnNode(Opcodes.INVOKEINTERFACE, ownerType.getInternalName, methodName, "(I)V"))
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
  }
}
