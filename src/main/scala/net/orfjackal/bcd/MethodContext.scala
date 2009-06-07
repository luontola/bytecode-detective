// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._

class MethodContext(
        val stack: List[Value],
        val locals: Map[Int, Value]
        ) {
  def this() = this (Nil, Map())

  def execute(insn: AbstractInsnNode): MethodContext = {
    insn.getType match {
      case AbstractInsnNode.INSN => execute(insn.asInstanceOf[InsnNode])
      case AbstractInsnNode.INT_INSN => execute(insn.asInstanceOf[IntInsnNode])
      case AbstractInsnNode.VAR_INSN => execute(insn.asInstanceOf[VarInsnNode])
      case AbstractInsnNode.TYPE_INSN => execute(insn.asInstanceOf[TypeInsnNode])
      case AbstractInsnNode.FIELD_INSN => execute(insn.asInstanceOf[FieldInsnNode])
      case AbstractInsnNode.METHOD_INSN => execute(insn.asInstanceOf[MethodInsnNode])
      case AbstractInsnNode.JUMP_INSN => execute(insn.asInstanceOf[JumpInsnNode])
      case AbstractInsnNode.LABEL => execute(insn.asInstanceOf[LabelNode])
      case AbstractInsnNode.LDC_INSN => execute(insn.asInstanceOf[LdcInsnNode])
      case AbstractInsnNode.IINC_INSN => execute(insn.asInstanceOf[IincInsnNode])
      case AbstractInsnNode.TABLESWITCH_INSN => execute(insn.asInstanceOf[TableSwitchInsnNode])
      case AbstractInsnNode.LOOKUPSWITCH_INSN => execute(insn.asInstanceOf[LookupSwitchInsnNode])
      case AbstractInsnNode.MULTIANEWARRAY_INSN => execute(insn.asInstanceOf[MultiANewArrayInsnNode])
      case AbstractInsnNode.FRAME => execute(insn.asInstanceOf[FrameNode])
      case AbstractInsnNode.LINE => execute(insn.asInstanceOf[LineNumberNode])
    }
  }


  private def execute(insn: InsnNode) = {
    insn.getOpcode match {
    // TODO
      case Opcodes.NOP => this
      // Constants
      case Opcodes.ACONST_NULL => aconst(null, classOf[Object])
      case Opcodes.ICONST_M1 => const(-1, classOf[Int])
      case Opcodes.ICONST_0 => const(0, classOf[Int])
      case Opcodes.ICONST_1 => const(1, classOf[Int])
      case Opcodes.ICONST_2 => const(2, classOf[Int])
      case Opcodes.ICONST_3 => const(3, classOf[Int])
      case Opcodes.ICONST_4 => const(4, classOf[Int])
      case Opcodes.ICONST_5 => const(5, classOf[Int])
      case Opcodes.LCONST_0 => const2(0L, classOf[Long])
      case Opcodes.LCONST_1 => const2(1L, classOf[Long])
      case Opcodes.FCONST_0 => const(0.0F, classOf[Float])
      case Opcodes.FCONST_1 => const(1.0F, classOf[Float])
      case Opcodes.FCONST_2 => const(2.0F, classOf[Float])
      case Opcodes.DCONST_0 => const2(0.0, classOf[Double])
      case Opcodes.DCONST_1 => const2(1.0, classOf[Double])
      // TODO
      case Opcodes.IALOAD => this
      case Opcodes.LALOAD => this
      case Opcodes.FALOAD => this
      case Opcodes.DALOAD => this
      case Opcodes.AALOAD => this
      case Opcodes.BALOAD => this
      case Opcodes.CALOAD => this
      case Opcodes.SALOAD => this
      // TODO
      case Opcodes.IASTORE => this
      case Opcodes.LASTORE => this
      case Opcodes.FASTORE => this
      case Opcodes.DASTORE => this
      case Opcodes.AASTORE => this
      case Opcodes.BASTORE => this
      case Opcodes.CASTORE => this
      case Opcodes.SASTORE => this
      // Stack
      case Opcodes.POP => pop()
      case Opcodes.POP2 => pop().pop()
      case Opcodes.DUP => dup()
      case Opcodes.DUP_X1 => dup_x(1)
      case Opcodes.DUP_X2 => dup_x(2)
      case Opcodes.DUP2 => dup2()
      case Opcodes.DUP2_X1 => dup2_x(1)
      case Opcodes.DUP2_X2 => dup2_x(2)
      case Opcodes.SWAP => swap()
      // TODO
      case Opcodes.IADD => this
      case Opcodes.LADD => this
      case Opcodes.FADD => this
      case Opcodes.DADD => this
      case Opcodes.ISUB => this
      case Opcodes.LSUB => this
      case Opcodes.FSUB => this
      case Opcodes.DSUB => this
      case Opcodes.IMUL => this
      case Opcodes.LMUL => this
      case Opcodes.FMUL => this
      case Opcodes.DMUL => this
      case Opcodes.IDIV => this
      case Opcodes.LDIV => this
      case Opcodes.FDIV => this
      case Opcodes.DDIV => this
      case Opcodes.IREM => this
      case Opcodes.LREM => this
      case Opcodes.FREM => this
      case Opcodes.DREM => this
      case Opcodes.INEG => this
      case Opcodes.LNEG => this
      case Opcodes.FNEG => this
      case Opcodes.DNEG => this
      case Opcodes.ISHL => this
      case Opcodes.LSHL => this
      case Opcodes.ISHR => this
      case Opcodes.LSHR => this
      case Opcodes.IUSHR => this
      case Opcodes.LUSHR => this
      case Opcodes.IAND => this
      case Opcodes.LAND => this
      case Opcodes.IOR => this
      case Opcodes.LOR => this
      case Opcodes.IXOR => this
      case Opcodes.LXOR => this
      // TODO
      case Opcodes.I2L => this
      case Opcodes.I2F => this
      case Opcodes.I2D => this
      case Opcodes.L2I => this
      case Opcodes.L2F => this
      case Opcodes.L2D => this
      case Opcodes.F2I => this
      case Opcodes.F2L => this
      case Opcodes.F2D => this
      case Opcodes.D2I => this
      case Opcodes.D2L => this
      case Opcodes.D2F => this
      case Opcodes.I2B => this
      case Opcodes.I2C => this
      case Opcodes.I2S => this
      case Opcodes.LCMP => this
      case Opcodes.FCMPL => this
      case Opcodes.FCMPG => this
      case Opcodes.DCMPL => this
      case Opcodes.DCMPG => this
      // TODO
      case Opcodes.IRETURN => this
      case Opcodes.LRETURN => this
      case Opcodes.FRETURN => this
      case Opcodes.DRETURN => this
      case Opcodes.ARETURN => this
      case Opcodes.RETURN => this
      // TODO
      case Opcodes.ARRAYLENGTH => this
      case Opcodes.ATHROW => this
      // TODO
      case Opcodes.MONITORENTER => this
      case Opcodes.MONITOREXIT => this
    }
  }

  private def aconst[T <: AnyRef](value: T, typ: Class[T]) = push(new KnownRef(value, typ))

  private def const[T <: AnyVal](value: T, typ: Class[T]) = push(new KnownValue(value, typ))

  // 64-bit data is duplicated instead of split, so that processing the values would be easier
  private def const2[T <: AnyVal](value: T, typ: Class[T]) = const(value, typ).const(value, typ)

  private def push(value: Value) = new MethodContext(value :: stack, locals)

  private def pop() = new MethodContext(stack.tail, locals)

  private def dup() = new MethodContext(stack.head :: stack, locals)

  private def dup_x(n: Int) = new MethodContext(stack.take(n + 1) ::: stack.head :: stack.drop(n + 1), locals)

  private def dup2() = new MethodContext(stack.take(2) ::: stack, locals)

  private def dup2_x(n: Int) = new MethodContext(stack.take(n + 2) ::: stack.take(2) ::: stack.drop(n + 2), locals)

  private def swap() = new MethodContext(stack.tail.head :: stack.head :: stack.drop(2), locals)


  private def execute(insn: IntInsnNode) = {
    insn.getOpcode match {
    // Constants
      case Opcodes.BIPUSH => const(insn.operand.asInstanceOf[Byte], classOf[Byte])
      case Opcodes.SIPUSH => const(insn.operand.asInstanceOf[Short], classOf[Short])
      // TODO
      case Opcodes.NEWARRAY => this
    }
  }


  private def execute(insn: VarInsnNode) = {
    val idx = insn.`var`
    insn.getOpcode match {
    // Local variables
      case Opcodes.ILOAD => load(idx, classOf[Int])
      case Opcodes.LLOAD => load2(idx, classOf[Long])
      case Opcodes.FLOAD => load(idx, classOf[Float])
      case Opcodes.DLOAD => load2(idx, classOf[Double])
      case Opcodes.ALOAD => load(idx, classOf[Object])
      case Opcodes.ISTORE => store(idx, classOf[Int])
      case Opcodes.LSTORE => store2(idx, classOf[Long])
      case Opcodes.FSTORE => store(idx, classOf[Float])
      case Opcodes.DSTORE => store2(idx, classOf[Double])
      case Opcodes.ASTORE => store(idx, classOf[Object])
      // Subroutines
      case Opcodes.RET => throw new IllegalArgumentException("V1_5 bytecode is not supported")
    }
  }

  private def load(idx: Int, typ: Class[_]) = {
    val value = locals.get(idx) match {
      case Some(v: KnownValue[_]) => v
      case Some(v: KnownRef[_]) => v
      case _ => KnownType(typ)
    }
    new MethodContext(value :: stack, locals.update(idx, value))
  }

  private def store(idx: Int, typ: Class[_]) = {
    val value = stack.head match {
      case v: KnownValue[_] => v
      case v: KnownRef[_] => v
      case _ => KnownType(typ)
    }
    new MethodContext(stack.tail, locals.update(idx, value))
  }

  // "Implementors are free to decide the appropriate way to divide a 64-bit data value between two local variables."
  // [3.6.1, p. 67 in JVMS]
  // "Implementors are free to decide the appropriate way to divide a 64-bit data value between two operand stack words."
  // [3.6.2, p. 67 in JVMS]
  private def load2(idx: Int, typ: Class[_]) = load(idx + 1, typ).load(idx, typ)

  private def store2(idx: Int, typ: Class[_]) = store(idx, typ).store(idx + 1, typ)


  private def execute(insn: TypeInsnNode) = {
    insn.getOpcode match {
    // TODO
      case Opcodes.NEW => this
      case Opcodes.ANEWARRAY => this
      // TODO
      case Opcodes.CHECKCAST => this
      case Opcodes.INSTANCEOF => this
    }
  }


  private def execute(insn: FieldInsnNode) = {
    insn.getOpcode match {
    // TODO
      case Opcodes.GETSTATIC => this
      case Opcodes.PUTSTATIC => this
      case Opcodes.GETFIELD => this
      case Opcodes.PUTFIELD => this
    }
  }


  private def execute(insn: MethodInsnNode) = {
    insn.getOpcode match {
    // TODO
      case Opcodes.INVOKEVIRTUAL => this
      case Opcodes.INVOKESPECIAL => this
      case Opcodes.INVOKESTATIC => this
      case Opcodes.INVOKEINTERFACE => this
    }
  }


  private def execute(insn: JumpInsnNode) = {
    insn.getOpcode match {
    // TODO
      case Opcodes.IFEQ => this
      case Opcodes.IFNE => this
      case Opcodes.IFLT => this
      case Opcodes.IFGE => this
      case Opcodes.IFGT => this
      case Opcodes.IFLE => this
      case Opcodes.IF_ICMPEQ => this
      case Opcodes.IF_ICMPNE => this
      case Opcodes.IF_ICMPLT => this
      case Opcodes.IF_ICMPGE => this
      case Opcodes.IF_ICMPGT => this
      case Opcodes.IF_ICMPLE => this
      case Opcodes.IF_ACMPEQ => this
      case Opcodes.IF_ACMPNE => this
      case Opcodes.GOTO => this
      // Subroutines
      case Opcodes.JSR => throw new IllegalArgumentException("V1_5 bytecode is not supported")
      // TODO
      case Opcodes.IFNULL => this
      case Opcodes.IFNONNULL => this
    }
  }


  private def execute(insn: LabelNode) = {
    // TODO
    this
  }


  private def execute(insn: LdcInsnNode) = {
    assert(insn.getOpcode == Opcodes.LDC)
    insn.cst match {
    // Constants (constant pool)
      case cst: java.lang.Integer => const(cst.intValue, classOf[Int])
      case cst: java.lang.Float => const(cst.floatValue, classOf[Float])
      case cst: java.lang.Long => const2(cst.longValue, classOf[Long])
      case cst: java.lang.Double => const2(cst.doubleValue, classOf[Double])
      case cst: java.lang.String => aconst(cst, classOf[java.lang.String])
      case cst: org.objectweb.asm.Type => aconst(cst, classOf[org.objectweb.asm.Type])
    }
  }


  private def execute(insn: IincInsnNode) = {
    assert(insn.getOpcode == Opcodes.IINC)
    // TODO
    this
  }


  private def execute(insn: TableSwitchInsnNode) = {
    assert(insn.getOpcode == Opcodes.TABLESWITCH)
    // TODO
    this
  }


  private def execute(insn: LookupSwitchInsnNode) = {
    assert(insn.getOpcode == Opcodes.LOOKUPSWITCH)
    // TODO
    this
  }


  private def execute(insn: MultiANewArrayInsnNode) = {
    assert(insn.getOpcode == Opcodes.MULTIANEWARRAY)
    // TODO
    this
  }


  private def execute(insn: FrameNode) = {
    insn.`type` match {
    // TODO
      case Opcodes.F_NEW => this
      case Opcodes.F_FULL => this
      case Opcodes.F_APPEND => this
      case Opcodes.F_CHOP => this
      case Opcodes.F_SAME => this
      case Opcodes.F_SAME1 => this
    }
  }


  private def execute(insn: LineNumberNode) = {
    // TODO
    this
  }
}
