// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm._
import org.objectweb.asm.tree._

class MethodContext(
        val stack: List[Value],
        val locals: Map[Int, Value],
        val nextInstructions: Set[AbstractInsnNode]
        ) {
  def this(stack: List[Value], locals: Map[Int, Value]) = this (stack, locals, Set.empty)

  def this() = this (Nil, Map.empty, Set.empty)

  def execute(insn: AbstractInsnNode): MethodContext = updateValues(insn).updateNextInstructions(insn)

  // TODO: handle TryCatchBlockNode? unless it is handled, the code of catch blocks will not be visited.
  private def updateValues(insn: AbstractInsnNode): MethodContext = {
    insn.getType match {
      case AbstractInsnNode.INSN => updateValues(insn.asInstanceOf[InsnNode])
      case AbstractInsnNode.INT_INSN => updateValues(insn.asInstanceOf[IntInsnNode])
      case AbstractInsnNode.VAR_INSN => updateValues(insn.asInstanceOf[VarInsnNode])
      case AbstractInsnNode.TYPE_INSN => updateValues(insn.asInstanceOf[TypeInsnNode])
      case AbstractInsnNode.FIELD_INSN => updateValues(insn.asInstanceOf[FieldInsnNode])
      case AbstractInsnNode.METHOD_INSN => updateValues(insn.asInstanceOf[MethodInsnNode])
      case AbstractInsnNode.JUMP_INSN => updateValues(insn.asInstanceOf[JumpInsnNode])
      case AbstractInsnNode.LABEL => updateValues(insn.asInstanceOf[LabelNode])
      case AbstractInsnNode.LDC_INSN => updateValues(insn.asInstanceOf[LdcInsnNode])
      case AbstractInsnNode.IINC_INSN => updateValues(insn.asInstanceOf[IincInsnNode])
      case AbstractInsnNode.TABLESWITCH_INSN => updateValues(insn.asInstanceOf[TableSwitchInsnNode])
      case AbstractInsnNode.LOOKUPSWITCH_INSN => updateValues(insn.asInstanceOf[LookupSwitchInsnNode])
      case AbstractInsnNode.MULTIANEWARRAY_INSN => updateValues(insn.asInstanceOf[MultiANewArrayInsnNode])
      case AbstractInsnNode.FRAME => updateValues(insn.asInstanceOf[FrameNode])
      case AbstractInsnNode.LINE => updateValues(insn.asInstanceOf[LineNumberNode])
    }
  }

  private def updateNextInstructions(insn: AbstractInsnNode): MethodContext = {
    insn.getType match {
      case AbstractInsnNode.INSN => updateNextInstructions(insn.asInstanceOf[InsnNode])
      case AbstractInsnNode.JUMP_INSN => updateNextInstructions(insn.asInstanceOf[JumpInsnNode])
      case AbstractInsnNode.TABLESWITCH_INSN => updateNextInstructions(insn.asInstanceOf[TableSwitchInsnNode])
      case AbstractInsnNode.LOOKUPSWITCH_INSN => updateNextInstructions(insn.asInstanceOf[LookupSwitchInsnNode])
      case _ => defaultNextInstruction(insn)
    }
  }

  private def defaultNextInstruction(insn: AbstractInsnNode) = {
    this withNext insn.getNext
  }

  private def withNext(next: AbstractInsnNode) = {
    if (next == null)
      this
    else
      new MethodContext(stack, locals, Set(next))
  }

  private def withNext(next: List[AbstractInsnNode]) = {
    val nextNonNull = Set.empty ++ next.filter((n) => n != null)
    new MethodContext(stack, locals, nextNonNull)
  }


  private def updateValues(insn: InsnNode) = {
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
      // Arrays
      case Opcodes.IALOAD => aload(classOf[Int])
      case Opcodes.LALOAD => aload2(classOf[Long])
      case Opcodes.FALOAD => aload(classOf[Float])
      case Opcodes.DALOAD => aload2(classOf[Double])
      case Opcodes.AALOAD => aaload()
      case Opcodes.BALOAD => aload(classOf[Byte])
      case Opcodes.CALOAD => aload(classOf[Char])
      case Opcodes.SALOAD => aload(classOf[Short])
      case Opcodes.IASTORE => astore()
      case Opcodes.LASTORE => astore2()
      case Opcodes.FASTORE => astore()
      case Opcodes.DASTORE => astore2()
      case Opcodes.AASTORE => astore()
      case Opcodes.BASTORE => astore()
      case Opcodes.CASTORE => astore()
      case Opcodes.SASTORE => astore()
      // Stack
      case Opcodes.POP => pop()
      case Opcodes.POP2 => pop2()
      case Opcodes.DUP => dup()
      case Opcodes.DUP_X1 => dup_x(1)
      case Opcodes.DUP_X2 => dup_x(2)
      case Opcodes.DUP2 => dup2()
      case Opcodes.DUP2_X1 => dup2_x(1)
      case Opcodes.DUP2_X2 => dup2_x(2)
      case Opcodes.SWAP => swap()
      // Arithmetic
      case Opcodes.IADD => icalc()
      case Opcodes.LADD => lcalc()
      case Opcodes.FADD => fcalc()
      case Opcodes.DADD => dcalc()
      case Opcodes.ISUB => icalc()
      case Opcodes.LSUB => lcalc()
      case Opcodes.FSUB => fcalc()
      case Opcodes.DSUB => dcalc()
      case Opcodes.IMUL => icalc()
      case Opcodes.LMUL => lcalc()
      case Opcodes.FMUL => fcalc()
      case Opcodes.DMUL => dcalc()
      case Opcodes.IDIV => icalc()
      case Opcodes.LDIV => lcalc()
      case Opcodes.FDIV => fcalc()
      case Opcodes.DDIV => dcalc()
      case Opcodes.IREM => icalc()
      case Opcodes.LREM => lcalc()
      case Opcodes.FREM => fcalc()
      case Opcodes.DREM => dcalc()
      case Opcodes.INEG => ineg()
      case Opcodes.LNEG => lneg()
      case Opcodes.FNEG => fneg()
      case Opcodes.DNEG => dneg()
      case Opcodes.ISHL => icalc()
      case Opcodes.LSHL => lcalc()
      case Opcodes.ISHR => icalc()
      case Opcodes.LSHR => lcalc()
      case Opcodes.IUSHR => icalc()
      case Opcodes.LUSHR => lcalc()
      case Opcodes.IAND => icalc()
      case Opcodes.LAND => lcalc()
      case Opcodes.IOR => icalc()
      case Opcodes.LOR => lcalc()
      case Opcodes.IXOR => icalc()
      case Opcodes.LXOR => lcalc()
      // Casts
      case Opcodes.I2L => pop().push2(KnownType(classOf[Long]))
      case Opcodes.I2F => pop().push(KnownType(classOf[Float]))
      case Opcodes.I2D => pop().push2(KnownType(classOf[Double]))
      case Opcodes.L2I => pop2().push(KnownType(classOf[Int]))
      case Opcodes.L2F => pop2().push(KnownType(classOf[Float]))
      case Opcodes.L2D => pop2().push2(KnownType(classOf[Double]))
      case Opcodes.F2I => pop().push(KnownType(classOf[Int]))
      case Opcodes.F2L => pop().push2(KnownType(classOf[Long]))
      case Opcodes.F2D => pop().push2(KnownType(classOf[Double]))
      case Opcodes.D2I => pop2().push(KnownType(classOf[Int]))
      case Opcodes.D2L => pop2().push2(KnownType(classOf[Long]))
      case Opcodes.D2F => pop2().push(KnownType(classOf[Float]))
      case Opcodes.I2B => pop().push(KnownType(classOf[Byte]))
      case Opcodes.I2C => pop().push(KnownType(classOf[Char]))
      case Opcodes.I2S => pop().push(KnownType(classOf[Short]))
      // Logic
      case Opcodes.LCMP => lcmp()
      case Opcodes.FCMPL => fcmp()
      case Opcodes.FCMPG => fcmp()
      case Opcodes.DCMPL => dcmp()
      case Opcodes.DCMPG => dcmp()
      // Return
      case Opcodes.IRETURN => pop()
      case Opcodes.LRETURN => pop2()
      case Opcodes.FRETURN => pop()
      case Opcodes.DRETURN => pop2()
      case Opcodes.ARETURN => pop()
      case Opcodes.RETURN => this
      // Arrays
      case Opcodes.ARRAYLENGTH => pop().push(KnownType(classOf[Int]))
      // Return
      case Opcodes.ATHROW => pop()
      // Objects
      case Opcodes.MONITORENTER => pop()
      case Opcodes.MONITOREXIT => pop()
    }
  }

  private def updateNextInstructions(insn: InsnNode) = {
    val endOfMethod = insn.getOpcode match {
      case Opcodes.IRETURN => true
      case Opcodes.LRETURN => true
      case Opcodes.FRETURN => true
      case Opcodes.DRETURN => true
      case Opcodes.ARETURN => true
      case Opcodes.RETURN => true
      case Opcodes.ATHROW => true
      case _ => false
    }
    if (endOfMethod) {
      this withNext Nil
    } else {
      defaultNextInstruction(insn)
    }
  }

  private def updateValues(insn: IntInsnNode) = {
    insn.getOpcode match {
    // Constants
      case Opcodes.BIPUSH => const(insn.operand.asInstanceOf[Byte], classOf[Byte])
      case Opcodes.SIPUSH => const(insn.operand.asInstanceOf[Short], classOf[Short])
      // Arrays
      case Opcodes.NEWARRAY => newarray(typecode2class(insn.operand))
    }
  }

  private def updateValues(insn: VarInsnNode) = {
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

  private def updateValues(insn: TypeInsnNode) = {
    val typ = desc2class(insn.desc)
    insn.getOpcode match {
    // Objects
      case Opcodes.NEW => push(KnownType(typ))
      // Arrays
      case Opcodes.ANEWARRAY => anewarray(typ)
      // Casts
      case Opcodes.CHECKCAST => checkcast(typ)
      // Objects
      case Opcodes.INSTANCEOF => pop().push(KnownType(classOf[Boolean]))
    }
  }

  private def updateValues(insn: FieldInsnNode) = {
    val fieldType = Type.getType(insn.desc)
    insn.getOpcode match {
    // Fields
      case Opcodes.GETSTATIC =>
        if (isWide(fieldType))
          push2(KnownType(fieldType))
        else
          push(KnownType(fieldType))

      case Opcodes.PUTSTATIC =>
        if (isWide(fieldType))
          pop2()
        else
          pop()

      case Opcodes.GETFIELD =>
        if (isWide(fieldType))
          pop().push2(KnownType(fieldType))
        else
          pop().push(KnownType(fieldType))

      case Opcodes.PUTFIELD =>
        if (isWide(fieldType))
          pop2().pop()
        else
          pop().pop()
    }
  }

  private def updateValues(insn: MethodInsnNode) = {
    val args = Type.getArgumentTypes(insn.desc)
    val ret = Type.getReturnType(insn.desc)
    insn.getOpcode match {
    // Methods
      case Opcodes.INVOKEVIRTUAL => invoke_virtual(args, ret)
      case Opcodes.INVOKESPECIAL => invoke_virtual(args, ret)
      case Opcodes.INVOKESTATIC => invoke_static(args, ret)
      case Opcodes.INVOKEINTERFACE => invoke_virtual(args, ret)
    }
  }

  private def updateValues(insn: JumpInsnNode) = {
    insn.getOpcode match {
    // Jumps
      case Opcodes.IFEQ => pop()
      case Opcodes.IFNE => pop()
      case Opcodes.IFLT => pop()
      case Opcodes.IFGE => pop()
      case Opcodes.IFGT => pop()
      case Opcodes.IFLE => pop()
      case Opcodes.IF_ICMPEQ => pop2()
      case Opcodes.IF_ICMPNE => pop2()
      case Opcodes.IF_ICMPLT => pop2()
      case Opcodes.IF_ICMPGE => pop2()
      case Opcodes.IF_ICMPGT => pop2()
      case Opcodes.IF_ICMPLE => pop2()
      case Opcodes.IF_ACMPEQ => pop2()
      case Opcodes.IF_ACMPNE => pop2()
      case Opcodes.GOTO => this
      // Subroutines
      case Opcodes.JSR => throw new IllegalArgumentException("V1_5 bytecode is not supported")
      // Jumps
      case Opcodes.IFNULL => pop()
      case Opcodes.IFNONNULL => pop()
    }
  }

  private def updateNextInstructions(insn: JumpInsnNode) = {
    if (insn.getOpcode == Opcodes.GOTO) {
      this withNext insn.label
    } else {
      this withNext List(insn.getNext, insn.label)
    }
  }

  private def updateValues(insn: LabelNode) = {
    // TODO
    this
  }

  private def updateValues(insn: LdcInsnNode) = {
    assert(insn.getOpcode == Opcodes.LDC)
    insn.cst match {
    // Constants (constant pool)
      case cst: java.lang.Integer => const(cst.intValue, classOf[Int])
      case cst: java.lang.Float => const(cst.floatValue, classOf[Float])
      case cst: java.lang.Long => const2(cst.longValue, classOf[Long])
      case cst: java.lang.Double => const2(cst.doubleValue, classOf[Double])
      case cst: java.lang.String => aconst(cst, classOf[String])
      case cst: org.objectweb.asm.Type => aconst(type2class(cst), classOf[Class[_]])
    }
  }

  private def updateValues(insn: IincInsnNode) = {
    assert(insn.getOpcode == Opcodes.IINC)
    val idx = insn.`var`
    val value = KnownType(classOf[Int])
    new MethodContext(stack, locals.updated(idx, value))
  }

  private def updateValues(insn: TableSwitchInsnNode) = {
    assert(insn.getOpcode == Opcodes.TABLESWITCH)
    pop()
  }

  private def updateNextInstructions(insn: TableSwitchInsnNode) = {
    this withNext (List(insn.dflt) ++ insn.labels.toArray(Array[LabelNode]()))
  }

  private def updateValues(insn: LookupSwitchInsnNode) = {
    assert(insn.getOpcode == Opcodes.LOOKUPSWITCH)
    pop()
  }

  private def updateNextInstructions(insn: LookupSwitchInsnNode) = {
    this withNext (List(insn.dflt) ++ insn.labels.toArray(Array[LabelNode]()))
  }

  private def updateValues(insn: MultiANewArrayInsnNode) = {
    assert(insn.getOpcode == Opcodes.MULTIANEWARRAY)
    multianewarray(desc2class(insn.desc), insn.dims)
  }

  private def updateValues(insn: FrameNode) = {
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

  private def updateValues(insn: LineNumberNode) = {
    // TODO
    this
  }


  // Local variables

  private def load(idx: Int, typ: Class[_]) = {
    val value = locals.get(idx) match {
      case Some(v: KnownValue[_]) => v
      case Some(v: KnownRef[_]) => v
      case _ => KnownType(typ)
    }
    assert(typ isAssignableFrom value.getType.get, "expected '" + typ + "' at " + idx + " but found " + value)
    new MethodContext(value :: stack, locals.updated(idx, value))
  }

  private def store(idx: Int, typ: Class[_]) = {
    val value = stack.head match {
      case v: KnownValue[_] => v
      case v: KnownRef[_] => v
      case _ => KnownType(typ)
    }
    assert(typ isAssignableFrom value.getType.get, "expected '" + typ + "' at " + idx + " but found " + value)
    new MethodContext(stack.tail, locals.updated(idx, value))
  }

  // "Implementors are free to decide the appropriate way to divide a 64-bit data value between two local variables."
  // [3.6.1, p. 67 in JVMS]
  // "Implementors are free to decide the appropriate way to divide a 64-bit data value between two operand stack words."
  // [3.6.2, p. 67 in JVMS]
  private def load2(idx: Int, typ: Class[_]) = load(idx + 1, typ).load(idx, typ)

  private def store2(idx: Int, typ: Class[_]) = store(idx, typ).store(idx + 1, typ)

  // Stack

  private def push(value: Value) = new MethodContext(value :: stack, locals)

  private def push2(value: Value) = push(value).push(value)

  private def pop() = new MethodContext(stack.tail, locals)

  private def pop2() = new MethodContext(stack.drop(2), locals)

  private def dup() = new MethodContext(stack.head :: stack, locals)

  private def dup_x(n: Int) = new MethodContext(stack.take(n + 1) ::: stack.head :: stack.drop(n + 1), locals)

  private def dup2() = new MethodContext(stack.take(2) ::: stack, locals)

  private def dup2_x(n: Int) = new MethodContext(stack.take(n + 2) ::: stack.take(2) ::: stack.drop(n + 2), locals)

  private def swap() = new MethodContext(stack.tail.head :: stack.head :: stack.drop(2), locals)

  // Constants

  private def aconst[T <: AnyRef](value: T, typ: Class[T]) = push(new KnownRef(value, typ))

  private def const[T <: AnyVal](value: T, typ: Class[T]) = push(new KnownValue(value, typ))

  // 64-bit data is duplicated instead of split, so that processing the values would be easier
  private def const2[T <: AnyVal](value: T, typ: Class[T]) = const(value, typ).const(value, typ)

  // Arithmetics

  private def icalc() = pop().pop().push(KnownType(classOf[Int]))

  private def lcalc() = pop2().pop2().push2(KnownType(classOf[Long]))

  private def fcalc() = pop().pop().push(KnownType(classOf[Float]))

  private def dcalc() = pop2().pop2().push2(KnownType(classOf[Double]))

  private def ineg() = pop().push(KnownType(classOf[Int]))

  private def lneg() = pop2().push2(KnownType(classOf[Long]))

  private def fneg() = pop().push(KnownType(classOf[Float]))

  private def dneg() = pop2().push2(KnownType(classOf[Double]))

  // Logic

  private def lcmp() = pop2().pop2().push(KnownType(classOf[Int]))

  private def fcmp() = pop().pop().push(KnownType(classOf[Int]))

  private def dcmp() = pop2().pop2().push(KnownType(classOf[Int]))

  // Casts

  private def checkcast(cls: Class[_]) = {
    stack.head match {
      case UnknownValue() => pop().push(KnownType(cls))
      case _ => this
    }
  }

  // Methods

  private def invoke_virtual(args: Array[Type], ret: Type) = invoke(args, ret, false)

  private def invoke_static(args: Array[Type], ret: Type) = invoke(args, ret, true)

  private def invoke(argumentTypes: Array[Type], returnType: Type, static: Boolean) = {
    var c = this
    // objectref
    if (!static)
      c = c.pop()
    // argN, ..., arg2, arg1
    for (argType <- argumentTypes) {
      if (isWide(argType))
        c = c.pop2()
      else
        c = c.pop()
    }
    // result
    if (returnType == Type.VOID_TYPE)
      c
    else if (isWide(returnType))
      c.push2(KnownType(returnType))
    else
      c.push(KnownType(returnType))
  }

  // Arrays

  private def newarray(componentType: Class[_]) = {
    val array = java.lang.reflect.Array.newInstance(componentType, 0)
    pop().push(KnownType(array.getClass))
  }

  private def anewarray(componentType: Class[_]) = {
    val array = java.lang.reflect.Array.newInstance(componentType, 0)
    pop().push(KnownType(array.getClass))
  }

  private def multianewarray(componentType: Class[_], dimensions: Int) = {
    val array = java.lang.reflect.Array.newInstance(componentType, new Array[Int](dimensions): _*)
    var c = this
    for (i <- 0 until dimensions)
      c = c.pop()
    c.push(KnownType(array.getClass))
  }

  private def aload(typ: Class[_]) = pop().pop().push(KnownType(typ))

  private def aload2(typ: Class[_]) = pop().pop().push2(KnownType(typ))

  private def aaload() = {
    val arrayRef = stack.tail.head
    arrayRef match {
      case KnownType(arrayType) => aload(arrayType.getComponentType)
      case KnownRef(array, arrayType) => aload(arrayType.getComponentType)
      case _ => aload(classOf[Object])
    }
  }

  private def astore() = pop().pop().pop()

  private def astore2() = pop2().pop().pop()

  // ASM Utils

  private implicit def type2class(typ: Type): Class[_] = {
    typ.getSort match {
      case Type.VOID => classOf[Void]
      case Type.BOOLEAN => classOf[Boolean]
      case Type.CHAR => classOf[Char]
      case Type.BYTE => classOf[Byte]
      case Type.SHORT => classOf[Short]
      case Type.INT => classOf[Int]
      case Type.FLOAT => classOf[Float]
      case Type.LONG => classOf[Long]
      case Type.DOUBLE => classOf[Double]
      case Type.ARRAY => desc2class(typ.getInternalName)
      case Type.OBJECT => desc2class(typ.getInternalName)
    }
  }

  private def typecode2class(typecode: Int): Class[_] = {
    typecode match {
      case Opcodes.T_BOOLEAN => classOf[Boolean]
      case Opcodes.T_CHAR => classOf[Char]
      case Opcodes.T_FLOAT => classOf[Float]
      case Opcodes.T_DOUBLE => classOf[Double]
      case Opcodes.T_BYTE => classOf[Byte]
      case Opcodes.T_SHORT => classOf[Short]
      case Opcodes.T_INT => classOf[Int]
      case Opcodes.T_LONG => classOf[Long]
    }
  }

  private def desc2class(desc: String) = Class.forName(desc.replace('/', '.'))

  private def isWide(typ: Type): Boolean = (typ.getSort == Type.LONG || typ.getSort == Type.DOUBLE)

}
