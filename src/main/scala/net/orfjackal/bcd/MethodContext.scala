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
      case _ => this
    }
  }

  private def execute(insn: IntInsnNode) = {
    insn.getOpcode match {
      case _ => this
    }
  }

  private def execute(insn: VarInsnNode) = {
    val idx = insn.`var`
    insn.getOpcode match {
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
      case Opcodes.RET => throw new IllegalArgumentException("V1_5 bytecode is not supported")
    }
  }

  private def load(idx: Int, typ: Class[_]) = {
    val value = locals.get(idx) match {
      case Some(v: KnownValue) => v
      case Some(v: KnownRef) => v
      case _ => KnownType(typ)
    }
    new MethodContext(value :: stack, locals.update(idx, value))
  }

  private def store(idx: Int, typ: Class[_]) = {
    val value = stack.head match {
      case v: KnownValue => v
      case v: KnownRef => v
      case _ => KnownType(typ)
    }
    new MethodContext(stack.tail, locals.update(idx, value))
  }

  // We have decided to store 64-bit data as follows:
  // - stack words: lower bytes at top
  // - local variables: lower bytes at the lower index
  //
  // "Implementors are free to decide the appropriate way to divide a 64-bit data value between two local variables."
  // [3.6.1, p. 67 in JVMS]
  //
  // "Implementors are free to decide the appropriate way to divide a 64-bit data value between two operand stack words."
  // [3.6.2, p. 67 in JVMS]

  private def load2(idx: Int, typ: Class[_]) = load(idx + 1, typ).load(idx, typ)

  private def store2(idx: Int, typ: Class[_]) = store(idx, typ).store(idx + 1, typ)


  private def execute(insn: TypeInsnNode) = {
    insn.getOpcode match {
      case _ => this
    }
  }

  private def execute(insn: FieldInsnNode) = {
    insn.getOpcode match {
      case _ => this
    }
  }

  private def execute(insn: MethodInsnNode) = {
    insn.getOpcode match {
      case _ => this
    }
  }

  private def execute(insn: JumpInsnNode) = {
    insn.getOpcode match {
      case Opcodes.JSR => throw new IllegalArgumentException("V1_5 bytecode is not supported")
    }
  }

  private def execute(insn: LabelNode) = {
    insn.getOpcode match {
      case _ => this
    }
  }

  private def execute(insn: LdcInsnNode) = {
    insn.getOpcode match {
      case _ => this
    }
  }

  private def execute(insn: IincInsnNode) = {
    insn.getOpcode match {
      case _ => this
    }
  }

  private def execute(insn: TableSwitchInsnNode) = {
    insn.getOpcode match {
      case _ => this
    }
  }

  private def execute(insn: LookupSwitchInsnNode) = {
    insn.getOpcode match {
      case _ => this
    }
  }

  private def execute(insn: MultiANewArrayInsnNode) = {
    insn.getOpcode match {
      case _ => this
    }
  }

  private def execute(insn: FrameNode) = {
    insn.getOpcode match {
      case _ => this
    }
  }

  private def execute(insn: LineNumberNode) = {
    insn.getOpcode match {
      case _ => this
    }
  }
}
