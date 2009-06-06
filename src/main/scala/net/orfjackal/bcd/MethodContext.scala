// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._

class MethodContext(
        val stack: List[Value]
        ) {
  def this() = this (Nil)

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
    insn.getOpcode match {
      case Opcodes.ILOAD => push(KnownType(classOf[Int]))
      case Opcodes.LLOAD => push(KnownType(classOf[Long])).push(KnownType(classOf[Long]))
      case Opcodes.FLOAD => push(KnownType(classOf[Float]))
      case Opcodes.DLOAD => push(KnownType(classOf[Double])).push(KnownType(classOf[Double]))
      case Opcodes.ALOAD => push(KnownType(classOf[Object]))
      case Opcodes.ISTORE => pop()
      case Opcodes.LSTORE => pop().pop()
      case Opcodes.FSTORE => pop()
      case Opcodes.DSTORE => pop().pop()
      case Opcodes.ASTORE => pop()
      case Opcodes.RET => throw new IllegalArgumentException("V1_5 bytecode is not supported")
    }
  }

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

  private def push(v: Value) = {
    new MethodContext(v :: stack)
  }

  private def pop() = {
    new MethodContext(stack.tail)
  }
}
