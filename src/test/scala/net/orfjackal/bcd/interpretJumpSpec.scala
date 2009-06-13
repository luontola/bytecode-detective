// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.specs._
import org.specs.runner._

object interpretJumpSpec extends Specification {
  "When an instruction is executed" should {
    def exec(insn: AbstractInsnNode) = {
      val c = new MethodContext(List(UnknownValue()), Map.empty)
      c.execute(insn)
    }

    val label = new LabelNode()
    val n1 = new InsnNode(Opcodes.ICONST_1)
    val jump = new JumpInsnNode(Opcodes.IFEQ, label)
    val n2 = new InsnNode(Opcodes.ICONST_2)
    val r1 = new InsnNode(Opcodes.RETURN)
    val r2 = new InsnNode(Opcodes.RETURN)

    val list = new InsnList()
    list.add(n1)
    list.add(jump)
    list.add(n2)
    list.add(r1)
    list.add(label)
    list.add(r2)

    "a normal instruction is followed by the next instruction" in {
      exec(n1).nextInstructions must_== Set(jump)
      exec(n2).nextInstructions must_== Set(r1)
      exec(label).nextInstructions must_== Set(r2)
    }
    "a jump instruction is followed by one of many instructions" in {
      exec(jump).nextInstructions must_== Set(n2, label)
    }
    "the method's end is not followed by anything" in {
      exec(r1).nextInstructions must_== Set() // return in the middle
      exec(r2).nextInstructions must_== Set() // return in the end
    }
  }

  "Jumping" should {
    val n1 = new InsnNode(Opcodes.ICONST_1)
    val label = new LabelNode()
    val n2 = new InsnNode(Opcodes.ICONST_2)
    val sw1 = new LabelNode()
    val sw2 = new LabelNode()
    val sw3 = new LabelNode()

    val ORIG_STACK_SIZE = 2
    def exec(insn: AbstractInsnNode) = {
      val list = new InsnList()
      list.add(insn)
      list.add(n1)
      list.add(label)
      list.add(n2)
      list.add(sw1)
      list.add(sw2)
      list.add(sw3)
      //      list.add(table)
      //      list.add(lookup)

      val stack = List(UnknownValue(), UnknownValue())
      val c = new MethodContext(stack, Map.empty)
      c.stack.size must_== ORIG_STACK_SIZE
      c.execute(insn)
    }

    "IFEQ" in {
      val c = exec(new JumpInsnNode(Opcodes.IFEQ, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "IFNE" in {
      val c = exec(new JumpInsnNode(Opcodes.IFNE, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "IFLT" in {
      val c = exec(new JumpInsnNode(Opcodes.IFLT, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "IFGE" in {
      val c = exec(new JumpInsnNode(Opcodes.IFGE, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "IFGT" in {
      val c = exec(new JumpInsnNode(Opcodes.IFGT, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "IFLE" in {
      val c = exec(new JumpInsnNode(Opcodes.IFLE, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "IF_ICMPEQ" in {
      val c = exec(new JumpInsnNode(Opcodes.IF_ICMPEQ, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
    "IF_ICMPNE" in {
      val c = exec(new JumpInsnNode(Opcodes.IF_ICMPNE, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
    "IF_ICMPLT" in {
      val c = exec(new JumpInsnNode(Opcodes.IF_ICMPLT, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
    "IF_ICMPGE" in {
      val c = exec(new JumpInsnNode(Opcodes.IF_ICMPGE, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
    "IF_ICMPGT" in {
      val c = exec(new JumpInsnNode(Opcodes.IF_ICMPGT, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
    "IF_ICMPLE" in {
      val c = exec(new JumpInsnNode(Opcodes.IF_ICMPLE, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
    "IF_ACMPEQ" in {
      val c = exec(new JumpInsnNode(Opcodes.IF_ACMPEQ, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
    "IF_ACMPNE" in {
      val c = exec(new JumpInsnNode(Opcodes.IF_ACMPNE, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 2
    }
    "GOTO" in {
      val c = exec(new JumpInsnNode(Opcodes.GOTO, label))
      c.nextInstructions must_== Set(label)
      c.stack.size must_== ORIG_STACK_SIZE
    }
    "IFNULL" in {
      val c = exec(new JumpInsnNode(Opcodes.IFNULL, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "IFNONNULL" in {
      val c = exec(new JumpInsnNode(Opcodes.IFNONNULL, label))
      c.nextInstructions must_== Set(n1, label)
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "TABLESWITCH" in {
      val c = exec(new TableSwitchInsnNode(10, 12, sw1, Array(sw2, sw3)))
      c.nextInstructions must_== Set(sw1, sw2, sw3)
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
    "LOOKUPSWITCH" in {
      val c = exec(new LookupSwitchInsnNode(sw1, Array(10, 20), Array(sw2, sw3)))
      c.nextInstructions must_== Set(sw1, sw2, sw3)
      c.stack.size must_== ORIG_STACK_SIZE - 1
    }
  }
}
