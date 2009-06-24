// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

sealed abstract class Value {
  def getType: Option[Class[_]]
}
case class UnknownValue() extends Value {
  def getType = None
}
case class KnownType(typ: Class[_]) extends Value {
  def getType = Some(typ)
}
case class KnownValue[T <: AnyVal](value: T, typ: Class[T]) extends Value {
  def getType = Some(typ)
}
case class KnownRef[T <: AnyRef](ref: T, typ: Class[T]) extends Value {
  def getType = Some(typ)
}
