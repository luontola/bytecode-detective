// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

sealed abstract class Value
case class UnknownValue() extends Value
case class KnownType(t: Class[_]) extends Value
case class KnownValue[T <: AnyVal](value: T, typ: Class[T]) extends KnownType(typ)
case class KnownRef[T <: AnyRef](ref: T, typ: Class[T]) extends KnownType(typ)
