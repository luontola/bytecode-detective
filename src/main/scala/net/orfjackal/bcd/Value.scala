// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

sealed abstract class Value
case class UnknownValue() extends Value
case class KnownType(typ: Class[_]) extends Value
case class KnownValue[T <: AnyVal](value: T, _typ: Class[T]) extends KnownType(_typ)
case class KnownRef[T <: AnyRef](ref: T, _typ: Class[T]) extends KnownType(_typ)
