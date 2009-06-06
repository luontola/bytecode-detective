// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

abstract class Value
case class UnknownValue() extends Value
case class KnownType(typ: Class[_]) extends Value
case class KnownValue(value: AnyVal, override val typ: Class[_ <: AnyVal]) extends KnownType(typ)
case class KnownRef(ref: AnyRef, override val typ: Class[_ <: AnyRef]) extends KnownType(typ)
