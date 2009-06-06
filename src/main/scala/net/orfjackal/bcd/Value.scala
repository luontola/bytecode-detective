// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

abstract class Value
case class UnknownValue() extends Value
case class KnownType(typ: Class[_]) extends Value
case class KnownValue(value: AnyVal, t: Class[_ <: AnyVal]) extends KnownType(t)
case class KnownRef(ref: AnyRef, t: Class[_ <: AnyRef]) extends KnownType(t)
