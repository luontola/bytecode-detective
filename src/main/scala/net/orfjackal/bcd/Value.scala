// Copyright © 2009, Esko Luontola. All Rights Reserved.
// This software is released under the MIT License. See LICENSE.txt

package net.orfjackal.bcd

abstract class Value
case class UnknownValue() extends Value
case class KnownType(t: Class[_]) extends Value
case class KnownValue(v: Object, tt: Class[_]) extends KnownType(tt)
