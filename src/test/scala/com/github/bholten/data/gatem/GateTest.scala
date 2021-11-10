/*
 * Copyright 2021 Brennan Holten
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.bholten.data.gatem

import com.github.bholten.data.gatem.Gate._
import munit.FunSuite

class GateTest extends FunSuite {
  val gate: Gate.Branch[Aliquot, Int] =
    BranchBuilder
      .newBuilder[Aliquot, Int]
      .withClassifier(deficient)
      .whenTrue(Terminus(Deficient))
      .whenFalse(
        BranchBuilder
          .newBuilder[Aliquot, Int]
          .withClassifier(abundant)
          .whenTrue(Terminus(Abundant))
          .whenFalse(Terminus(Perfect))
          .build
      )
      .build
  def deficient(n: Int): Boolean = aliquot(n) < n
  def abundant(n: Int): Boolean  = aliquot(n) > n
  def perfect(n: Int): Boolean   = aliquot(n) == n
  def aliquot(n: Int): Int       = (1 until n).filter(n % _ == 0).sum
  sealed trait Aliquot
  final case object Deficient   extends Aliquot
  final case object Abundant    extends Aliquot
  final case object Perfect     extends Aliquot
  final case object NonPositive extends Aliquot

  test("6 is perfect") {
    assertEquals(gate.scanInput(6)._1, Perfect)
    assertEquals(gate.runInput(6), Perfect)
  }

  test("5 is deficient") {
    assertEquals(gate.scanInput(5)._1, Deficient)
    assertEquals(gate.runInput(5), Deficient)
  }

  test("12 is abundant") {
    assertEquals(gate.scanInput(12)._1, Abundant)
    assertEquals(gate.runInput(12), Abundant)
  }

  test("show the full classification tree with scan") {
    assertEquals(
      gate
        .scanInput(12)
        ._2
        .toString,
      "List((Branch(left: Terminus(Abundant), right: Terminus(Perfect)),true), (Branch(left: Terminus(Deficient), right: Branch(left: Terminus(Abundant), right: Terminus(Perfect))),false))"
    )
  }
}
