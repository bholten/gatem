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
import munit.ScalaCheckSuite
import org.scalacheck.Prop

class GateSpec extends ScalaCheckSuite {
  val exampleGate: Gate[Classification, Int] =
    new BranchBuilder[Classification, Int]
      .withClassifier(_ % 2 == 0)
      .whenTrue(
        new BranchBuilder[Classification, Int]
          .withClassifier(_ > 100)
          .whenTrue(Terminus(EvenGreaterThan100))
          .whenFalse(Terminus(EvenLessThanOrEqualTo100))
          .build
      )
      .whenFalse(
        new BranchBuilder[Classification, Int]
          .withClassifier(_ < 100)
          .whenTrue(Terminus(OddLessThan100))
          .whenFalse(Terminus(OddGreaterThanOrEqualTo100))
          .build
      )
      .build

  sealed trait Classification
  final case object EvenGreaterThan100         extends Classification
  final case object EvenLessThanOrEqualTo100   extends Classification
  final case object OddLessThan100             extends Classification
  final case object OddGreaterThanOrEqualTo100 extends Classification

  property("All numbers should match the spec") {
    Prop.forAll { n: Int =>
      val manualTest =
        if (n % 2 == 0)
          if (n > 100)
            EvenGreaterThan100
          else
            EvenLessThanOrEqualTo100
        else if (n < 100)
          OddLessThan100
        else
          OddGreaterThanOrEqualTo100

      exampleGate.runInput(n) == manualTest
    }
  }

  property("Run Input and Scan Input should always match") {
    Prop.forAll { n: Int =>
      val manualTest =
        if (n % 2 == 0)
          if (n > 100)
            EvenGreaterThan100
          else
            EvenLessThanOrEqualTo100
        else if (n < 100)
          OddLessThan100
        else
          OddGreaterThanOrEqualTo100

      exampleGate.scanInput(n)._1 == manualTest &&
      exampleGate.runInput(n) == manualTest
    }
  }
}
