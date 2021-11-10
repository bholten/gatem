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

import scala.annotation.tailrec
import scala.util.{ Failure, Success, Try }

object Gate {

  /**
    * Type alias for a classification function of type `T => Boolean`.
    *
    * @tparam T The type of the input into the [[Gate]]
    */
  type Classifier[T] = T => Boolean

  /**
    * Type corresponding to the branch of an ordinary binary tree.
    *
    * Holds the classification function, and two other [[Gate]]s: the left one is taken when the
    * classification function evaluates to `true`; the right one is taken when the classification
    * function evaluates to `false`.
    *
    * @param c the classification function
    * @param l the left gate
    * @param r the right gate
    * @tparam C the data type used as a classification
    * @tparam T the input data type
    */
  final case class Branch[C, T](c: Classifier[T], l: Gate[C, T], r: Gate[C, T]) extends Gate[C, T] {
    override def toString: String = s"Branch(left: $l, right: $r)"
  }

  /**
    * Type corresponding to the leaf in an ordinary binary tree.
    *
    * Holds a single value of type C.
    *
    * @param v the value of the [[Terminus]]
    * @tparam C the type of the terminal value
    * @tparam T the type of the classification values
    */
  final case class Terminus[C, T](v: C) extends Gate[C, T]

  /**
    * Builder wrapper for creating [[Branch]].
    *
    * @tparam C the classification type
    * @tparam T the type of the data being classified
    */
  class BranchBuilder[C, T] {
    private var classifier = Option.empty[Classifier[T]]
    private var left       = Option.empty[Gate[C, T]]
    private var right      = Option.empty[Gate[C, T]]

    /**
      * The [[Classifier]] to add to the [[Branch]].
      *
      * @param c the [[Classifier]]
      * @return the modified [[BranchBuilder]]
      */
    def withClassifier(c: Classifier[T]): BranchBuilder[C, T] = {
      classifier = Some(c)
      this
    }

    /**
      * The `true` bath for the [[Branch]].
      *
      * @param left the [[Gate]] to when the [[Classifier]] returns `true`
      * @return the modified [[BranchBuilder]]
      */
    def whenTrue(left: Gate[C, T]): BranchBuilder[C, T] = {
      this.left = Some(left)
      this
    }

    /**
      * The `false` bath for the [[Branch]].
      *
      * @param right the [[Gate]] to when the [[Classifier]] returns `false`
      * @return the modified [[BranchBuilder]]
      */
    def whenFalse(right: Gate[C, T]): BranchBuilder[C, T] = {
      this.right = Some(right)
      this
    }

    /**
      * Final build for the [[Branch]].
      *
      * @throws java.lang.IllegalStateException when either the left or right sub-[[Gate]]s are invalid
      * @return the completed [[Branch]]
      */
    @throws(classOf[IllegalStateException])
    def build: Branch[C, T] = {
      val branchOption = for {
        c <- classifier
        r <- right
        l <- left
      } yield Branch(c, l, r)

      Try(branchOption.get) match {
        case Failure(_: NoSuchElementException) =>
          throw new IllegalStateException("Branch must contain classifier, left, and right")
        case Failure(e) =>
          throw new IllegalStateException(s"Branch could not be created due to exception: $e")
        case Success(value) => value
      }
    }
  }

  object BranchBuilder {

    /**
      * Convenience constructor for creating a [[BranchBuilder]].
      *
      * @tparam C the classification type
      * @tparam T the data type being classified
      * @return the [[BranchBuilder]]
      */
    def newBuilder[C, T]: BranchBuilder[C, T] = new BranchBuilder[C, T]
  }
}

/**
  * A [[Gate]] is a binary-tree that descends its nodes based on some internal [[com.github.bholten.data.gatem.Gate.Classifier]].
  *
  * The [[com.github.bholten.data.gatem.Gate.Classifier]] is a function, `T => Boolean`. If given some input of type `T`, the
  * [[Gate]] will decend with the following rules:
  *
  *   - If the [[com.github.bholten.data.gatem.Gate.Classifier]] evaluates to `true` with the input, take the left branch.
  *   - If the [[com.github.bholten.data.gatem.Gate.Classifier]] evaluates to `false` with the input, take the right branch.
  *   - If the `branch` is a [[com.github.bholten.data.gatem.Gate.Terminus]], then return the value wrapped by the [[com.github.bholten.data.gatem.Gate.Terminus]].
  *
  * @tparam C the [[com.github.bholten.data.gatem.Gate.Classifier]] type
  * @tparam T the type of the data being classified
  */
sealed trait Gate[+C, -T] {
  import Gate._

  /**
    * @return the value of the [[Terminus]], otherwise `None`
    */
  def value: Option[C] =
    this match {
      case _: Branch[C, T]   => None
      case t: Terminus[C, T] => Some(t.v)
    }

  /**
    * @return The left [[Gate]]
    */
  def left: Gate[C, T] =
    this match {
      case n: Branch[C, T]   => n.l
      case t: Terminus[C, T] => t
    }

  /**
    * @return The right [[Gate]]
    */
  def right: Gate[C, T] =
    this match {
      case n: Branch[C, T]   => n.r
      case t: Terminus[C, T] => t
    }

  /**
    * Runs the given input through the [[Gate]].
    *
    * @param input the input data
    * @return the result of the input run through the [[Gate]]
    */
  def runInput(input: T): C = {
    @tailrec
    def rec(gate: Gate[C, T], t: T): C =
      gate match {
        case Terminus(v)     => v
        case Branch(c, l, r) => if (c.apply(t)) rec(l, t) else rec(r, t)
      }
    rec(this, input)
  }

  /**
    * Runs the given input through the [[Gate]], but returns the result and the path it took through the [[Gate]].
    *
    * @param input the input data
    * @return the result of the input run through the [[Gate]], as well as the path it took
    */
  def scanInput(input: T): (C, List[(Gate[C, T], Boolean)]) = {
    @tailrec
    def rec(
        gate: Gate[C, T],
        t: T,
        acc: List[(Gate[C, T], Boolean)]
    ): (C, List[(Gate[C, T], Boolean)]) =
      gate match {
        case Terminus(v) => (v, acc)
        case b @ Branch(c, l, r) =>
          if (c.apply(t))
            rec(l, t, (b, true) :: acc)
          else
            rec(r, t, (b, false) :: acc)
      }
    rec(this, input, List())
  }
}
