/*
* This file is part of the diffson project.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package diffson

import cats._
import cats.data.Chain

package object jsonpointer {

  type Part = Either[String, Int]
  type Pointer = Chain[Part]

  object Pointer {

    val Root: Pointer = Chain.empty

    private val IsDigit = "(0|[1-9][0-9]*)".r

    def apply(elems: String*): Pointer = Chain.fromSeq(elems.map {
      case IsDigit(idx) => Right(idx.toInt)
      case key          => Left(key)
    })

    def unapply(p: Pointer): Option[(Part, Pointer)] =
      p.uncons

  }

  object Leaf {

    def unapply(p: Pointer): Option[Part] =
      p.uncons.flatMap {
        case (a, Chain.nil) => Some(a)
        case _              => None
      }

  }

  implicit class PointerOps(val p: Pointer) extends AnyVal {

    def /(s: String): Pointer =
      p.append(Left(s))

    def /(i: Int): Pointer =
      p.append(Right(i))

  }

}
