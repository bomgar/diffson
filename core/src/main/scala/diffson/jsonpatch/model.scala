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
package jsonpatch

import jsonpointer._

import cats._
import cats.implicits._

import scala.language.higherKinds

case class JsonPatch[Json](ops: List[Operation[Json]])

sealed trait Operation[Json] {
  val path: Pointer
}

case class Add[Json](path: Pointer, value: Json) extends Operation[Json]

case class Remove[Json](path: Pointer) extends Operation[Json]

case class Replace[Json](path: Pointer, value: Json) extends Operation[Json]

case class Move[Json](from: Pointer, path: Pointer) extends Operation[Json]

case class Copy[Json](from: Pointer, path: Pointer) extends Operation[Json]

case class Test[Json](path: Pointer, value: Json) extends Operation[Json]

object JsonPatch {

  implicit def JsonPatchPatch[F[_], Json: Jsony](implicit F: MonadError[F, Throwable]): Patch[F, Json, JsonPatch[Json]] =
    new Patch[F, Json, JsonPatch[Json]] {
      def apply(json: Json, patch: JsonPatch[Json]): F[Json] =
        patch.ops.foldM(json)(applyOp)

      private def applyOp(json: Json, op: Operation[Json]): F[Json] =
        op match {
          case Add(pointer, v) => applyAdd(json, pointer, v)
        }

      private def applyAdd(original: Json, path: Pointer, v: Json): F[Json] =
        (path, original) match {
          case (Pointer.Root, _) => F.pure(v)
          case (Leaf(Left(n)), JsObject(fields)) =>
            F.pure(JsObject(fields.updated(n, v)))
          case (Pointer(Left(n), rest), JsObject(fields)) =>
            def f(child: Option[Json]): F[Option[Json]] =
              child match {
                case None        => cannot("insert", "object", n, path)
                case Some(child) => applyAdd(child, rest, v).map(Some(_))
              }
            modify(n, f(_), fields).map(JsObject(_))
          case (Leaf(Right(i)), JsArray(array)) =>
            if (i < 0 || i >= array.size)
              cannot("insert", "array", i, path)
            else
              F.pure(JsArray(array.updated(i, v)))
          case (Pointer(Right(i), rest), JsArray(array)) =>
            def f(child: Option[Json]): F[Option[Json]] =
              child match {
                case None        => cannot("traverse", "array", i, path)
                case Some(child) => applyAdd(child, rest, v).map(Some(_))
              }
            modify(i, f(_), array).map(JsArray(_))
          case (Pointer(Left("-"), rest), JsArray(array)) =>
            F.pure(JsArray(array :+ v))
          case (_, _) =>
            pointerException(path, original)
        }

      private def modify(s: String, f: Option[Json] => F[Option[Json]], map: Map[String, Json]) =
        f(map.get(s)).map {
          case None    => map - s
          case Some(v) => map.updated(s, v)
        }

      private def modify(i: Int, f: Option[Json] => F[Option[Json]], array: Vector[Json]) = {
        val a = array.lift(i)
        (F.pure(a), f(a)).mapN {
          case (None, None) =>
            array
          case (Some(_), None) =>
            val (before, after) = array.splitAt(i)
            before ++ after.tail
          case (None, Some(a)) =>
            val (before, after) = array.splitAt(i)
            before ++ (a +: after)
          case (Some(_), Some(a)) =>
            array.updated(i, a)
        }
      }

      private def cannot[Ret, T: Show](op: String, tpe: String, name: T, pointer: Pointer): F[Ret] =
        F.raiseError(PatchException(show"Cannot $op missing $tpe member at $name in $pointer."))

      private def pointerException(path: Pointer, json: Json): F[Json] =
        path match {
          case Pointer.Root =>
            F.raiseError(new PatchException("Empty pointer"))
          case Pointer(h, _) =>
            val tpe = h match {
              case Left(_)  => "object"
              case Right(_) => "array"
            }
            F.raiseError(new PatchException(show"Cannot follow $path. Expected $tpe but got $json."))
        }

    }

}
