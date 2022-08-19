package me.viluon.lua.lang

import scala.collection.mutable.ArrayBuffer
import scala.language.{dynamics, higherKinds, implicitConversions}
import scala.lms.common.{Base, BooleanOps, IfThenElse, LiftString, Record, StringOps, StructOps, TupleOps}
import scala.reflect.SourceContext

trait LuaADTs extends Base { self: IfThenElse with BooleanOps with StructOps with TupleOps with StringOps with LuaError =>
  case class InvalidPatternException(msg: String) extends RuntimeException(msg)
  implicit def anyTyp: Typ[Any]

  trait StagedDynamic extends Dynamic {
    def selectDynamic[A](name: String): Rep[A]
  }

  /**
   * Beware, this helper case class hides mutable state.
   */
  case class SwitchOps[A](adt: ADT[A], self: Rep[ADT[A]])(implicit src: SourceContext)
    extends StagedDynamic {
    private val cases: ArrayBuffer[(Boolean, Rep[Any])] = collection.mutable.ArrayBuffer()

    // upcast needed here,
    // see https://stackoverflow.com/questions/17072185/understanding-the-limits-of-scala-gadt-support
    def selectDynamic[X](field: String): Rep[X] = (adt: ADT[_]) match {
      case ADTUnit() => throw InvalidPatternException("the unit type has no fields")
      case ADTString() => throw InvalidPatternException("the string type has no fields")
      case ADTInt() => throw InvalidPatternException("the int type has no fields")
      case ADTDouble() => throw InvalidPatternException("the double type has no fields")
      case prod: ADTProduct2[_, _] => selectFromProduct(prod, field)(prod.at, prod.bt)
      case ADTSum2(name, l, r) => throw InvalidPatternException(s"the sum type $name has no fields")
    }

    private def selectFromProduct[B: Typ, C: Typ, R](prod: ADTProduct2[B, C], field: String): Rep[R] = {
      val ADTProduct2(name, ADTField(lf, lt), ADTField(rf, rt)) = prod
      PatProduct2[B, C, R](
        Option(field) match {
          case `lf` => (fst, _) => fst.asInstanceOf[Rep[R]]
          case `rf` => (_, snd) => snd.asInstanceOf[Rep[R]]
          case _ =>
            throw InvalidPatternException(s"the product type $name has no field $field")
        }
      )(implicitly, implicitly)(self.asInstanceOf[Rep[ADT[(B, C)]]])
    }

    /*
     * Draft for pattern-matching syntax:
     * x.switch {
     *   // pattern constructor for product types
     *   Left("im", drop) ~~> {
     *     _.im // this <: Dynamic, forwards to x.selectDynamic("im")
     *   }
     * }
     */

    case class CaptureOps(private val captures: Map[String, Rep[Any]],
                          private val patSrc: SourceContext
                         ) extends StagedDynamic {
      def selectDynamic[X](field: String): Rep[X] = captures.get(field) match {
        case Some(x) => x.asInstanceOf[Rep[X]]
        case None => throw InvalidPatternException(s"field $field is not a part of the pattern at $patSrc")
      }
    }

    case class PatternOps(pat: ExperimentalPattern, captures: Seq[String], src: SourceContext) {
      def ~~>[R](f: CaptureOps => Rep[R]): Unit = {
        val value: Rep[_] = self.asInstanceOf[Sum2Rep[Any]].v
        val (flag, ctor) = pat match {
          case _: Left.type => false -> adt.asInstanceOf[ADTSum2[_, _]].l
          case _: Right.type => true -> adt.asInstanceOf[ADTSum2[_, _]].r
        }
        cases += flag -> f(
          CaptureOps(captures.zipWithIndex.map {
            case (name, i) if ctor.typ.isInstanceOf[ADTProduct2[_, _]] =>
              val product = ctor.typ.asInstanceOf[ADTProduct2[_, _]]
              val t: (Rep[_], Rep[_]) = value.asInstanceOf[Rep[(Any, Any)]]
              val (l, r) = t
              name -> (i match {
                case 0 => l
                case 1 => r
                case _ => throw InvalidPatternException(s"the product type ${product.name} has only 2 fields")
              })
            case _ => throw InvalidPatternException(s"trying to capture fields from ${ctor.typ}")
          }.toMap, src)
        )
      }
    }

    def patternMatch[B](x: Rep[B]): Rep[B] = if (cases.nonEmpty) {
      val tag = self.asInstanceOf[Sum2Rep[Any]].t
      for ((flag, ctor) <- cases) {
        if (tag == flag) return ctor.asInstanceOf[Rep[B]]
      }
      error("no match for " + tag)
    } else x

    sealed trait ExperimentalPattern {
      def apply(captures: String*)(implicit src: SourceContext): PatternOps =
        PatternOps(this, captures, src)
    }

    case object Left extends ExperimentalPattern
    case object Right extends ExperimentalPattern
  }

  sealed trait ADT[A] { self =>
    implicit def typ: Typ[A]
    final type TypeMember = A
    def name: String

    def apply(v: Rep[A]): Rep[self.type] =
      v.asInstanceOf[Rep[self.type]]

    def +[B: Typ](other: ADT[B]): ADTSum2[A, B] = ADTSum2(
      s"$name | ${other.name}",
      ADTConstructor(None, self),
      ADTConstructor(None, other)
    )

    def +[B: Typ](constructor: ADTConstructor[B]): ADTSum2[A, B] = ADTSum2(
      s"$name | ${constructor.name}",
      ADTConstructor(None, self),
      constructor
    )

    def *[B: Typ](other: ADT[B]): ADTProduct2[A, B] = ADTProduct2(
      s"($name * ${other.name})",
      ADTField(None, self),
      ADTField(None, other)
    )

    def *[B: Typ](field: ADTField[B]): ADTProduct2[A, B] = ADTProduct2(
      s"($name * ${field.name})",
      ADTField(None, self),
      field
    )

    def switch[B](v: Rep[self.type])(f: SwitchOps[A] => Rep[B])(implicit src: SourceContext): Rep[B] = {
      val ops = SwitchOps(self, v)
      // run the user switch body. This may fill in cases for sum types.
      // The go() method then takes care of either emitting appropriate
      // pattern-matching code or just returning the value from user code instead.
      ops.patternMatch(f(ops))
    }
  }

  sealed trait Pattern[A, R] {
    def apply(v: Rep[ADT[A]]): Rep[R]
  }

  case class PatSum2[A: Typ, B: Typ, R: Typ](l: Rep[A] => Rep[R], r: Rep[B] => Rep[R]) extends Pattern[Either[A, B], R] {
    override def apply(v: Rep[ADT[Either[A, B]]]): Rep[R] =
      if (v.asInstanceOf[Sum2Rep[Any]].t) l(v.asInstanceOf[Sum2Rep[A]].v)
      else r(v.asInstanceOf[Sum2Rep[B]].v)
  }

  implicit class PatProduct2[A: Typ, B: Typ, R](p: (Rep[A], Rep[B]) => Rep[R]) extends Pattern[(A, B), R] {
    override def apply(v: Rep[ADT[(A, B)]]): Rep[R] = {
      val t: (Rep[A], Rep[B]) = v.asInstanceOf[Rep[(A, B)]]
      val (a, b) = t
      p(a, b)
    }
  }

  implicit class FunOps[A: Typ, R: Typ](f: Rep[A] => Rep[R]) {
    def |[B: Typ](g: Rep[B] => Rep[R]): Pattern[Either[A, B], R] = PatSum2(f, g)
  }

  case class ADTUnit() extends ADT[Unit] {
    def name = "Unit"
    override implicit def typ: Typ[Unit] = unitTyp
  }
  case class ADTString() extends ADT[String] {
    def name = "String"
    override implicit def typ: Typ[String] = ???
  }
  case class ADTInt() extends ADT[Int] {
    def name = "Int"
    override implicit def typ: Typ[Int] = intTyp
  }
  case class ADTDouble() extends ADT[Double] {
    def name = "Double"
    override implicit def typ: Typ[Double] = doubleTyp
  }

  case class ADTField[A: Typ](name: Option[String], typ: ADT[A]) {
    def givenName: String = name.getOrElse("_1")
    def *[B: Typ](other: ADTField[B]): ADT[(A, B)] = ADTProduct2(
      s"{$givenName: ${typ.name}, ${other.givenName}: ${other.typ.name}}",
      this,
      other
    )
  }

  case class ADTConstructor[A: Typ](name: Option[String], typ: ADT[A]) {
    def +[B: Typ](other: ADTConstructor[B]): ADT[Either[A, B]] = ADTSum2(
      s"(${name.getOrElse("_1")}: ${typ.name} | ${other.name.getOrElse("_2")}: ${other.typ.name})",
      this,
      other
    )
  }

  case class ADTProduct2[A, B](name: String, l: ADTField[A], r: ADTField[B])(implicit val at: Typ[A], implicit val bt: Typ[B]) extends ADT[(A, B)] {
    override implicit def typ: Typ[(A, B)] = tuple2_typ
  }

  private type Sum2[T] = Record { val t: Boolean; val v: T }
  private type Sum2Rep[T] = Rep[Sum2[T]]
  case class ADTSum2[A, B](name: String, l: ADTConstructor[A], r: ADTConstructor[B])(implicit val at: Typ[A], implicit val bt: Typ[B]) extends ADT[Either[A, B]] {
    override implicit def typ: Typ[Either[A, B]] = ???
  }

  implicit def pairIsField[A: Typ](pair: (String, ADT[A])): ADTField[A] = ADTField(Some(pair._1), pair._2)
  implicit def pairIsConstructor[A: Typ](pair: (String, ADT[A])): ADTConstructor[A] = ADTConstructor(Some(pair._1), pair._2)

  implicit def liftEither[A: Typ, B: Typ](e: Either[A, B])(implicit aToRep: A => Rep[A], bToRep: B => Rep[B]): Rep[Either[A, B]] =
    eitherToTaggedUnion(e match {
      case Left(a) => Left(aToRep(a))
      case Right(b) => Right(bToRep(b))
    })

  implicit def eitherToTaggedUnion[A, B](e: Either[Rep[A], Rep[B]]): Rep[Either[A, B]] = e match {
    case Left(a) =>
      val record: Sum2Rep[A] = new Record {
        val t: Boolean = unit(true)
        val v: Any = a
      }.asInstanceOf[Sum2Rep[A]]
      record.asInstanceOf[Rep[Either[A, B]]]
    case Right(b) =>
      val record: Sum2Rep[B] = new Record {
        val t: Boolean = unit(false)
        val v: Any = b
      }.asInstanceOf[Sum2Rep[B]]
      record.asInstanceOf[Rep[Either[A, B]]]
  }

  object ADTs {
    def Option[A: Typ](t: ADT[A]) = ADTUnit() + t
  }

  implicit class ADTValueOps[D <: ADT[_]](x: Rep[D])(implicit val typ: D) extends Dynamic {
//    def selectDynamic[A](name: String): Rep[A] = typ match {
//      case ADTProduct2(nm, l, r) => () match {
//        // FIXME wrong
//        case _ if name == l.givenName => l.typ(x).asInstanceOf[Rep[A]]
//        case _ if name == r.givenName => r.typ(x).asInstanceOf[Rep[A]]
//        case _ => throw new IllegalArgumentException(s"$name is not a field of $nm")
//      }
//      case _ => throw new IllegalArgumentException(s"Unknown field $name")
//    }
  }



  /**
   * Natural transformation.
   */
  trait ~>[F[_], G[_]] { self =>
    def apply[A](fa: F[A]): G[A]
    def compose[K[_]](f: K ~> F): K ~> G = new (K ~> G) {
      def apply[A](ka: K[A]): G[A] = self(f(ka))
    }
    def andThen[K[_]](f: G ~> K): F ~> K = new (F ~> K) {
      def apply[A](fa: F[A]): K[A] = f(self(fa))
    }
  }

  case class Id[A](a: A)

  /*
  we want to express
    forall d b. Data d => c (d -> b) -> d -> c b
  in Scala terms. That's a natural transformation from (c (d -> _)) to (d -> c _),
  where d is constrained to Data.
   */

  trait ConstrainedNT[F[_, _], G[_, _], Constraint[_]] {
    def apply[A, C: Constraint](fa: F[C, A]): G[C, A]
  }

  /**
   *
   *
   *
   * @tparam A
   */
  trait Data[A] {
    /**
     * A port of the Haskell gfoldl for Data, see Hoogle for details.
     *
     * Left-associative fold operation for constructor applications.
     * The type of gfoldl is a headache, but operationally it is a simple generalisation of a list fold.
     * The default definition for gfoldl is const id, which is suitable for abstract datatypes with no substructures.
     *
     * "But I don't want to go among mad people," Alice remarked.
     * "Oh, you can't help that," said the Cat: "we're all mad here. I'm mad. You're mad."
     * "How do you know I'm mad?" said Alice.
     * "You must be," said the Cat, "or you wouldn't have come here."
     * ∼ Lewis Carroll, "Alice's Adventures in Wonderland"
     *
     * @param nonEmptyCtors defines how nonempty constructor applications are folded. It takes the folded tail
     *                      of the constructor application and its head, i.e., an immediate subterm,
     *                      and combines them in some way.
     * @param emptyCtors defines how the empty constructor application is folded, like the neutral / start
     *                   element for list folding.
     * @param d structure to be folded.
     * @tparam C
     * @return result, with a type defined in terms of [[A]], but variability is achieved by means of
     *         type constructor [[C]] for the construction of the actual result type.
     */
    def gfoldl[C[_]]
    (nonEmptyCtors: ConstrainedNT[
      ({type L[d, a] = C[d => a]})#L,
      ({type L[d, a] = d => C[a]})#L,
      Data
    ])                    // forall d b. Data d => c (d -> b) -> d -> c b
    (emptyCtors: Id ~> C) // forall g. g -> c g
    (d: A):               // a
    C[A]                  // c a
  }
}
