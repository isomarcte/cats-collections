package cats.collections

import cats.kernel._
import cats.syntax.all._

sealed abstract class Interval[A] extends Serializable {
  def bounds: Option[(A, Option[A])]

  final def startInclusive: Option[A] =
    bounds.map(_._1)

  final def endExclusive: Option[A] =
    bounds.flatMap(_._2)

  final def startExclusive(implicit A: PartialNext[A]): Option[A] =
    startInclusive.flatMap(A.partialNext)

  final def endInclusive(implicit A: PartialPrevious[A]): Option[A] =
    endExclusive.flatMap(
      A.partialPrevious
    ).flatMap(value =>
      startInclusive.flatMap(start =>
        if (A.partialOrder.lteqv(start, value)) {
          Some(value)
        } else {
          None
        }
      )
    )

  final def isLeftOf(other: Interval[A])(implicit A: PartialOrder[A]): Boolean =
    endExclusive <= other.startInclusive

  final def isRightOf(other: Interval[A])(implicit A: PartialOrder[A]): Boolean =
    other.endExclusive <= startInclusive

  final def overlapsWith(other: Interval[A])(implicit A: PartialOrder[A]): Boolean =
    !(isLeftOf(other) || isRightOf(other))

  final def isEmpty: Boolean =
    startInclusive.isEmpty

  final def isFinite: Boolean =
    endExclusive.nonEmpty

  final def toIterator(implicit A: PartialNext[A]): Iterator[A] =
    bounds match {
      case Some((s, e)) =>
        new Interval.NonEmptyIterator[A](s, e)
      case _ =>
        new Interval.EmptyIterator[A]
    }

  final def inverse(implicit A0: Previous[A], A1: Order[A]): Interval[Inverse[A]] =
    bounds match {
      case (Some((s, Some(e)))) =>
        Interval.until(Inverse(A0.previous(e)), Inverse(s))
      case (Some((s, None))) =>
        Interval.infiniteOf(Inverse(s))
      case _ =>
        Interval.empty
    }

  final def toList(implicit A: PartialNext[A]): List[A] =
    toIterator.toList

  override final def toString: String =
    bounds match {
      case Some((s, Some(e))) =>
        s"Interval($s until $e)"
      case Some((s, None)) =>
        s"Interval($s until infinity)"
      case None =>
        "Interval(empty)"
    }
}

object Interval {
  private[this] final case class IntervalImpl[A](override val bounds: Option[(A, Option[A])]) extends Interval[A]

  private final class EmptyIterator[A] extends scala.collection.AbstractIterator[A] {
    override val hasNext: Boolean = false
    override def next(): A =
      throw new NoSuchElementException
  }

  private final class NonEmptyIterator[A](startInclusive: A, endExclusive: Option[A])(implicit private val partialNext: PartialNext[A]) extends scala.collection.AbstractIterator[A] {
    private var cursor: A = startInclusive
    private var first: Boolean = true

    override def hasNext: Boolean =
      if (first) {
        true
      } else {
        endExclusive match {
          case Some(endExclusive) =>
            partialNext.partialNext(cursor) match {
              case Some(next) =>
                partialNext.partialOrder.lt(next, endExclusive)
              case _ =>
                false
            }
          case _ =>
            true
        }
      }

    override def next(): A =
      if (hasNext) {
        if (first) {
          first = false
          cursor
        } else {
          cursor = partialNext.partialNext(cursor).get
          cursor
        }
      } else {
        throw new NoSuchElementException
      }
  }

  def empty[A]: Interval[A] =
    IntervalImpl(None)

  def untilPartial[A: PartialOrder](startInclusive: A, endExclusive: A): Either[String, Interval[A]] =
    startInclusive.partialCompare(endExclusive) match {
      case result if result <= 0d =>
        Right(IntervalImpl((Some((startInclusive, Some(endExclusive))))))
      case result if result > 0d =>
        Right(IntervalImpl(None))
      case _ =>
        Left(s"start must be <= end, but ${startInclusive} is not comparable to ${endExclusive}")
    }

  def unsafeUntilPartial[A: PartialOrder](startInclusive: A, endExclusive: A): Interval[A] =
    untilPartial[A](startInclusive, endExclusive).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def until[A: Order](startInclusive: A, endExclusive: A): Interval[A] =
    startInclusive.compare(endExclusive) match {
      case value if value <= 0 =>
        IntervalImpl(Some((startInclusive, Some(endExclusive))))
      case _ =>
        IntervalImpl(None)
    }

  def infiniteOf[A](startInclusive: A)(implicit A: Next[A]): Interval[A] =
    IntervalImpl(Some((startInclusive, None)))
}
