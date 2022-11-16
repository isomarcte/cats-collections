package cats.collections

import cats.kernel._
import cats.syntax.all._

sealed abstract class Interval[A] extends Serializable {
  def startInclusive: Option[A]
  def endExclusive: Option[A]

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
    new Interval.Iterator[A](startInclusive, endExclusive)

  final def toList(implicit A: PartialNext[A]): List[A] =
    toIterator.toList

  override final def toString: String =
    startInclusive.fold(
      "Interval(empty)"
    )(startInclusive =>
      endExclusive.fold(
        s"Interval(${startInclusive} until infinity)"
      )(endExclusive =>
        s"Interval(${startInclusive} until ${endExclusive})"
      )
    )
}

object Interval {
  private[this] final case class IntervalImpl[A](override val startInclusive: Option[A], override val endExclusive: Option[A]) extends Interval[A]

  private final class EmptyIterator[A] extends scala.collection.AbstractIterator[A] {
    override val hasNext: Boolean = false
    override def next(): A =
      throw new NoSuchElementException
  }

  private final class Iterator[A](startInclusive: A, endExclusive: Option[A])(implicit private val partialNext: PartialNext[A]) extends scala.collection.AbstractIterator[A] {
    private var cursor: A = startInclusive
    private var first: Boolean = true
    private val empty: Boolean =
      partialNext.partialOrder.lteqv(startInclusive, endExclusive)

    override def hasNext: Boolean =
      !empty && (first || (partialNext.partialOrder.lt(cursor, endExclusive) && partialNext.partialNext(cursor).isDefined))

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

  def untilP[A: PartialOrder](startInclusive: A, endExclusive: A): Either[String, Interval[A]] =
    startInclusive.partialCompare(endExclusive) match {
      case result if result <= 0d =>
        Right(IntervalImpl(startInclusive, endExclusive))
      case result if result > 0d =>
        Left(s"start must be <= end, but ${startInclusive} > ${endExclusive}")
      case _ =>
        Left(s"start must be <= end, but ${startInclusive} is not comparable to ${endExclusive}")
    }

  def unsafeUntilP[A: PartialOrder](startInclusive: A, endExclusive: A): Interval[A] =
    untilP[A](startInclusive, endExclusive).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def until[A: Order](startInclusive: A, endExclusive: A): Interval[A] =
    startInclusive.compare(endExclusive) match {
      case value if value <= 0 =>
        IntervalImpl(startInclusive, endExclusive)
      case _ =>
        IntervalImpl(startInclusive, startInclusive)
    }

  def untilStrict[A: Order](startInclusive: A, endExclusive: A): Either[String, Interval[A]] =
    startInclusive.compare(endExclusive) match {
      case value if value <= 0 =>
        Right(IntervalImpl(startInclusive, endExclusive))
      case _ =>
        Left(s"start must be <= end, but got ${startInclusive} > ${endExclusive}")
    }

  def to[A](startInclusive: A, endInclusive: A)(implicit A: PartialNext[A]): Either[String, Interval[A]] =
    A.partialNext(endInclusive).fold(
      Left(s"Unable to determine element following end: ${endInclusive}"): Either[String, Interval[A]]
    )(endExclusive =>
      untilP[A](startInclusive, endExclusive)(A.partialOrder)
    )

  def unsafeTo[A](startInclusive: A, endInclusive: A)(implicit A: PartialNext[A]): Interval[A] =
    to[A](startInclusive, endInclusive).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
