package cats.collections

/**
 * Helper trait for `newtype`s. These allow you to create a zero-allocation wrapper around a specific type.
 * Similar to `AnyVal` value classes, but never have any runtime overhead.
 * It's coped from the newtypes lib by @alexknvl
 * For more detail see https://github.com/alexknvl/newtypes
 */
private[collections] trait Newtype { self =>
  private[collections] type Base
  private[collections] trait Tag extends Any
  type Type[A] <: Base with Tag
}
