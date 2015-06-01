package slick.mongodb.lifted

import scala.reflect.ClassTag
import slick.ast.{Ordering, BaseTypedType, ScalaType}
import slick.profile.RelationalTypesComponent


trait MongoTypesComponent extends RelationalTypesComponent{driver: MongoDriver =>
  // TODO: check
  override type ColumnType[T] = ScalaType[T]
  override type BaseColumnType[T] = ScalaType[T] with BaseTypedType[T]
  override val MappedColumnType: MappedColumnTypeFactory = MappedColumnTypeFactory // TODO: review if MappedColumnType is correctly implemented


  object MappedColumnTypeFactory extends super.MappedColumnTypeFactory {
    def base[T : ClassTag, U : BaseColumnType](tmap: T => U, tcomap: U => T): BaseColumnType[T] = {
      assertNonNullType(implicitly[BaseColumnType[U]])
      new MappedColumnType(implicitly[BaseColumnType[U]], tmap, tcomap)
    }
  }

  class MappedColumnType[T, U](val baseType: ColumnType[U], toBase: T => U, toMapped: U => T)(implicit val classTag: ClassTag[T]) extends ScalaType[T] with BaseTypedType[T] {
    def nullable: Boolean = baseType.nullable
    def ordered: Boolean = baseType.ordered
    def scalaOrderingFor(ord: Ordering): scala.math.Ordering[T] = new scala.math.Ordering[T] {
      val uOrdering = baseType.scalaOrderingFor(ord)
      def compare(x: T, y: T): Int = uOrdering.compare(toBase(x), toBase(y))
    }
  }
}

