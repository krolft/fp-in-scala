import scala.reflect.ClassTag

object MainProcedureReturnsUnit {

  def empty(): Unit = {
    // what type has a an empty block?
    // why is this satisfying the empty's signature?
    // let's find out => see main
  }

  def main(args: Array[String]): Unit = {
    println(s"type of {} is: ${f({})}")
    println(s"type of () is: ${f(())}")
  }

  // see https://stackoverflow.com/questions/19386964/i-want-to-get-the-type-of-a-variable-at-runtime
<<<<<<< HEAD
  def f[T](v: T)(implicit ev: ClassTag[T]) = {
=======
  def f[T](v: T)(implicit ev: ClassTag[T]): String = {
>>>>>>> 10b7e907080f169bffc46c5b06e510d4170f4dc0
    ev.toString
  }

  def producesUnit(): Unit = ()

  def producesAlsoUnit(): Unit = {}

}
