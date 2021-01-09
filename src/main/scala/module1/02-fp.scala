package module1

import scala.annotation.tailrec

/**
 * Реализуем тип Option.
 */

object opt {

  /**
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата.
   */

  sealed trait Option[+A] {
    /**
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть.
     */
    def printIfAny(): Unit = this match {
      case Option.Some(v) => print(v)
      case Option.None => ()
    }

    /**
     * реализовать метод orElse, который будет возвращать другой Option, если данный пустой.
     */
    def orElse[B >: A](o: Option[B]): Option[B] = this match {
      case Option.Some(_) => this
      case Option.None => o
    }

    /**
     * Реализовать метод isEmpty, который будет возвращать true, если Option не пуст, и false в
     * противном случае.
     */
    def isEmpty: Boolean = this match {
      case Option.Some(_) => false
      case Option.None => true
    }

    /**
     * Реализовать метод get, который будет возвращать значение.
     */
    def get: A = this match {
      case Option.Some(v) => v
      case Option.None => throw new Exception("get on empty Option")
    }

    /**
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option.
     */
    def zip[B](o: Option[B]): Option[(A, B)] = (this, o) match {
      case (Option.Some(v1), Option.Some(v2)) => Option.Some(v1, v2)
      case _ => Option.None
    }

    /**
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае, если исходный не пуст и предикат от значения равен true.
     */
    def filter(p: A => Boolean): Option[A] = this match {
      case Option.Some(v) if p(v) => this
      case _ => Option.None
    }
  }

  object Option {
    case class Some[A](v: A) extends Option[A]
    case object None extends Option[Nothing]
  }
}

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Long = {
    var _n = 1L
    var i = 2
    while (i <= n) {
      _n *= i
      i += 1
    }
    _n
  }

  def !!(n: Int): Long = {
    if(n <= 1) 1
    else n * !!(n - 1)
  }

  def !(n: Int): Long = {
    @tailrec
    def loop(n1: Int, acc: Long): Long = {
      if(n <= 1) acc
      else loop(n1 - 1, n1 * acc)
    }
    loop(n, 1)
  }
}

object list {

  /**
   * Реализовать односвязанный имутабельный список List.
   */

  sealed trait List[+A]{
    import List.{Cons, Nil, ::}

    override def toString: String = this.mkString

    /**
     * Реализовать метод cons ::, который позволит добавлять элемент в голову списка.
     */
    def ::[A1 >: A](head: A1): List[A1] = Cons(head, this)

   /**
    * Реализовать метод mkString, который позволит красиво представить список в виде строки.
    */
    def mkString: String = mkString(", ")
    def mkString(sep: String): String = {
      def loop(l: List[A], acc: StringBuilder): StringBuilder =
        l match {
          case Nil => acc
          case h :: Nil => acc.append(h)
          case h :: t => loop(t, acc.append(s"$h$sep"))
        }
      loop(this, new StringBuilder()).mkString
    }

   /**
    * Реализовать метод reverse, который позволит заменить порядок элементов в списке на
    * противоположный.
    */
    def reverse: List[A] = {
      @tailrec
      def iter(list: List[A], accu: List[A]): List[A] = list match {
        case h :: t => iter(t, h :: accu)
        case Nil => accu
      }
      iter(this, Nil)
    }

   /**
    * Реализовать метод для списка, который будет применять некоторую функццию к элементам данного
    * списка.
    */
    def map[B](f: A => B): List[B] = this match {
      case h :: t => f(h) :: t.map(f)
      case Nil => Nil
    }
  }

  object List {
    case class ::[A](head: A, tail: List[A]) extends List[A]
    case object Nil extends List[Nothing]
    val Cons = ::

    /**
     * Реализовать конструктор, для создания списка n элементов
     */
    def apply[T](arg: T*): List[T] =
      arg.foldRight[List[T]](Nil)((x: T, l: List[T]) => x :: l)

    /**
     * Написать функцию incList, которая будет принимать список Int и возвращать список,
     * где каждый элемент будет увеличен на 1.
      */
    def incList(l: List[Int]): List[Int] = l.map(_ + 1)

    /**
     * Написать функцию shoutString, котрая будет принимать список String и возвращать список,
     * где к каждому элементу будет добавлен префикс в виде '!'.
     */
    def shoutString(l: List[String]): List[String] = l.map(s => s"$s!")
  }
}
