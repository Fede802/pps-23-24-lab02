package lab02

import u02.Values.s
import java.awt.print.Printable

object Tasks extends App:

  def printFormatter(s: String = ""): Unit = s match
    case "" => println("---------")
    case _ => {
      println("\n++++++++++++++++++"); println(s); println("++++++++++++++++++")
    }

  enum Sequence[A]:
    case Cons(head: A, tail: Sequence[A])
    case Nil()

  object Sequence:
    def apply[E](x: E*): Sequence[E] = x.isEmpty match
      case true => Nil()
      case _    => Cons(x.head, apply(x.tail: _*))

  import Sequence.*

  object Evaluator:
    def printResult[A, B](f: A => B)(l: Sequence[A]): Unit = l match
      case Cons(h, t) => println(f(h)); printResult(f)(t)
      case _          => printFormatter()

    def printResult[A, B](f: (A, A) => B)(l: Sequence[(A, A)]): Unit = l match
      case Cons(h, t) => println(f(h(0), h(1))); printResult(f)(t)
      case _          => printFormatter()

    def printResult[A, B](f: A => Boolean, p: (Boolean, Boolean) => Boolean)(
        l: Sequence[(A, A)]
    ): Unit = l match
      case Cons(h, t) => println(p(f(h(0)), f(h(1)))); printResult(f, p)(t)
      case _          => printFormatter()

  import Evaluator.*
  // Tasks part 2a
  // 3.a)
  printFormatter("Section 3a")
  val evaluateSignInLambdaStyle: Int => String = _ match
    case x if x >= 0 => "positive"
    case _           => "negative"

  printResult(evaluateSignInLambdaStyle)(Sequence(2, 0, -1))

  def evaluateSignInMethodStyle(x: Int): String = x match
    case x if x >= 0 => "positive"
    case _           => "negative"

  printResult(evaluateSignInMethodStyle)(Sequence(2, 0, -1))

  // 3.b)
  printFormatter("Section 3b")
  def evaluateStringAndPrint(f: String => Boolean): Unit =
    println(f("foo"))
    println(f(""))
    println(f("foo") && !f(""))

  val empty: String => Boolean = _ == ""

  val negWithLambdaStyle: (String => Boolean) => String => Boolean = f => !f(_)
  printResult(negWithLambdaStyle(empty))(Sequence("foo", ""))
  printResult(negWithLambdaStyle(empty), _ && !_)(Sequence(("foo", "")))

  def negWithMethodStyle(f: String => Boolean): String => Boolean = !f(_)
  printResult(negWithMethodStyle(empty))(Sequence("foo", ""))
  printResult(negWithMethodStyle(empty), _ && !_)(Sequence(("foo", "")))

  // 3.c)
  printFormatter("Section 3c")
  val positive: Int => Boolean = _ >= 0
  // not trivial to simulate/make genericNegWithLambdaStyle
  def genericNegWithMethodStyle[A](f: A => Boolean): A => Boolean = !f(_)
  printResult(genericNegWithMethodStyle(empty))(Sequence("foo", ""))
  printResult(genericNegWithMethodStyle(empty), _ && !_)(Sequence(("foo", "")))
  printResult(genericNegWithMethodStyle(positive))(Sequence(2, 0, -1))

  // Tasks part 2b
  // 4)
  printFormatter("Section 4")
  val p1: Integer => Integer => Boolean => Boolean = x => y => z => x < y == z
  val p2: (Integer, Integer, Boolean) => Boolean = _ <= _ == _
  def p3(x: Integer)(y: Integer)(z: Boolean): Boolean = x <= y == z
  def p4(x: Integer, y: Integer, z: Boolean): Boolean = x <= y == z

  // 5)
  printFormatter("Section 5")
  def compose(f: Int => Int, g: Int => Int): Int => Int = i => f(g(i))
  printResult(compose(_ - 1, _ * 2))(Sequence(5))

  // the constraint is continuity of types between g and f (output of g must be equal to input of f)
  def genericCompose[A, B, C](f: B => C, g: A => B): A => C = i => f(g(i))
  printResult(genericCompose[Int, Int, Int](_ - 1, _ * 2))(Sequence(5))

  // Tasks part 3
  // 6)
  printFormatter("Section 6")
  // didn't found an intuitive non tail-recursive version
  // wrong hint in the slides, the condition of Euclidean algorithm need b > 0 not a > b
  @annotation.tailrec
  def gcd(a: Int, b: Int): Int = b match
    case b if b == 0 => a
    case _           => gcd(b, a % b)
  printResult(gcd)(Sequence((8, 12), (7, 14)))

  // Tasks part 4
  // 7)
  enum Shape:
    case Rectangle(witdh: Double, height: Double)
    case Circle(radius: Double)
    case Square(size: Double)

  object Shape:
    def perimeter(shape: Shape): Double = shape match
      case Rectangle(w, h) => (w + h) * 2
      case Circle(r)       => 2 * Math.PI * r
      case Square(s)       => s * s

    def scale(shape: Shape, alpha: Double): Shape = shape match
      case Rectangle(w, h) => Rectangle(w * alpha, h * alpha)
      case Circle(r)       => Circle(r * alpha)
      case Square(s)       => Square(s * alpha)

  // Tasks part 5
  // 8)

  /** Optional is a type that represents a value that may or may not be present.
    * Similar to Optional in Java but using the ADT concept. Therefore, an
    * Optional is a sum type with two cases: Maybe and Empty. Maybe contains the
    * value, and Empty represents the absence of a value.
    *
    * @tparam A
    */
  enum Optional[A]:
    case Maybe(value: A)
    case Empty()

  object Optional:
    /** isEmpty returns true if the optional is Empty, false otherwise. Example:
      *
      * isEmpty(Empty()) == true isEmpty(Maybe(1)) == false
      *
      * @param optional
      *   the optional to check
      * @tparam A
      *   the type of the optional
      * @return
      *   true if the optional is Empty, false otherwise
      */
    def isEmpty[A](optional: Optional[A]): Boolean = optional match
      case Empty() => true
      case _       => false

    /** getOrElse returns the value of the optional if it is Maybe, otherwise it
      * returns the default value. Example: orElse(Maybe(1), 0) == 1
      * orElse(Empty(), 0) == 0
      *
      * @param optional
      *   the optional to get the value from
      * @param default
      *   the default value to return if the optional is Empty
      * @tparam A
      *   the type of the optional
      * @tparam B
      *   the type of the default value
      * @return
      *   the value of the optional if it is Maybe, otherwise the default value
      */
    def orElse[A, B >: A](optional: Optional[A], default: B): B = optional match
      case Maybe(value) => value
      case Empty()      => default

    /** map applies the function f to the value of the optional if it is Maybe,
      * otherwise it returns Empty. Example:
      *
      * map(Maybe(1), (x: Int) => x + 1) == Maybe(2) map(Empty(), (x: Int) => x
      * + 1) == Empty()
      *
      * @param optional
      *   the optional to apply the function to
      * @param f
      *   the function to apply to the value of the optional
      * @tparam A
      *   the type of the optional
      * @tparam B
      *   the type of the result of the function
      * @return
      *   the result of applying the function to the value of the optional if it
      *   is Maybe, otherwise Empty
      */
    def map[A, B](optional: Optional[A], f: A => B): Optional[B] =
      optional match
        case Empty()  => Empty()
        case Maybe(x) => Maybe(f(x))

    def filter[A](o: Optional[A])(f: A => Boolean): Optional[A] = o match
      case Maybe(x) if (f(x)) => Maybe(x)
      case _                  => Empty()
