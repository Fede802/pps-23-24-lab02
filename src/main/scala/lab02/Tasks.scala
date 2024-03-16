package lab02

object Tasks extends App:

  def printFormatter(s: String = ""): Unit = s match
    case "" => println("---------")
    case _  => println("\n++++++++++++++++++"); println(s); println("++++++++++++++++++")

  enum Sequence[A]:
    case Cons(head: A, tail: Sequence[A])
    case Nil()

  object Sequence:
    def apply[A](x: A*): Sequence[A] = x.isEmpty match
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

  // Tasks – part 2a (functions), svolto da solo
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

  // non trivial to simulate/make genericNegWithLambdaStyle
  def genericNegWithMethodStyle[A](f: A => Boolean): A => Boolean = !f(_)

  printResult(genericNegWithMethodStyle(empty))(Sequence("foo", ""))
  printResult(genericNegWithMethodStyle(empty), _ && !_)(Sequence(("foo", "")))
  printResult(genericNegWithMethodStyle(positive))(Sequence(2, 0, -1))

  // Tasks – part 2b (functions), svolto da solo
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

  // Tasks – part 3 (recursion), svolto da solo
  // 6)
  printFormatter("Section 6")

  // didn't found an intuitive non tail-recursive version
  // the condition of Euclidean algorithm need b > 0 not a > b
  @annotation.tailrec
  def gcd(a: Int, b: Int): Int = b match
    case 0 => a
    case _ => gcd(b, a % b)

  printResult(gcd)(Sequence((8, 12), (7, 14)))

  // Tasks – part 4 (sum types, product types, modules), svolto da solo
  // 7)
  printFormatter("Section 7")

  enum Shape:
    case Rectangle(w: Double, h: Double)
    case Circle(r: Double)
    case Square(s: Double)

  object Shape:

    def perimeter(shape: Shape): Double = shape match
      case Rectangle(w, h) => (w + h) * 2
      case Circle(r)       => 2 * Math.PI * r
      case Square(s)       => s * 4

    def scale(shape: Shape, alpha: Double): Shape = shape match
      case Rectangle(w, h) => Rectangle(w * alpha, h * alpha)
      case Circle(r)       => Circle(r * alpha)
      case Square(s)       => Square(s * alpha)

  // Tasks – part 5 (more functional combinators), svolto da solo
  // 8)
  printFormatter("Section 8")

  enum Optional[A]:
    case Maybe(value: A)
    case Empty()

  object Optional:

    def isEmpty[A](o: Optional[A]): Boolean = o match
      case Empty() => true
      case _       => false

    def orElse[A, B >: A](o: Optional[A], default: B): B = o match
      case Maybe(v) => v
      case _        => default

    def map[A, B](o: Optional[A])(f: A => B): Optional[B] = o match
      case Maybe(v) => Maybe(f(v))
      case _        => Empty()

    def filter[A](o: Optional[A])(f: A => Boolean): Optional[A] = o match
      case Maybe(v) if (f(v)) => Maybe(v)
      case _                  => Empty()
