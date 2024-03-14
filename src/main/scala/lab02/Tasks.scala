package lab02

import u02.Values.s
import java.awt.print.Printable

object Tasks extends App :

    enum Functions[A,B]:
        case SingleArg(f: A => B)
        case DoubleArg(f: (A,A) => B)
        case SingleArgPredicate(f: A => Boolean)
    
    enum Inputs[A]:
        case SingleItem(a: List[A])
        case PairItem(a: List[(A,A)])
        
    import Functions.*
    import Inputs.*

    def printFormatter(s:String = ""): Unit = s match
        case "" => println("---------")
        case _ => {println("\n++++++++++++++++++");println(s);println("++++++++++++++++++")}

    enum Printable[A,B]:
        case SingleArgFunction(f: SingleArg[A,B], i: SingleItem[A])
        case DoubleArgFunction(f: DoubleArg[A,B], i: PairItem[A])
        case SingleArgAndFunction(f: SingleArgPredicate[A,B], i: PairItem[A])

    object Printable:
        
        @annotation.tailrec
        def _iterateTuple[A,B](f: Functions[A,B], l: Inputs[A], index: Int): Unit = (f,l) match
             case (SingleArg(fun), SingleItem(a)) if index < a.size => {println(fun(a(index))); _iterateTuple(f,l,index+1)}
             case (DoubleArg(fun), PairItem(a)) if index < a.size => {println(fun(a(index)._1,a(index)._2)); _iterateTuple(f,l,index+1)}
             case (SingleArgPredicate(fun), PairItem(a)) if index < a.size => {println(fun(a(index)._1) && fun(a(index)._2)); _iterateTuple(f,l,index+1)}
             case _ => printFormatter()
        
        def printResult[A,B](printable: Printable[A,B]): Unit = printable match
            case SingleArgFunction(f,i) => _iterateTuple(f,i,0)
            case DoubleArgFunction(f,i) => _iterateTuple(f,i,0)
            case SingleArgAndFunction(f,i) => _iterateTuple(f,i,0)
            
    import Printable.*
      
    //Tasks part 2a
    //3.a)
    printFormatter("Section 3a")
    val evaluateSignInLambdaStyle: Int => String = _ match
        case x if x >= 0 => "positive"
        case _ => "negative"

    printResult(SingleArgFunction(SingleArg(evaluateSignInLambdaStyle),SingleItem(List(2,0,-1))))
    
    def evaluateSignInMethodStyle(x: Int): String = x match
        case x if x >= 0 => "positive"
        case _ => "negative"

    printResult(SingleArgFunction(SingleArg(evaluateSignInMethodStyle),SingleItem(List(2,0,-1))))

    //3.b)
    printFormatter("Section 3b")
    def evaluateStringAndPrint(f: String => Boolean): Unit =
        println(f("foo"))
        println(f(""))
        println(f("foo") && !f(""))

    val empty: String => Boolean = _ == ""

    val negWithLambdaStyle: (String => Boolean) => String => Boolean = f => !f(_)
    printResult(SingleArgFunction(SingleArg(negWithLambdaStyle(empty)),SingleItem(List("foo",""))))
    printResult(SingleArgAndFunction(SingleArgPredicate(negWithLambdaStyle(empty)),PairItem(List(("foo","")))))

    def negWithMethodStyle(f:String => Boolean): String => Boolean = !f(_)
    printResult(SingleArgFunction(SingleArg(negWithMethodStyle(empty)),SingleItem(List("foo",""))))
    printResult(SingleArgAndFunction(SingleArgPredicate(negWithMethodStyle(empty)),PairItem(List(("foo","")))))

    //3.c)
    printFormatter("Section 3c")
    val positive: Int => Boolean = _ >= 0     
    //not trivial to simulate/make genericNegWithLambdaStyle
    def genericNegWithMethodStyle[A](f:A => Boolean): A => Boolean = !f(_)
    printResult(SingleArgFunction(SingleArg(genericNegWithMethodStyle(empty)),SingleItem(List("foo",""))))
    printResult(SingleArgAndFunction(SingleArgPredicate(genericNegWithMethodStyle(empty)),PairItem(List(("foo","")))))
    printResult(SingleArgFunction(SingleArg(genericNegWithMethodStyle(positive)),SingleItem(List(2,0,-1))))

    //Tasks part 2b
    //4)
    printFormatter("Section 4")
    val p1: Integer => Integer => Boolean => Boolean = x => y => z => x < y == z
    val p2: (Integer, Integer, Boolean) => Boolean = _ <= _ == _
    def p3(x: Integer)(y: Integer)(z: Boolean): Boolean = x <= y == z
    def p4(x: Integer, y: Integer, z: Boolean): Boolean = x <= y == z

    //5)
    printFormatter("Section 5")
    def compose(f: Int => Int, g: Int => Int): Int => Int = i => f(g(i))
    printResult(SingleArgFunction(SingleArg(compose(_ - 1, _ * 2)),SingleItem(List(5))))

    

    //the constraint is continuity of types between g and f (output of g must be equal to input of f)
    def genericCompose[A,B,C](f: B => C, g: A => B): A => C = i => f(g(i))
    printResult(SingleArgFunction(SingleArg(genericCompose[Int, Int, Int](_ - 1, _ * 2)),SingleItem(List(5))))
    

    //Tasks part 3
    //6)
    printFormatter("Section 6")
    //didn't found an intuitive non tail-recursive version
    //wrong hint in the slides, the condition of Euclidean algorithm need b > 0 not a > b
    @annotation.tailrec
    def gcd(a: Int, b: Int): Int = b match
        case b if b == 0 => a
        case _ => gcd(b, a % b)
    printResult(DoubleArgFunction(DoubleArg(gcd),PairItem(List((8,12),(7,14)))))
    
    //Tasks part 4
    //7)
    enum Shape:
        case Rectangle(witdh: Double, height: Double)
        case Circle(radius: Double)
        case Square(size: Double)

    object Shape:
        def perimeter(shape: Shape): Double = shape match
            case Rectangle (w,h) => (w+h)*2
            case Circle (r) => 2*Math.PI*r
            case Square(s) => s*s

        def scale(shape: Shape, alpha: Double): Shape = shape match
            case Rectangle (w,h) => Rectangle(w*alpha,h*alpha)
            case Circle (r) => Circle(r*alpha)
            case Square(s) => Square(s*alpha)

    //Tasks part 5
    //8)

    /**
   * Optional is a type that represents a value that may or may not be present.
   * Similar to Optional in Java but using the ADT concept.
   * Therefore, an Optional is a sum type with two cases: Maybe and Empty.
   * Maybe contains the value, and Empty represents the absence of a value.
   *
   * @tparam A
   */
    enum Optional[+A]:
        case Maybe(value: A)
        case Empty()

    object Optional:
    /**
     * isEmpty returns true if the optional is Empty, false otherwise.
     * Example:
     *
     * isEmpty(Empty()) == true
     * isEmpty(Maybe(1)) == false
     *
     * @param optional the optional to check
     * @tparam A the type of the optional
     * @return true if the optional is Empty, false otherwise
     */
        def isEmpty[A](optional: Optional[A]): Boolean = optional match
            case Empty() => true
            case _ => false

    /**
     *
     * getOrElse returns the value of the optional if it is Maybe, otherwise it returns the default value.
     * Example:
     * orElse(Maybe(1), 0) == 1
     * orElse(Empty(), 0) == 0
     *
     * @param optional the optional to get the value from
     * @param default the default value to return if the optional is Empty
     * @tparam A the type of the optional
     * @tparam B the type of the default value
     * @return the value of the optional if it is Maybe, otherwise the default value
     */
        def orElse[A, B >: A](optional: Optional[A], default: B): B = optional match
            case Maybe(value) => value
            case Empty() => default

    
    /**
     * map applies the function f to the value of the optional if it is Maybe, otherwise it returns Empty.
     * Example:
     *
     * map(Maybe(1), (x: Int) => x + 1) == Maybe(2)
     * map(Empty(), (x: Int) => x + 1) == Empty()
     *
     *
     * @param optional the optional to apply the function to
     * @param f the function to apply to the value of the optional
     * @tparam A the type of the optional
     * @tparam B the type of the result of the function
     * @return the result of applying the function to the value of the optional if it is Maybe, otherwise Empty
     */
        def map[A, B](optional: Optional[A], f: A => B): Optional[B] = optional match
            case Empty() => Empty()
            case Maybe(x) => Maybe(f(x))

        def filter[A](o: Optional[A])(f: A => Boolean): Optional[A] = o match
            case Maybe(x) if (f(x))=> Maybe(x)
            case _ => Empty()

   

   

    