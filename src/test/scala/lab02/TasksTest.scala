package lab02
import org.junit.*
import org.junit.Assert.*
import lab02.Tasks.Optional
import lab02.Tasks.evaluateSignInLambdaStyle
import lab02.Tasks.evaluateSignInMethodStyle
import lab02.Tasks.negWithLambdaStyle
import lab02.Tasks.negWithMethodStyle
import lab02.Tasks.genericNegWithMethodStyle
import lab02.Tasks.compose
import lab02.Tasks.genericCompose
import java.awt.Rectangle
import lab02.Tasks.Shape

class TasksTest {

  @Test def signEvaluation(): Unit =
    assertEquals("positive", evaluateSignInLambdaStyle(2))
    assertEquals("positive", evaluateSignInLambdaStyle(0))
    assertEquals("negative", evaluateSignInLambdaStyle(-2))

    assertEquals("positive", evaluateSignInMethodStyle(2))
    assertEquals("positive", evaluateSignInMethodStyle(0))
    assertEquals("negative", evaluateSignInMethodStyle(-2))

  @Test def negEvaluation(): Unit =
    val empty: String => Boolean = _ == ""
    val positive: Int => Boolean = _ >= 0

    assertTrue(negWithLambdaStyle(empty)("foo"))
    assertFalse(negWithLambdaStyle(empty)(""))
    assertTrue(
      negWithLambdaStyle(empty)("foo") && !negWithLambdaStyle(empty)("")
    )

    assertTrue(negWithMethodStyle(empty)("foo"))
    assertFalse(negWithMethodStyle(empty)(""))
    assertTrue(
      negWithMethodStyle(empty)("foo") && !negWithMethodStyle(empty)("")
    )

    assertTrue(genericNegWithMethodStyle(empty)("foo"))
    assertFalse(genericNegWithMethodStyle(empty)(""))
    assertTrue(
      genericNegWithMethodStyle(empty)("foo") && !genericNegWithMethodStyle(
        empty
      )("")
    )

    assertFalse(genericNegWithMethodStyle(positive)(2))
    assertFalse(genericNegWithMethodStyle(positive)(0))
    assertTrue(genericNegWithMethodStyle(positive)(-2))

  @Test def composeEvaluation(): Unit =
    assertEquals(9, compose(_ - 1, _ * 2)(5))
    assertEquals(9, genericCompose[Int, Int, Int](_ - 1, _ * 2)(5))

  @Test def shapeCreation(): Unit =
    val x = 5;
    val y = 2;
    val r = Shape.Rectangle(x, y)
    val c = Shape.Circle(x)
    val s = Shape.Square(x)
    assertEquals(x, r match { case Shape.Rectangle(x, _) => x }, 0)
    assertEquals(y, r match { case Shape.Rectangle(_, y) => y }, 0)
    assertEquals(x, c match { case Shape.Circle(r) => r }, 0)
    assertEquals(x, s match { case Shape.Square(s) => s }, 0)

  @Test def perimeterCalculation(): Unit =
    val x = 5;
    val y = 2;
    val r = Shape.Rectangle(x, y)
    val c = Shape.Circle(x)
    val s = Shape.Square(x)
    assertEquals((x + y) * 2, Shape.perimeter(r), 0)
    assertEquals(2 * Math.PI * x, Shape.perimeter(c), 0)
    assertEquals(x * 4, Shape.perimeter(s), 0)

  @Test def shapeScaling(): Unit =
    val x = 5;
    val y = 2;
    val alpha = 2
    val r = Shape.Rectangle(x, y)
    val c = Shape.Circle(x)
    val s = Shape.Square(x)
    assertEquals(Shape.Rectangle(x * alpha, y * alpha), Shape.scale(r, alpha))
    assertEquals(Shape.Circle(x * alpha), Shape.scale(c, alpha))
    assertEquals(Shape.Square(x * alpha), Shape.scale(s, alpha))

    // to test shape creation with negative values

  @Test def emptyOptionalShouldBeEmpty(): Unit =
    val empty = Optional.Empty()
    assertTrue(Optional.isEmpty(empty))

  @Test def nonEmptyOptionalShouldNotBeEmpty(): Unit =
    val nonEmpty = Optional.Maybe(0)
    assertFalse(Optional.isEmpty(nonEmpty))

  @Test def orElseShouldReturnDefaultWhenEmpty(): Unit =
    val nonEmpty = Optional.Maybe(0)
    assertEquals(0, Optional.orElse(nonEmpty, 1))

  @Test def orElseShouldReturnValueWhenNonEmpty(): Unit =
    val empty = Optional.Empty()
    assertEquals(1, Optional.orElse(empty, 1))

  @Test def mapShouldReturnEmptyWhenEmpty(): Unit =
    val empty: Optional[Int] = Optional.Empty()
    val result = Optional.map(empty)(_ + 1)
    assertTrue(Optional.isEmpty(result))

  @Test def mapShouldReturnTransformedValueWhenNonEmpty(): Unit =
    val nonEmpty = Optional.Maybe(0)
    val result = Optional.map(nonEmpty)(_ + 1)
    assertEquals(1, Optional.orElse(result, 1))

  @Test def filterShouldReturnEmptyWhenEmpty(): Unit =
    val empty: Optional[Int] = Optional.Empty()
    val result = Optional.filter(empty)(_ > 10)
    assertTrue(Optional.isEmpty(result))

  @Test def filterShouldReturnWhenConditionIsSatisfied(): Unit =
    val valid = Optional.Maybe(20)
    val invalid = Optional.Maybe(0)
    val filter: Int => Boolean = _ > 10
    val validResult = Optional.filter(valid)(filter)
    val invalidResult = Optional.filter(invalid)(filter)
    assertEquals(20, Optional.orElse(validResult, 1))
    assertTrue(Optional.isEmpty(invalidResult))

}
