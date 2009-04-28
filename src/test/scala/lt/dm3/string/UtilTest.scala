package lt.dm3.string

import org.scalatest._
import org.scalatest.testng.TestNGSuite

import org.testng.annotations.Configuration
import org.testng.annotations.Test
import org.testng.annotations.DataProvider

import org.hamcrest.MatcherAssert._
import org.hamcrest.Matchers._

import lt.dm3.string.Util._

class UtilTest extends TestNGSuite {

  @Test{ val dataProvider = "stringProvider" }
  def shouldReturnProperDistance(a: String, b: String) {
    assertThat(levensteinDistance(a, b), equalTo(0))
  }

  @DataProvider //{ val name = "stringProvider" }
  def stringProvider = Array(v("a", "a"))

  def v(i: String*) = Array(i)
}
