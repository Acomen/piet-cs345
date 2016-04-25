package pietcs345
import org.scalatest.FunSuite
 
class StackTest extends FunSuite {
 
  test("Stack length") {
  	val s = new ProgramStack()
    assert(s.length() == 0)
  	s.push(5)
    assert(s.length() == 1)
  }

  test("Stack push") {
  	val s = new ProgramStack()
  	s.push(5)
    assert(s.top() == 5)
    s.push(10)
    assert(s.top() == 10)
  }

  test("Stack pop") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    assert(s.length() == 2)
    var tmp = s.pop()
    assert(tmp == 10)
   	assert(s.length() == 1)
   	tmp = s.pop()
   	assert(tmp == 5)
   	assert(s.length() == 0)
  }

  test("Stack add") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(1)
    assert(s.length() == 3)
    s.add()
   	assert(s.length() == 2)
   	assert(s.top() == 11)
  }

  test("Stack subtract") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(1)
    assert(s.length() == 3)
    s.subtract()
   	assert(s.length() == 2)
   	assert(s.top() == 9)
  }

  test("Stack multiply") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(2)
    assert(s.length() == 3)
    s.multiply()
   	assert(s.length() == 2)
   	assert(s.top() == 20)
  }

  test("Stack divide") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(2)
    assert(s.length() == 3)
    s.divide()
   	assert(s.length() == 2)
   	assert(s.top() == 5)
  }

  test("Stack mod") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(3)
    assert(s.length() == 3)
    s.mod()
   	assert(s.length() == 2)
   	assert(s.top() == 1)
  }

  test("Stack mod negative") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(-10)
    s.push(3)
    assert(s.length() == 3)
    s.mod()
   	assert(s.length() == 2)
   	assert(s.top() == 2)
  }

  test("Stack not (zero)") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(0)
    assert(s.length() == 3)
    s.not()
   	assert(s.length() == 3)
   	assert(s.top() == 1)
  }

  test("Stack not (non-zero)") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(3)
    assert(s.length() == 3)
    s.not()
   	assert(s.length() == 3)
   	assert(s.top() == 0)
  }

  test("Stack greater (is greater)") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(3)
    assert(s.length() == 3)
    s.greater()
   	assert(s.length() == 2)
   	assert(s.top() == 1)
  }

  test("Stack greater (is not greater)") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(1)
    s.push(3)
    assert(s.length() == 3)
    s.greater()
   	assert(s.length() == 2)
   	assert(s.top() == 0)
  }

  test("Stack duplicate") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    assert(s.length() == 2)
    s.duplicate()
   	assert(s.length() == 3)
   	assert(s.top() == 10)
  }

  test("Stack roll") {
  	val s = new ProgramStack()
  	s.push(12)
    s.push(20)
    s.push(15)
    s.push(10)
    s.push(5)
    s.push(3)
    s.push(2)
    s.roll()
   	assert(s.length() == 5)
   	assert(s.pop() == 15)
   	assert(s.pop() == 5)
   	assert(s.pop() == 10)
   	assert(s.pop() == 20)
   	assert(s.pop() == 12)
  }

  test("Stack roll (negative)") {
  	val s = new ProgramStack()
  	s.push(12)
    s.push(20)
    s.push(15)
    s.push(10)
    s.push(5)
    s.push(3)
    s.push(-2)
    s.roll()
   	assert(s.length() == 5)
   	assert(s.pop() == 10)
   	assert(s.pop() == 15)
   	assert(s.pop() == 5)
   	assert(s.pop() == 20)
   	assert(s.pop() == 12)

  }


}