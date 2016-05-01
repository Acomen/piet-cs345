package pietcs345
import org.scalatest.FunSuite

class CodelTest extends FunSuite{
  test("Testing light red"){
    val c = new Codel(Light_Red(), 0, 0)
    assert(c.hue == Red())
    assert(c.lightness == Light())
  }

  test("Testing normal red"){
    val c = new Codel(Normal_Red(), 0, 0)
    assert(c.hue == Red())
    assert(c.lightness == Normal())
  }

  test("Testing dark red"){
    val c = new Codel(Dark_Red(), 0, 0)
    assert(c.hue == Red())
    assert(c.lightness == Dark())
  }

  test("Testing dark cyan"){
    val c = new Codel(Dark_Cyan(), 0, 0)
    assert(c.hue == Cyan())
    assert(c.lightness == Dark())
  }

  test("Testing light yellow"){
    val c = new Codel(Light_Yellow(), 0, 0)
    assert(c.hue == Yellow())
    assert(c.lightness == Light())
  }

  test("Testing dark blue"){
    val c = new Codel(Dark_Blue(), 0, 0)
    assert(c.hue == Blue())
    assert(c.lightness == Dark())
  }

  test("Testing normal magenta"){
    val c = new Codel(Normal_Magenta(), 0, 0)
    assert(c.hue == Magenta())
    assert(c.lightness == Normal())
  }

  test("Testing light green"){
    val c = new Codel(Light_Green(), 0, 0)
    assert(c.hue == Green())
    assert(c.lightness == Light())
  }


  test("Testing black"){
    val c = new Codel(Black(), 0, 0)
    assert(c.lightness == null)
    assert(c.hue == null)
    assert(c.color == Black())
  }

  test("Testing white"){
    val c = new Codel(White(), 0, 0)
    assert(c.lightness == null)
    assert(c.hue == null)
    assert(c.color == White())
  }

  test("Lightness difference between light and dark"){
    val c1 = new Codel(Light_Blue(), 0, 0)
    val c2 = new Codel(Dark_Red(), 0, 0)
    assert(c1.get_lightness_difference(c2) == 2)
  }

  test("Lightness difference between normal and dark"){
    val c1 = new Codel(Normal_Blue(), 0, 0)
    val c2 = new Codel(Dark_Magenta(), 0, 0)
    assert(c1.get_lightness_difference(c2) == 1)
  }

  test("Lightness difference between normal and light"){
    val c1 = new Codel(Normal_Blue(), 0, 0)
    val c2 = new Codel(Light_Green(), 0, 0)
    assert(c1.get_lightness_difference(c2) == 2)
  }

  test("Lightness difference between dark and light"){
    val c1 = new Codel(Dark_Blue(), 0, 0)
    val c2 = new Codel(Light_Cyan(), 0, 0)
    assert(c1.get_lightness_difference(c2) == 1)
  }

  test("Lightness difference between dark and normal"){
    val c1 = new Codel(Dark_Blue(), 0, 0)
    val c2 = new Codel(Normal_Yellow(), 0, 0)
    assert(c1.get_lightness_difference(c2) == 2)
  }

  test("Lightness difference between normal and normal"){
    val c1 = new Codel(Normal_Blue(), 0, 0)
    val c2 = new Codel(Normal_Blue(), 0, 0)
    assert(c1.get_lightness_difference(c2) == 0)
  }

  test("Lightness difference between light and light"){
    val c1 = new Codel(Light_Blue(), 0, 0)
    val c2 = new Codel(Light_Green(), 0, 0)
    assert(c1.get_lightness_difference(c2) == 0)
  }

  test("Lightness difference between dark and dark"){
    val c1 = new Codel(Dark_Blue(), 0, 0)
    val c2 = new Codel(Dark_Red(), 0, 0)
    assert(c1.get_lightness_difference(c2) == 0)
  }

  test("Hue differnce between red and yellow"){
    val c1 = new Codel(Light_Red(), 0, 0)
    val c2 = new Codel(Normal_Yellow(), 0, 0)
    assert(c1.get_hue_difference(c2) == 1)
  }

  test("Hue differnce between red and cyan"){
    val c1 = new Codel(Light_Red(), 0, 0)
    val c2 = new Codel(Normal_Cyan(), 0, 0)
    assert(c1.get_hue_difference(c2) == 3)
  }

  test("Hue differnce between red and magenta"){
    val c1 = new Codel(Light_Red(), 0, 0)
    val c2 = new Codel(Normal_Magenta(), 0, 0)
    assert(c1.get_hue_difference(c2) == 5)
  }

  test("Hue differnce between magenta and red"){
    val c1 = new Codel(Light_Magenta(), 0, 0)
    val c2 = new Codel(Normal_Red(), 0, 0)
    assert(c1.get_hue_difference(c2) == 1)
  }

  test("Hue differnce between green and yellow"){
    val c1 = new Codel(Dark_Green(), 0, 0)
    val c2 = new Codel(Light_Yellow(), 0, 0)
    assert(c1.get_hue_difference(c2) == 5)
  }

  test("Matching identical"){
    val c1 = new Codel(Dark_Green(), 0, 0)
    val c2 = new Codel(Dark_Green(), 0, 0)
    assert(c1.check_match(c2))
  }

  test("Matching different hues"){
    val c1 = new Codel(Dark_Red(), 0, 0)
    val c2 = new Codel(Dark_Green(), 0, 0)
    assert(!c1.check_match(c2))
  }

  test("Matching different lightness"){
    val c1 = new Codel(Light_Green(), 0, 0)
    val c2 = new Codel(Dark_Green(), 0, 0)
    assert(!c1.check_match(c2))
  }

  test("Matching completely different"){
    val c1 = new Codel(Light_Red(), 0, 0)
    val c2 = new Codel(Dark_Green(), 0, 0)
    assert(!c1.check_match(c2))
  }
}

class ColorBlockTest extends FunSuite {
  test("Creating a color block"){
    val c = new Codel(Dark_Green(), 0, 0)
    val b = new ColorBlock(c)
    assert(b.codels.length == 1)
  }

  test("Adding a codel to a color block"){
    val c1 = new Codel(Light_Blue(), 0, 0)
    val c2 = new Codel(Light_Blue(), 1, 0)
    val b = new ColorBlock(c1)
    b.append_codel(c2)
    assert(b.codels.length == 2)
  }

  test("Associating block with codel on creation"){
    val c = new Codel(Light_Blue(), 0, 0)
    val b = new ColorBlock(c)
    assert(c.block == b)
  }

  test("Associating block with codel on append"){
    val c1 = new Codel(Light_Blue(), 0, 0)
    val c2 = new Codel(Light_Blue(), 1, 0)
    val b = new ColorBlock(c1)
    b.append_codel(c2)
    assert(c1.block == b)
    assert(c2.block == b)
  }
}

class ProgramTest extends FunSuite {

  test("Simple program test"){
    var arr = Array.ofDim[Int](2,2)
    arr(0)(0) = 0xFFFFC0C0 //light red
    arr(0)(1) = 0xFFFF0000 //normal red
    arr(1)(0) = 0xFFC00000 //dark red
    arr(1)(1) = 0xFF000000 //black
    val p = new Program(arr, 2, 2)
    assert(p.codel_arr(0)(0).lightness == Light())
    assert(p.codel_arr(0)(0).hue == Red())
    assert(p.codel_arr(0)(1).lightness == Normal())
    assert(p.codel_arr(0)(1).hue == Red())
    assert(p.codel_arr(1)(0).lightness == Dark())
    assert(p.codel_arr(1)(0).hue == Red())
    assert(p.codel_arr(1)(1).lightness == null)
    assert(p.codel_arr(1)(1).hue == null)
    assert(p.codel_arr(1)(1).color == Black())
  }

  test("Color block test, non contiguous"){
    var arr = Array.ofDim[Int](2,2)
    arr(0)(0) = 0xFFFFC0C0 //light red
    arr(0)(1) = 0xFFFF0000 //normal red
    arr(1)(0) = 0xFFC00000 //dark red
    arr(1)(1) = 0xFF000000 //black
    val p = new Program(arr, 2, 2)
    assert(p.codel_arr(0)(0).block.codels.length == 1)
    assert(p.codel_arr(0)(1).block.codels.length == 1)
    assert(p.codel_arr(1)(0).block.codels.length == 1)
    assert(p.codel_arr(1)(1).block.codels.length == 1)
  }

  test("Color block test, three blocks"){
    var arr = Array.ofDim[Int](2,2)
    arr(0)(0) = 0xFFFFC0C0 
    arr(0)(1) = 0xFFFFC0C0 
    arr(1)(0) = 0xFFC00000 
    arr(1)(1) = 0xFF000000 
    val p = new Program(arr, 2, 2)
    assert(p.codel_arr(0)(0).block.codels.length == 2)
    assert(p.codel_arr(0)(1).block.codels.length == 2)
    assert(p.codel_arr(1)(0).block.codels.length == 1)
    assert(p.codel_arr(1)(1).block.codels.length == 1)
  }

  test("Color block test, four blocks"){
    var arr = Array.ofDim[Int](2,2)
    arr(0)(0) = 0xFFFFC0C0  
    arr(0)(1) = 0xFF000000  
    arr(1)(0) = 0xFFC00000 
    arr(1)(1) = 0xFFFFC0C0 
    val p = new Program(arr, 2, 2)
    assert(p.codel_arr(0)(0).block.codels.length == 1)
    assert(p.codel_arr(0)(1).block.codels.length == 1)
    assert(p.codel_arr(1)(0).block.codels.length == 1)
    assert(p.codel_arr(1)(1).block.codels.length == 1)
  }

  test("Color block test, two blocks"){
    var arr = Array.ofDim[Int](2,2)
    arr(0)(0) = 0xFFFFC0C0 
    arr(0)(1) = 0xFFFFC0C0 
    arr(1)(0) = 0xFFC00000 
    arr(1)(1) = 0xFFC00000 
    val p = new Program(arr, 2, 2)
    assert(p.codel_arr(0)(0).block.codels.length == 2)
    assert(p.codel_arr(0)(1).block.codels.length == 2)
    assert(p.codel_arr(1)(0).block.codels.length == 2)
    assert(p.codel_arr(1)(1).block.codels.length == 2)
  }

  test("Color block test, two blocks (2)"){
    var arr = Array.ofDim[Int](2,2)
    arr(0)(0) = 0xFFFFC0C0 
    arr(0)(1) = 0xFFFFC0C0 
    arr(1)(0) = 0xFFC00000 
    arr(1)(1) = 0xFFFFC0C0 
    val p = new Program(arr, 2, 2)
    assert(p.codel_arr(0)(0).block.codels.length == 3)
    assert(p.codel_arr(0)(1).block.codels.length == 3)
    assert(p.codel_arr(1)(0).block.codels.length == 1)
    assert(p.codel_arr(1)(1).block.codels.length == 3)
  }

  test("Color block test, one block "){
    var arr = Array.ofDim[Int](2,2)
    arr(0)(0) = 0xFFFFC0C0 
    arr(0)(1) = 0xFFFFC0C0 
    arr(1)(0) = 0xFFFFC0C0 
    arr(1)(1) = 0xFFFFC0C0
    val p = new Program(arr, 2, 2)
    assert(p.codel_arr(0)(0).block.codels.length == 4)
    assert(p.codel_arr(0)(1).block.codels.length == 4)
    assert(p.codel_arr(1)(0).block.codels.length == 4)
    assert(p.codel_arr(1)(1).block.codels.length == 4)
  }
}

class StackTest extends FunSuite {
 
  test("Stack length()") {
  	val s = new ProgramStack()
    assert(s.length() == 0)
  	s.push(5)
    assert(s.length() == 1)
  }

  test("Stack push(value : Int)") {
  	val s = new ProgramStack()
  	s.push(5)
    assert(s.top() == 5)
    s.push(10)
    assert(s.top() == 10)
  }

  test("Stack pop()") {
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

  test("Stack add()") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(1)
    assert(s.length() == 3)
    s.add()
   	assert(s.length() == 2)
   	assert(s.top() == 11)
  }

  test("Stack subtract()") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(1)
    assert(s.length() == 3)
    s.subtract()
   	assert(s.length() == 2)
   	assert(s.top() == 9)
  }

  test("Stack multiply()") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(2)
    assert(s.length() == 3)
    s.multiply()
   	assert(s.length() == 2)
   	assert(s.top() == 20)
  }

  test("Stack divide()") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(2)
    assert(s.length() == 3)
    s.divide()
   	assert(s.length() == 2)
   	assert(s.top() == 5)
  }

  test("Stack mod()") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(3)
    assert(s.length() == 3)
    s.mod()
   	assert(s.length() == 2)
   	assert(s.top() == 1)
  }

  test("Stack mod() negative") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(-10)
    s.push(3)
    assert(s.length() == 3)
    s.mod()
   	assert(s.length() == 2)
   	assert(s.top() == 2)
  }

  test("Stack not() (zero)") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(0)
    assert(s.length() == 3)
    s.not()
   	assert(s.length() == 3)
   	assert(s.top() == 1)
  }

  test("Stack not() (non-zero)") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(3)
    assert(s.length() == 3)
    s.not()
   	assert(s.length() == 3)
   	assert(s.top() == 0)
  }

  test("Stack greater() (is greater)") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    s.push(3)
    assert(s.length() == 3)
    s.greater()
   	assert(s.length() == 2)
   	assert(s.top() == 1)
  }

  test("Stack greater() (is not greater)") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(1)
    s.push(3)
    assert(s.length() == 3)
    s.greater()
   	assert(s.length() == 2)
   	assert(s.top() == 0)
  }

  test("Stack duplicate()") {
  	val s = new ProgramStack()
  	s.push(5)
    s.push(10)
    assert(s.length() == 2)
    s.duplicate()
   	assert(s.length() == 3)
   	assert(s.top() == 10)
  }

  test("Stack roll()") {
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

  test("Stack roll() (negative)") {
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