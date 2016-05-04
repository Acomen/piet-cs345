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
  test("Getting the correct codel upon assocation"){
    val c = new Codel(Light_Blue(),0,0)	
    val b = new ColorBlock(c)
    assert(b.get_codel.check_match(c))
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

class ExecuteTest extends FunSuite {
 	test("Light difference 1, Hue difference 0"){
		var arr = Array.ofDim[Int](2,2)
   		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
    		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Normal_Red(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 1)
	}
	test("Light difference 2, Hue difference 0"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Dark_Red(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(1)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 0)
	}
	test("Light difference 0, Hue difference 1"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Light_Yellow(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(1)
		ps.push(2)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 1)
		assert (proc.get_stack.top == 3)
	}
	test("Light difference 1, Hue difference 1"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Normal_Yellow(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(4)
		ps.push(1)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 1)
		assert (proc.get_stack.top == 3)
	}
	test("Light difference 2, Hue difference 1"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Dark_Yellow(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(3)
		ps.push(2)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 1)
		assert (proc.get_stack.top == 6)
	}
	test("Light difference 0, Hue difference 2"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Light_Green(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(4)
		ps.push(2)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 1)
		assert (proc.get_stack.top == 2)
	}
	test("Light difference 1, Hue difference 2"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Normal_Green(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(5)
		ps.push(2)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 1)
		assert (proc.get_stack.top == 1)
	}
	test("Light difference 2, Hue difference 2"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Dark_Green(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(1)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 1)
		assert (proc.get_stack.top == 0)
	}
	test("Light difference 0, Hue difference 3"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Light_Cyan(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(1)
		ps.push(2)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 1)
		assert (proc.get_stack.top == 0)
	}
	test("Light difference 1, Hue difference 3, Update Less than 4"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Normal_Cyan(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(3)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 0)
		assert (proc.get_dp == 3)
	}
	test("Light difference 1, Hue difference 3, Update Greater than 4"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Normal_Cyan(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(7)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 0)
		assert (proc.get_dp == 3)
	}
	test("Light difference 1, Hue difference 3, Update Less than 0"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Normal_Cyan(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(-3)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 0)
		assert (proc.get_dp == 1)
	}
	test("Light difference 1, Hue difference 3, Update Less than -4"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Normal_Cyan(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(-5)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 0)
		assert (proc.get_dp == 3)
	}
	test("Light difference 2, Hue difference 3, Update Less than 2"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Dark_Cyan(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(1)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 0)
		assert (proc.get_cc == 1)
	}
	test("Light difference 2, Hue difference 3, Update Greater than 2"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Dark_Cyan(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(4)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 0)
		assert (proc.get_cc == 0)
	}
	test("Light difference 2, Hue difference 3, Update Less than 0"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Dark_Cyan(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(-5)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 0)
		assert (proc.get_cc == 1)
	}
	test("Light difference 0, Hue difference 4"){
		var arr = Array.ofDim[Int](2,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000 //normal red
		arr(1)(0) = 0xFFC00000 //dark red
    		arr(1)(1) = 0xFF000000 //black
   		val p = new Program(arr, 2, 2)
		val c1 = new Codel(Light_Red(), 0, 0)
    		val c2 = new Codel(Light_Blue(), 1, 0)
    		val b1 = new ColorBlock(c1)
		val b2 = new ColorBlock(c2)
    		val proc = new Processor(p)
		val ps = proc.get_stack
		ps.push(5)
		proc.execute(b1,b2)
		assert (proc.get_stack.length == 2)
		assert (proc.get_stack.top == 5)
	}
}
class ShapeTests extends FunSuite {
        test("find min row with 1 block"){
                val c = new Codel(Light_Red(),0,0)
                val b = new ColorBlock(c)
                assert(b.find_min_row == 0)
        }
        test("find max row with 1 block"){
                val c = new Codel(Light_Red(),0,0)
                val b = new ColorBlock(c)
                assert(b.find_max_row == 0)
        }
        test("find max col with 1 block"){
                val c = new Codel(Light_Red(),1,5)
                val b = new ColorBlock(c)
                assert(b.find_max_col == 5)
        }
        test("find min col with 1 block"){
                val c = new Codel(Light_Red(),1,5)
                val b = new ColorBlock(c)
                assert(b.find_min_col == 5)
        }
        test("find min row with 2 blocks"){
                val c1 = new Codel(Light_Red(),6,5)
		val c2 = new Codel(Light_Red(),7,5)
                val b = new ColorBlock(c1)
		b.append_codel(c2)		
                assert(b.find_min_row == 6)
        }
        test("find min col with 2 blocks"){
                val c1 = new Codel(Light_Red(),7,4)
		val c2 = new Codel(Light_Red(),7,5)
                val b = new ColorBlock(c1)
		b.append_codel(c2)		
                assert(b.find_min_col == 4)
        }
        test("find max row with 2 blocks"){
                val c1 = new Codel(Light_Red(),6,5)
		val c2 = new Codel(Light_Red(),7,5)
                val b = new ColorBlock(c1)
		b.append_codel(c2)		
                assert(b.find_max_row == 7)
        }
        test("find max col with 2 blocks"){
                val c1 = new Codel(Light_Red(),7,4)
		val c2 = new Codel(Light_Red(),7,5)
                val b = new ColorBlock(c1)
		b.append_codel(c2)		
                assert(b.find_max_col == 5)
        }
	test("find min col with 5 blocks"){
                val c1 = new Codel(Light_Red(),5,5)
		val c2 = new Codel(Light_Red(),6,5)
		val c3 = new Codel(Light_Red(),4,5)
		val c4 = new Codel(Light_Red(),5,6)
		val c5 = new Codel(Light_Red(),5,4)
                val b = new ColorBlock(c1)
		b.append_codel(c2)
		b.append_codel(c3)
		b.append_codel(c4)
		b.append_codel(c5)		
                assert(b.find_min_col == 4)
        }
	test("find max col with 5 blocks"){
                val c1 = new Codel(Light_Red(),5,5)
		val c2 = new Codel(Light_Red(),6,5)
		val c3 = new Codel(Light_Red(),4,5)
		val c4 = new Codel(Light_Red(),5,6)
		val c5 = new Codel(Light_Red(),5,4)
                val b = new ColorBlock(c1)
		b.append_codel(c2)
		b.append_codel(c3)
		b.append_codel(c4)
		b.append_codel(c5)		
                assert(b.find_max_col == 6)
        }
	test("find min row with 5 blocks"){
                val c1 = new Codel(Light_Red(),5,5)
		val c2 = new Codel(Light_Red(),6,5)
		val c3 = new Codel(Light_Red(),4,5)
		val c4 = new Codel(Light_Red(),5,6)
		val c5 = new Codel(Light_Red(),5,4)
                val b = new ColorBlock(c1)
		b.append_codel(c2)
		b.append_codel(c3)
		b.append_codel(c4)
		b.append_codel(c5)		
                assert(b.find_min_row == 4)
        }
	test("find max row with 5 blocks"){
                val c1 = new Codel(Light_Red(),5,5)
		val c2 = new Codel(Light_Red(),6,5)
		val c3 = new Codel(Light_Red(),4,5)
		val c4 = new Codel(Light_Red(),5,6)
		val c5 = new Codel(Light_Red(),5,4)
                val b = new ColorBlock(c1)
		b.append_codel(c2)
		b.append_codel(c3)
		b.append_codel(c4)
		b.append_codel(c5)		
                assert(b.find_max_row == 6)
        }
        test("find min row cond with 1 block"){
                val c = new Codel(Light_Red(),0,0)
                val d = new ColorBlock(c)
                assert(d.find_min_row_cond(0) == 0)
        }
        test("find min col cond with 1 block"){
                val c = new Codel(Light_Red(),0,0)
                val b = new ColorBlock(c)
                assert(b.find_min_col_cond(0) == 0)
        }
        test("find max row cond with 1 block"){
                val c = new Codel(Light_Red(),0,0)
                val b = new ColorBlock(c)
                assert(b.find_max_row_cond(0) == 0)
        }
        test("find max col cond with 1 block"){
                val c = new Codel(Light_Red(),0,0)
                val b = new ColorBlock(c)
                assert(b.find_max_col_cond(0) == 0)
        }
	test("find max row cond with 5 blocks, testing col limit"){
                val c1 = new Codel(Light_Red(),1,1)
		val c2 = new Codel(Light_Red(),2,1)
		val c3 = new Codel(Light_Red(),0,1)
		val c4 = new Codel(Light_Red(),1,2)
		val c5 = new Codel(Light_Red(),1,0)
                val b = new ColorBlock(c1)
		b.append_codel(c2)
		b.append_codel(c3)
		b.append_codel(c4)
		b.append_codel(c5)		
                assert(b.find_max_row_cond(0) == 1)
        }
	test("find max col cond with 5 blocks, testing row limit"){
                val c1 = new Codel(Light_Red(),1,1)
		val c2 = new Codel(Light_Red(),2,1)
		val c3 = new Codel(Light_Red(),0,1)
		val c4 = new Codel(Light_Red(),1,2)
		val c5 = new Codel(Light_Red(),1,0)
                val b = new ColorBlock(c1)
		b.append_codel(c2)
		b.append_codel(c3)
		b.append_codel(c4)
		b.append_codel(c5)		
                assert(b.find_max_col_cond(2) == 1)
        }
	test("find min row cond with 5 blocks, testing col limit"){
                val c1 = new Codel(Light_Red(),1,1)
		val c2 = new Codel(Light_Red(),2,1)
		val c3 = new Codel(Light_Red(),0,1)
		val c4 = new Codel(Light_Red(),1,2)
		val c5 = new Codel(Light_Red(),1,0)
                val b = new ColorBlock(c1)
		b.append_codel(c2)
		b.append_codel(c3)
		b.append_codel(c4)
		b.append_codel(c5)		
                assert(b.find_min_row_cond(1) == 0)
        }
	test("find min col cond with 5 blocks, testing row limit"){
                val c1 = new Codel(Light_Red(),1,1)
		val c2 = new Codel(Light_Red(),2,1)
		val c3 = new Codel(Light_Red(),0,1)
		val c4 = new Codel(Light_Red(),1,2)
		val c5 = new Codel(Light_Red(),1,0)
                val b = new ColorBlock(c1)
		b.append_codel(c2)
		b.append_codel(c3)
		b.append_codel(c4)
		b.append_codel(c5)		
                assert(b.find_min_row_cond(0) == 1)
        }
}
class find_next_test extends FunSuite {
	test("find_next with 2 blocks"){
		var arr = Array.ofDim[Int](1,2)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFF0000
		val c = new Codel(Normal_Red(),0,1)
   		val p = new Program(arr, 1, 2)
		val pro = new Processor(p)
		assert(pro.find_next.get_codel.check_match(c))
	}
	test("find_next with 4 blocks"){
		var arr = Array.ofDim[Int](3,3)
		arr(1)(1) = 0xFFFFC0C0 //light red
		arr(1)(2) = 0xFFFF0000
		arr(0)(2) = 0xFFFFFFFF
		arr(2)(1) = 0xFFC00000
		arr(0)(1) = 0xFFC00000
		arr(0)(0) = 0xFFFFFFFF
		arr(1)(0) = 0xFFC00000
		arr(2)(2) = 0xFFFFFFFF
		arr(2)(0) = 0xFFFFFFFF
		val c = new Codel(Normal_Red(),0,1)
   		val p = new Program(arr, 3, 3)
		val pro = new Processor(p)
		pro.current_cb = p.codel_arr(1)(1).block
		assert(pro.find_next.get_codel.check_match(c))
	}
	test("find_next with 4 blocks,1 Black"){
		var arr = Array.ofDim[Int](3,3)
		arr(1)(1) = 0xFFFFC0C0 //light red
		arr(1)(2) = 0xFF000000
		arr(0)(2) = 0xFFFFFFFF
		arr(2)(1) = 0xFFFF0000
		arr(0)(1) = 0xFFC00000
		arr(0)(0) = 0xFFFFFFFF
		arr(1)(0) = 0xFFC00000
		arr(2)(2) = 0xFFFFFFFF
		arr(2)(0) = 0xFFFFFFFF
		val c = new Codel(Normal_Red(),0,1)
   		val p = new Program(arr, 3, 3)
		val pro = new Processor(p)
		pro.current_cb = p.codel_arr(1)(1).block
		assert(pro.find_next.get_codel.check_match(c))
	}
	test("find_next with 4 black blocks"){
		var arr = Array.ofDim[Int](3,3)
		arr(1)(1) = 0xFFFFC0C0 //light red
		arr(1)(2) = 0xFF000000
		arr(0)(2) = 0xFFFFFFFF
		arr(2)(1) = 0xFF000000
		arr(0)(1) = 0xFF000000
		arr(0)(0) = 0xFFFFFFFF
		arr(1)(0) = 0xFF000000
		arr(2)(2) = 0xFFFFFFFF
		arr(2)(0) = 0xFFFFFFFF
		val c = new Codel(Normal_Red(),0,1)
   		val p = new Program(arr, 3, 3)
		val pro = new Processor(p)
		pro.current_cb = p.codel_arr(1)(1).block
		assert(pro.find_next == null)
	}
	test("find_next chain"){
		var arr = Array.ofDim[Int](3,3)
		arr(0)(0) = 0xFF0000FF //Blue
		arr(0)(1) = 0xFF000000 //Black
		arr(0)(2) = 0xFF0000FF //Blue
		arr(1)(0) = 0xFFFF00FF //Norm Magenta
		arr(1)(1) = 0xFFC000C0 //Dark Magenta
		arr(1)(2) = 0xFF0000FF //Blue
		arr(2)(0) = 0xFF0000FF //Blue
		arr(2)(1) = 0xFF000000 //Black
		arr(2)(2) = 0xFF0000FF //Blue
		val c = new Codel(Dark_Magenta(),0,1)
   		val p = new Program(arr, 3, 3)
		val pro = new Processor(p)
		pro.current_cb = 
		var next = pro.find_next
		
	}
/*	test("Run Test"){
		var arr = Array.ofDim[Int](3,3)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFFC0C0
		arr(0)(2) = 0xFFFFC0C0
		arr(1)(0) = 0xFF000000
		arr(1)(1) = 0xFFFFC0C0
		arr(1)(2) = 0xFF000000
		arr(2)(0) = 0xFFFF0000
		arr(2)(1) = 0xFFFF0000
		arr(2)(2) = 0xFFFF0000
		val c = new Codel(Normal_Red(),0,1)
   		val p = new Program(arr, 3, 3)
		val pro = new Processor(p)
		pro.run
		assert(pro.ps.top == 4)
	}
	test("Run Test with output"){
		var arr = Array.ofDim[Int](3,4)
		arr(0)(0) = 0xFFFFC0C0 //light red
		arr(0)(1) = 0xFFFFC0C0
		arr(0)(2) = 0xFFFFC0C0
		arr(0)(3) = 0xFF000000
		arr(1)(0) = 0xFF000000
		arr(1)(1) = 0xFFFF0000
		arr(1)(2) = 0xFFFF0000
		arr(1)(3) = 0xFF000000
		arr(2)(0) = 0xFFC000C0
		arr(2)(1) = 0xFFC000C0
		arr(2)(2) = 0xFFC000C0
		arr(2)(3) = 0xFFC000C0
   		val p = new Program(arr, 3, 4)
		val pro = new Processor(p)
		pro.run
		assert(pro.ps.length == 0)
	}*/
	test("hello world")
	{
		Piet.main(Array())
		assert(true)	
	}
}
