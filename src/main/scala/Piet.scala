package pietcs345
import scala.collection.mutable
import java.io.File
import javax.imageio.ImageIO

/* The different lightness that can exist
 * in a Piet program
 */
sealed trait Lightness
case class Light() extends Lightness
case class Normal() extends Lightness
case class Dark() extends Lightness

/* The 6 different hues that appear
 * in a piet program
 */
sealed trait Hue
case class Red() extends Hue
case class Yellow() extends Hue
case class Green() extends Hue
case class Cyan() extends Hue
case class Blue() extends Hue
case class Magenta() extends Hue

/* The 18 colors in a Piet Program
 */
sealed trait Color
case class Light_Red() extends Color
case class Normal_Red() extends Color
case class Dark_Red() extends Color
case class Light_Yellow() extends Color
case class Normal_Yellow() extends Color
case class Dark_Yellow() extends Color
case class Light_Green() extends Color
case class Normal_Green() extends Color
case class Dark_Green() extends Color
case class Light_Cyan() extends Color
case class Normal_Cyan() extends Color
case class Dark_Cyan() extends Color
case class Light_Blue() extends Color
case class Normal_Blue() extends Color
case class Dark_Blue() extends Color
case class Light_Magenta() extends Color
case class Normal_Magenta() extends Color
case class Dark_Magenta() extends Color
case class Black() extends Color
case class White() extends Color

object Piet {
  def main(args: Array[String]) {
  	val photo = ImageIO.read(new File("src/main/resources/piet_hello_2.gif"))
  	var arr = Array.ofDim[Int](photo.getWidth, photo.getHeight)
  	for(i <- 0 until photo.getHeight){
  		for(j <- 0 until photo.getWidth){
  			arr(i)(j) = photo.getRGB(j, i)
  		}
  	}

  	val p = new Program(arr, photo.getWidth, photo.getHeight)
  	p.print_colorblock()

  }
}

/* A codel represents a pixel in a Piet program. Pass in a color,
 * and Codel(Color) determines the hue and lightness, which are used
 * when interpreting a Piet program. Hue and lightness are encoded 
 * separately to make calculations more compact during interpretation
 */
class Codel(val value: Color, val row_val : Int, val col_val : Int){
	val row = row_val
	val col = col_val
	var block : ColorBlock = null

	val lightness = value match {
		case s:Color if s == Light_Red() || s == Light_Yellow() || 
						s == Light_Green() || s == Light_Cyan() || 
						s == Light_Blue() || s == Light_Magenta() => Light()
		case s:Color if s == Normal_Red() || s == Normal_Yellow() || 
						s == Normal_Green() || s == Normal_Cyan() || 
						s == Normal_Blue() || s == Normal_Magenta() => Normal()
		case s:Color if s == Dark_Red() || s == Dark_Yellow() || 
						s == Dark_Green() || s == Dark_Cyan() || 
						s == Dark_Blue() || s == Dark_Magenta() => Dark()
		case s:Color if s == Black() || s == White() => null
	}

	val hue = value match {
		case s:Color if s == Light_Red() || s == Normal_Red() ||
						s == Dark_Red() => Red()
		case s:Color if s == Light_Yellow() || s == Normal_Yellow() ||
						s == Dark_Yellow() => Yellow()
		case s:Color if s == Light_Green() || s == Normal_Green() ||
						s == Dark_Green() => Green()
		case s:Color if s == Light_Cyan() || s == Normal_Cyan() ||
						s == Dark_Cyan() => Cyan()
		case s:Color if s == Light_Blue() || s == Normal_Blue() ||
						s == Dark_Blue() => Blue()
		case s:Color if s == Light_Magenta() || s == Normal_Magenta() ||
						s == Dark_Magenta() => Magenta()
		case s:Color if s == Black() || s == White() => null
	}

	val color = value

	def set_block(parent : ColorBlock) {
		block = parent
	}

	def check_match(other : Codel) : Boolean = {
		return color == other.color
	}

	/* Returns the difference in the lightness between two codels
	 * The lightness cycle is: Light -> Normal -> Dark -> Light
	 */
	def get_lightness_difference(other: Codel) : Int = {
		val light_cycle = Array[Lightness](Light(), Normal(), Dark())

		return get_difference(lightness, other.lightness, light_cycle)
	}

	/* Returns the difference in the hue between two codels
	 * The hue cycle is: Red -> Yellow -> Green -> Cyan -> Blue -> Magenta -> Red
	 */
	def get_hue_difference(other: Codel) : Int = {
		val hue_cycle = Array[Hue](Red(), Yellow(), Green(), Cyan(), Blue(), Magenta())

		return get_difference(hue, other.hue, hue_cycle)
	}

	/* Generic function to return the difference between two values given
	 * a cycle. 
	 */
	def get_difference[T](value : T, other_value: T, cycle : Array[T]) : Int = {
		var index = 0
		var matched = false
		while(!matched){
			if(value == cycle(index))
				matched = true
			else
				index = index + 1
		}

		var count = 0
		matched = false
		while(!matched){
			if(index >= cycle.length)
				index = 0

			if(other_value == cycle(index))
				matched = true
			else
				count = count+1

			index = index + 1
		}

		return count
	}
}

/* Color blocks are a group of contiguous codels of the same color
 * They keep track of all contiguous codels, since the interpreter
 * depends on color blocks
 * TODO Find color block corners
 */
class ColorBlock(val seed : Codel){
	var codels = Array[Codel](seed)
	seed.set_block(this)

	def append_codel(x : Codel){
		x.set_block(this)
		codels = codels :+ x
	}

	def unify(other : ColorBlock){
		for(i <- 0 until other.codels.length){
			other.codels(i).set_block(this)
			codels = codels :+ other.codels(i)
		}
	}
	def get_size() : Int={
		return codels.length;
	}
	def get_codel() : Codel = {
		return codels(0)
	}
}

/* Pass in a 2-d array representing the Piet program, and this does stuff
 * to-do: have it create a 2-d array of codels.
 */
class Program(val arr: Array[Array[Int]], val columns: Int, val rows: Int){
	var codel_arr = Array.ofDim[Codel](columns, rows)

	// initialize codels
	for(i <- 0 until rows){
		for(j <- 0 until columns){
			codel_arr(i)(j) = arr(i)(j) match {
				case s:Int if s == 0xFFFFC0C0 => new Codel(Light_Red(), i, j)
				case s:Int if s == 0xFFFF0000 => new Codel(Normal_Red(), i, j)
				case s:Int if s == 0xFFC00000 => new Codel(Dark_Red(), i, j)
				case s:Int if s == 0xFFFFFFC0 => new Codel(Light_Yellow(), i, j)
				case s:Int if s == 0xFFFFFF00 => new Codel(Normal_Yellow(), i, j)
				case s:Int if s == 0xFFC0C000 => new Codel(Dark_Yellow(), i, j)
				case s:Int if s == 0xFFC0FFC0 => new Codel(Light_Green(), i, j)
				case s:Int if s == 0xFF00FF00 => new Codel(Normal_Green(), i, j)
				case s:Int if s == 0xFF00C000 => new Codel(Dark_Green(), i, j)
				case s:Int if s == 0xFFC0FFFF => new Codel(Light_Cyan(), i, j)
				case s:Int if s == 0xFF00FFFF => new Codel(Normal_Cyan(), i, j)
				case s:Int if s == 0xFF00C0C0 => new Codel(Dark_Cyan(), i, j)
				case s:Int if s == 0xFFC0C0FF => new Codel(Light_Blue(), i, j)
				case s:Int if s == 0xFF0000FF => new Codel(Normal_Blue(), i, j)
				case s:Int if s == 0xFF0000C0 => new Codel(Dark_Blue(), i, j)
				case s:Int if s == 0xFFFFC0FF => new Codel(Light_Magenta(), i, j)
				case s:Int if s == 0xFFFF00FF => new Codel(Normal_Magenta(), i, j)
				case s:Int if s == 0xFFC000C0 => new Codel(Dark_Magenta(), i, j)
				case s:Int if s == 0xFFFFFFFF => new Codel(White(), i, j)
				case s:Int if s == 0xFF000000 => new Codel(Black(), i, j)
			}
		}
	}

	// initialize colorblocks
	for(i <- 0 until rows){
		for(j <- 0 until columns){
			var appended = false
			if(i - 1 >= 0){
				if(codel_arr(i-1)(j).check_match(codel_arr(i)(j))){
					codel_arr(i-1)(j).block.append_codel(codel_arr(i)(j))
					appended = true
				}
			}

			if(j - 1 >= 0 && !appended){
	
	/*returns the prgram stack
	 *Used for testing
	 */			if(codel_arr(i)(j-1).check_match(codel_arr(i)(j))){
					codel_arr(i)(j-1).block.append_codel(codel_arr(i)(j))
					appended = true
				}
			}

			if(!appended){
				val block = new ColorBlock(codel_arr(i)(j))
			}
		}
	}			

	//first colorblock pass through doesn't work properly, this unifies them.
	//hacky!!! could probably do everything in one pass through, but this works for now
	for(i <- 0 until rows){
		for(j <- 0 until columns){
			val current = codel_arr(i)(j)
			if(i - 1 >= 0){
				val tmp = codel_arr(i-1)(j)
				if(tmp.check_match(current) && tmp.block != current.block){
					current.block.unify(tmp.block)
				}
			}
			if(j - 1 >= 0){
				val tmp = codel_arr(i)(j-1)
				if(tmp.check_match(current) && tmp.block != current.block){
					current.block.unify(tmp.block)
				}
			}
			if(i + 1 < rows){
				val tmp = codel_arr(i+1)(j)
				if(tmp.check_match(current) && tmp.block != current.block){
					current.block.unify(tmp.block)
				}
			}
			if(j + 1 < columns){
				val tmp = codel_arr(i)(j+1)
				if(tmp.check_match(current) && tmp.block != current.block){
					current.block.unify(tmp.block)
				}
			}
		}
	}	

	//temporary print function (for testing)
	def print_codel() = {
	  	var s = ""
	  	for(i <- 0 until rows){
	  		s = s + "["
	  		for(j <- 0 until columns){
	  			var light = codel_arr(i)(j) match {
		  			case s:Codel if s.lightness == Light() => "Light "
		  			case s:Codel if s.lightness == Normal() => "Normal "
		  			case s:Codel if s.lightness == Dark() => "Dark "
		  			case s:Codel if s.lightness == null => ""
	  			}
	  			var hue = codel_arr(i)(j) match {
		  			case s:Codel if s.hue == Red() => "Red"
		  			case s:Codel if s.hue == Yellow() => "Yellow"
		  			case s:Codel if s.hue == Green() => "Green"
		  			case s:Codel if s.hue == Cyan() => "Cyan"
		  			case s:Codel if s.hue == Blue() => "Blue"
		  			case s:Codel if s.hue == Magenta() => "Magenta"
		  			case s:Codel if s.hue == null => ""
	  			}
	  			var color = codel_arr(i)(j) match {
	  				case s:Codel if s.hue == null && s.color == Black() => "Black"
	  				case s:Codel if s.hue == null && s.color == White() => "White"
	  				case s:Codel if s.hue != null => ""
	  			}
	  			s = s + light + hue + color + ","
	  		}
	  		s = s + "]\n"
	  	}
	  	println(s)
	}

	def print_colorblock() = {
	  	var s = ""
	  	for(i <- 0 until rows){
	  		s = s + "["
	  		for(j <- 0 until columns){
	  			val padded = f"${codel_arr(i)(j).block.codels.length}% 3d"
	  			s = s + padded  + ","
	  		}
	  		s = s + "]\n"
	  	}
	  	println(s)
	}
}

/*	Functions as the stack used in evaluating a piet program.
 *	Various commands can be called on the stack, based on how the
 *	piet image is interpreted. 
 *
 *	stack: internal container (a mutable Stack[Int]), that is used as 
 *		the underlying container of the ProgramStack  
 */
class ProgramStack{
	var stack = new mutable.Stack[Int]()

	/* Pushes a value to the stack 
	 */
	def push(x: Int) = {
		stack.push(x)
	}

	/* Pops the top value off the stack 
	 */ 
	def pop() : Int = {
		val item = stack.pop()
		return item
	}

	/* Pops the top two values off the stack, adds them,
	 * and then pushes the result back onto the stack
	 */
	def add() = {
		val first = stack.pop()
		val second = stack.pop()
		stack.push(second + first)
	}

	/* Pops the top two values off the stack, subtracts the
	 * first value from the second value and then pushes the
	 * result back onto the stack
	 */
	def subtract() = {
		val first = stack.pop()
		val second = stack.pop()
		stack.push(second - first)
	}

	/* Pops the top two values off the stack, multiplies them,
	 * and then pushes the result back onto the stack
	 */
	def multiply() = {
		val first = stack.pop()
		val second = stack.pop()
		stack.push(second * first)
	}

	/* Pops the top two values off the stack, divides the second value
	 * by the first value, and then puhses the result back onto the stack
	 */
	def divide() = {
		val first = stack.pop()
		val second = stack.pop()
		stack.push(second / first)
	}

	/* Pops the top two values off the stack, calculates the second value
	 * modulo the first value, and pushes the result back on the stack.
	 * Note: return the 'least positive residue'
	 */
	def mod() = {
		val first = stack.pop()
		val second = stack.pop()
		var mod = second % first
		if(mod < 0){
			mod += first
		}
		stack.push(mod)
	}

	/* If the top value of the stack is non-zero, replace it zero,
	 * otherwise replace it with 1.
	 */
	def not() = {
		val top = stack.pop()
		if(top == 0){
			stack.push(1)
		}else{
			stack.push(0)
		}
	}

	/* Pops the top two values off the stock. If the second value is greater
	 * than the first value, push 1 onto the stack, otherwise push 0.
	 */
	def greater() = {
		val first = stack.pop()
		val second = stack.pop()
		if(second > first){
			stack.push(1)
		}else{
			stack.push(0)
		}
	}

	/* Pushes a copy of the top value onto the stack.
	 */
	def duplicate() = {
		val top = stack.top
		stack.push(top)
	}

	/* Pops the top two values off the stack, and rolls the stack to a depth
	 * equal to the second value, by a number of rolls equal to the first value.
	 * 
	 * If the first value is negative, roll in the opposite direction
	 */
	def roll() = {
		var rolls = stack.pop()
		val depth = stack.pop()

		var i = 0

		if(rolls > 0){
			for (i <- 0 until rolls){
				val split = stack.splitAt(depth)
				val top = split._1.pop()
				val bottom_half = split._2.push(top)
				stack = split._1 ++ bottom_half
			}
		}else{
			rolls = rolls * -1
			for (i <- 0 until rolls){
				val split = stack.splitAt(depth - 1)
				val top = split._2.pop()
				val top_half = split._1.push(top)
				stack = top_half ++ split._2
			}			
		}
	}

	/* Takes in an int value from STDIN and pushes it onto the stack. 
	 */
	def inint() = {
		val input = scala.Console.readInt
		stack.push(input)
	}

	/* Take in a char value from STDIN and pushes it onto the stack
	*/
	def inchar()= {
		val input = scala.Console.readChar
		stack.push(input.toInt)	
	}

	/* Pops the top value off of the stack and prints it to STDOUT
	 */
	def outint() = {
		val output = stack.pop()
		print(output)
	}
	
	/* Pops the top value off of the stack and print it to STDOUT as a char
	 */ 
	def outchar() = {
		val output = stack.pop()
		print(output.toChar)
	}


	/* Returns the top value on the stack
	 */
	def top() : Int = {
		return stack.top
	}

	/* Returns the number of elements in the stack
	 */
	def length() : Int = {
		return stack.length
	}
}

/* An class which process an instance of a program
 * The Direction pointer is represented by the following numbers
 * 0 = Right
 * 1 = Down
 * 2 = Left
 * 3 = Up
 * The value of the Codel Chooser is represented by the following numbers
 * 0 = Left
 * 1 = Right
 */
class Processor(p:Program){
	var CC = 0
	var DP = 0
	var ps = new ProgramStack

	/*Excutes a command between two Colorblocks
	 *
	 */
	 def execute( cb1:ColorBlock, cb2:ColorBlock)= {
		val lightdif = cb1.get_codel.get_lightness_difference(cb2.get_codel)
		val huedif = cb1.get_codel.get_hue_difference(cb2.get_codel)
		huedif match {
			case 0 => lightdif match {
					case 1 => ps.push(cb2.get_size)
					case 2 => ps.pop
				}
			case 1 => lightdif match {
					case 0 => ps.add
					case 1 => ps.subtract
					case 2 => ps.multiply
				}
			case 2 => lightdif match {
					case 0 => ps.divide
					case 1 => ps.mod
					case 2 => ps.not
				}
			case 3 => lightdif match {
					case 0 => ps.greater
					case 1 => { DP = (DP + ps.pop)%4
						  if(DP < 0){
							DP= 4+DP
					          }}
					case 2 => CC = (CC + scala.math.abs(ps.pop))%2
				}
			case 4 => lightdif match {
					case 0 => ps.duplicate
					case 1 => ps.roll
					case 2 => ps.inint
				}
			case 5 => lightdif match {
					case 0 => ps.inchar
					case 1 => ps.outint
					case 2 => ps.outchar
				}
		}
	}
	/*returns the prgram stack
	 *Used for testing
	 */
	def get_stack: ProgramStack = {
		return ps
	}
	
	/*Returns the Direction Pointer
	 *Used for testing
	 */
	def get_dp : Int = {
		return DP
	}
	/*Returns the Codel Chooser
	 *Used for testing
	 */
	def get_cc : Int = {
		return CC
	}	 
}
