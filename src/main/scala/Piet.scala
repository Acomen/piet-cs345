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
 *
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
  	p.print()

  }
}

/* A codel represents a pixel in a Piet program. Pass in a color,
 * and Codel(Color) determines the hue and lightness, which are used
 * when interpreting a Piet program. Hue and lightness are encoded 
 * separately to make calculations more compact during interpretation
 */
class Codel(val value: Color){
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
}

/* Pass in a 2-d array representing the Piet program, and this does stuff
 * to-do: have it create a 2-d array of codels.
 */
class Program(val arr: Array[Array[Int]], val columns: Int, val rows: Int){
	var codel_arr = Array.ofDim[Codel](columns, rows)

	for(i <- 0 until rows){
		for(j <- 0 until columns){
			codel_arr(i)(j) = arr(i)(j) match {
				case s:Int if s == 0xFFFFC0C0 => new Codel(Light_Red())
				case s:Int if s == 0xFFFF0000 => new Codel(Normal_Red())
				case s:Int if s == 0xFFC00000 => new Codel(Dark_Red())
				case s:Int if s == 0xFFFFFFC0 => new Codel(Light_Yellow())
				case s:Int if s == 0xFFFFFF00 => new Codel(Normal_Yellow())
				case s:Int if s == 0xFFC0C000 => new Codel(Dark_Yellow())
				case s:Int if s == 0xFFC0FFC0 => new Codel(Light_Green())
				case s:Int if s == 0xFF00FF00 => new Codel(Normal_Green())
				case s:Int if s == 0xFF00C000 => new Codel(Dark_Green())
				case s:Int if s == 0xFFC0FFFF => new Codel(Light_Cyan())
				case s:Int if s == 0xFF00FFFF => new Codel(Normal_Cyan())
				case s:Int if s == 0xFF00C0C0 => new Codel(Dark_Cyan())
				case s:Int if s == 0xFFC0C0FF => new Codel(Light_Blue())
				case s:Int if s == 0xFF0000FF => new Codel(Normal_Blue())
				case s:Int if s == 0xFF0000C0 => new Codel(Dark_Blue())
				case s:Int if s == 0xFFFFC0FF => new Codel(Light_Magenta())
				case s:Int if s == 0xFFFF00FF => new Codel(Normal_Magenta())
				case s:Int if s == 0xFFC000C0 => new Codel(Dark_Magenta())
				case s:Int if s == 0xFFFFFFFF => new Codel(White())
				case s:Int if s == 0xFF000000 => new Codel(Black())
			}
		}
	}

	//temporary print function (for testing)
	def print() = {
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

	/* Takes in a value from STDIN and pushes it onto the stack. 
	 */
	def in() = {
		val input = scala.Console.readInt
		stack.push(input)
	}

	/* Pops the top value off of the stack and prints it to STDOUT
	 */
	def out() = {
		val output = stack.pop()
		print(output)
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


