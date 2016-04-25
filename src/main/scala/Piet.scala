package pietcs345
import scala.collection.mutable

/*
 *	Functions as the stack used in evaluating a piet program.
 *	Various commands can be called on the stack, based on how the
 *	piet image is interpreted. 
 *
 *	stack: internal container (a mutable Stack[Int]), that is used as 
 *		the underlying container of the ProgramStack  
 */
class ProgramStack{
	var stack = new mutable.Stack[Int]()

	/* 
	 *	Pushes a value to the stack
	 */
	def push(x: Int) = {
		stack.push(x)
	}

	/*
	 * Pops the top value off the stack
	 */ 
	def pop() : Int = {
		val item = stack.pop()
		return item
	}

	/*
	 * Pops the top two values off the stack, adds them,
	 * and then pushes the result back onto the stack
	 */
	def add() = {
		val first = stack.pop()
		val second = stack.pop()
		stack.push(second + first)
	}

	/*
	 * Pops the top two values off the stack, subtracts the
	 * first value from the second value and then pushes the
	 * result back onto the stack
	 */
	def subtract() = {
		val first = stack.pop()
		val second = stack.pop()
		stack.push(second - first)
	}

	/*
	 * Pops the top two values off the stack, multiplies them,
	 * and then pushes the result back onto the stack
	 */
	def multiply() = {
		val first = stack.pop()
		val second = stack.pop()
		stack.push(second * first)
	}

	/*
	 * Pops the top two values off the stack, divides the second value
	 * by the first value, and then puhses the result back onto the stack
	 */
	def divide() = {
		val first = stack.pop()
		val second = stack.pop()
		stack.push(second / first)
	}

	/*
	 * Pops the top two values off the stack, calculates the second value
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

	/*
	 * If the top value of the stack is non-zero, replace it zero,
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

	/*
	 * Pops the top two values off the stock. If the second value is greater
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

	/*
	 * Pushes a copy of the top value onto the stack.
	 */
	def duplicate() = {
		val top = stack.top
		stack.push(top)
	}

	/*
	 * Pops the top two values off the stack, and rolls the stack to a depth
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

	/*
	 * Takes in a value from STDIN and pushes it onto the stack. 
	 */
	def in() = {
		val input = scala.Console.readInt
		stack.push(input)
	}

	/*
	 * Pops the top value off of the stack and prints it to STDOUT
	 */
	def out() = {
		val output = stack.pop()
		print(output)
	}


	/*
	 * Returns the top value on the stack
	 */
	def top() : Int = {
		return stack.top
	}

	/*
	 * Returns the number of elements in the stack
	 */
	def length() : Int = {
		return stack.length
	}
}


