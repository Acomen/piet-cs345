package pietcs345
import scala.collection.mutable

/**Important Note: Methods are implemented based on the mutable stack
	as the underlying container. This is probably poor design, should
	use the ProgramStack methods wherever possible - in case we decide
	to change the container. Not sure how to do it in Scala yet. **/
class ProgramStack{
	var stack = new mutable.Stack[Int]()

	def push(value: Int) = {
		stack = stack.push(value)
	}

	def pop() : Int = {
		val item = stack.pop()
		stack = stack
		return item
	}

	def add() = {
		val first = stack.pop()
		val second = stack.pop()
		stack = stack.push(second + first)
	}

	def subtract() = {
		val first = stack.pop()
		val second = stack.pop()
		stack = stack.push(second - first)
	}

	def multiply() = {
		val first = stack.pop()
		val second = stack.pop()
		stack = stack.push(second * first)
	}

	def divide() = {
		val first = stack.pop()
		val second = stack.pop()
		stack = stack.push(second / first)
	}

	def mod() = {
		val first = stack.pop()
		val second = stack.pop()
		var mod = second % first
		if(mod < 0){
			mod += first
		}
		stack = stack.push(mod)
	}

	def not() = {
		val top = stack.pop()
		if(top == 0){
			stack = stack.push(1)
		}else{
			stack = stack.push(0)
		}
	}

	def greater() = {
		val first = stack.pop()
		val second = stack.pop()
		if(second > first){
			stack = stack.push(1)
		}else{
			stack = stack.push(0)
		}
	}

	def duplicate() = {
		val top = stack.top
		stack = stack.push(top)
	}

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

	def top() : Int = {
		return stack.top
	}

	def length() : Int = {
		return stack.length
	}
}


