package pietcs345
import scala.collection.mutable

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

	def top() : Int = {
		return stack.top
	}

	def length() : Int = {
		return stack.length
	}
}


