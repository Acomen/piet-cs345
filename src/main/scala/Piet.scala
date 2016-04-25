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

	def top() : Int = {
		return stack.top
	}

	def length() : Int = {
		return stack.length
	}

}


