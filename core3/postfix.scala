// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object C3a {

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

val parentheses = Map(")" -> "(",
		"]" -> "[",
		"}" -> "{")

// the left parentheses used in the algorithm
val left_parentheses = List("(","[","{")

// the right parentheses used in the algorithm
val right_parentheses = List(")","]","}")

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList


// (1) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.
//
// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  
// 

def is_op(op: String) : Boolean = ops.contains(op)
	

def prec(op1: String, op2: String) : Boolean = precs.get(op1).get > precs.get(op2).getOrElse(0)


def isAllDigits(x: String) = x forall Character.isDigit 

def get_head(st: Toks) : String = st match {

	case Nil => ""
	case element :: rest => element
}

def pop_elements(st: Toks,left_parentheses : String) : Toks = st match {

	case Nil => Nil
	case element :: rest if (left_parentheses == element) => rest 
	case element :: rest => pop_elements(rest,left_parentheses) 
}

def get_popped_elements(st: Toks,left_parentheses : String) : Toks = st.dropRight(st.length - st.indexOf(left_parentheses))


def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
    
	case Nil => out ::: st 
	case element :: rest if isAllDigits(element) => syard(rest,st,out ::: element :: Nil)
	case element :: rest if (is_op(element) && prec(element,get_head(st)) )  => syard(rest,element :: st, out)
	case element :: rest if (is_op(element) && prec(element,get_head(st) ) == false ) => syard(rest,element :: Nil,out ::: st)
	case element :: rest if (left_parentheses.contains(element)) => syard(rest, element :: st , out)
	case element :: rest if (right_parentheses.contains(element)) => syard(rest, pop_elements(st,parentheses.get(element).get),out ::: get_popped_elements(st,parentheses.get(element).get)  )
	  
}

 


// test cases
//C3a.syard(C3a.split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//C3a.syard(C3a.split("10 + 12 * 33"))       // 10 12 33 * +
//C3a.syard(C3a.split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//C3a.syard(C3a.split("5 + 7 / 2"))          // 5 7 2 / +
//C3a.syard(C3a.split("5 * 7 / 2"))          // 5 7 * 2 /
//C3a.syard(C3a.split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//C3a.syard(C3a.split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (2) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as argumenta. The function should produce the 
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.    

def compute(toks: Toks, st: List[Int] = Nil) : Int = ???


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}


