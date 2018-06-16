package fst.dep

/**
 * @author Sven Van Hove
 * @author Dieter van Loon
 */
object Syntax {
	sealed abstract class Term { }
	
	// lambda stuff 
	case object TSet extends Term
	{
		override def toString = "Set"
	}
	
	case class TPi(varName: String, fromType: Term, toType: Term) extends Term
	{
		override def equals(other : Any) = other match
		{
			case TPi(_, fromType_, toType_) => fromType == fromType_ && toType == toType_ 
			case _ => false
		}
		
		override def toString = "(%s : %s -> %s)".format(varName, fromType, toType)
	}	
	case class Var (index : Int, varsInScope : Int) extends Term;
	case class Abs(varName : String, varType : Term, term : Term) extends Term
	{
		override def equals(o2 : Any) = o2 match
		{
			case Abs(_, varType_, term_) => term == term_ && varType == varType_
			case _ => false
		}
		
		override def toString() = "(\\%s : %s. %s)".format(varName, varType, term)
	}
	case class App(function : Term, arg : Term) extends Term
	{
		override def toString() = "(%s %s)".format(function, arg)
	}
	
	case class Let(varName: String, varType: Term, varVal: Term, term: Term) extends Term
	{
		override def toString = "let %s : %s = %s in\r\n%s".format(varName, varType, varVal, term)
	}
	
	// booleans
	case object TBool extends Term
	{
		override def toString = "Bool"
	}
	
	case object True extends Term;
	case object False extends Term;
	case class If(condition : Term, thenTerm: Term, elseTerm : Term) extends Term
	{
		override def toString = "\r\nif(%s) {\r\n\t%s\r\n} else {\r\n\t%s\r\n}\r\n".format(condition, thenTerm, elseTerm)
	}
	case object BoolElim extends Term;
	
	// naturals
	case object TNat extends Term
	{
		override def toString = "Nat"
	}
	case object Zero extends Term;
	case class Succ(t : Term) extends Term;
	case class Pred(t : Term) extends Term;
	case class IsZero(t : Term) extends Term;
	case object NatInd extends Term;
	
	// singleton types
	case object TI extends Term
	{
		override def toString = "I"
	}
	case object Refl extends Term;
	case object Subst extends Term;
}