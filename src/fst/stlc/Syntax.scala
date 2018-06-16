package fst.stlc

object Syntax {
	sealed abstract class Term { 
		def isVal = isNumVal;
		def isNumVal = false;
	}
	
	sealed abstract class Type {
	}
	
	// essentials
	case class Var (i : Int, n : Int) extends Term {
	  override def isVal = true;
	}
	case class Abs(v : String, ty : Type, t : Term) extends Term {
	  override def isVal = true;
	  // alpha-equivalence... not really needed for STLC, but still..
	  override def equals(o2 : Any) = o2 match { case Abs(_, ty_, t_) => t == t_ && ty == ty_; case _ => false };
	};
	case class App(f : Term, a : Term) extends Term;
	
	case class TArr(t1 : Type, t2 : Type) extends Type;
	
	// booleans
	case object TBool extends Type;
	case object True extends Term {
	  override def isVal = true;
	}
	case object False extends Term {
	  override def isVal = true;
	}
	case class If(c : Term, t1 : Term, t2: Term) extends Term;

	// naturals
	case object TNat extends Type;
	case object Zero extends Term {
	  override def isNumVal = true;
	}
	case class Succ(e : Term) extends Term {
	  override def isNumVal = e.isNumVal;
	}
	case class Pred(e : Term) extends Term;
	case class IsZero(e : Term) extends Term;
}