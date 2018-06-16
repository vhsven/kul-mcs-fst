package fst.dep

import Syntax._

case class TypeException(msg:String) extends Exception(msg);

/**
 * @author Sven Van Hove
 * @author Dieter van Loon
 */
class Typer(eval: Evaluator) {
	type Context = List[Term]
  
	def shiftContext(ctx: Context,d: Int, c: Int) = ctx map { eval.shift(_, 1, 0) }
	
	def shiftContextTop(ctx : Context) = shiftContext(ctx, 1, 0)
	
	def pushOnContext(t : Term, ctx : Context) = shiftContextTop(t :: ctx)
  
	def typeOf(t: Term,ctx: Context): Term = t match
	{
		case TSet => TSet																	// T-SETINSET
		case TNat => TSet																	// T-NAT
		case Zero => TNat																	// T-ZERO 
		case Succ(_) => TNat																// T-SUCC
		case Pred(_) => TNat																// T-PRED
		case IsZero(_) => TBool																// T-ISZERO
		case TBool => TSet																	// T-BOOL
		case True => TBool																	// T-TRUE
		case False => TBool																	// T-FALSE
		case If(c, t1, t2) =>																// T-IF 
			if(typeOf(c, ctx) != TBool) throw new TypeException("If: condition not bool")
			val t1Type = typeOf(t1, ctx)
			val t2Type = typeOf(t2, ctx)
			if(t1Type != t2Type) throw new TypeException("If: then and else branch have different types")
			
			return t1Type
		
		case Var(index,varsInScope) => ctx(index)											// T-VAR
	   
		case Abs(x, t1, t2) =>																// T-ABS 
			val t1_ = eval.eval(t1)
			val tT1_ = typeOf(t1_, ctx)
			if (tT1_ != TSet) throw new TypeException("Abs: varType != Set")
			val shiftedCtx = pushOnContext(t1_, ctx)
			val tT2 = typeOf(t2, shiftedCtx)
	  	 
			return TPi(x, t1_, tT2)
	  	 
		case TPi(x, t1, t2) =>																// T-PI
			if (typeOf(t1, ctx) != TSet) throw new TypeException("TPi: fromType != Set")
			val t1_ = eval.eval(t1)
			if(typeOf(t2, pushOnContext(t1_, ctx)) != TSet) throw new TypeException("TPi: toType != Set")
			
			return TSet
			
		case App(t1, t2) => 																// T-APP
			val t1_ = typeOf(t1, ctx)
			t1_ match
			{
				case TPi(x, t3, t4) =>
					val t2_ = typeOf(t2, ctx)
					if(t2_ != t3) throw new TypeException("App: wrong arg type. Found " + t2 + " : " + t2_ + ", expected type " + t3 + ".")
					val subst = eval.termSubstTop(t2, t4)
					val t4_ = eval.eval(subst)
					return t4_
					
				case _ => throw new TypeException("App: function was not of Pi-type")
			}
		
		case Let(x, t1, t2, t3) =>	//Let(varName, varType, varValue, term)					// T-LET
			if (typeOf(t1, ctx) != TSet) throw new TypeException("Let: varType != Set")
			val t1_ = eval.eval(t1)
			val tT2 = typeOf(t2, ctx)
			if(tT2 != t1_) throw new TypeException("Let: type of varValue (" + tT2 +  ") did not match varType (" + t1_ + ")")
			val subst = eval.termSubstTop(t2, t3)
			val t4 = typeOf(subst, ctx)
			
			return t4
		
		//T-NATIND
		case NatInd => TPi("P",TPi("_",TNat,TSet),TPi("_",App(Var(0,1),Zero),TPi("_",TPi("n",TNat,TPi("_",App(Var(2,3),Var(0,3)),App(Var(3,4),Succ(Var(1,4))))),TPi("n",TNat,App(Var(3,4),Var(0,4))))))
		
		//T-BOOLELIM
		case BoolElim => TPi("P",TPi("_",TBool,TSet),TPi("_",App(Var(0,1),True),TPi("_",App(Var(1,2),False),TPi("b",TBool,App(Var(3,4),Var(0,4))))))
		
		//T-TI
		case TI => TPi("A",TSet,TPi("_",Var(0,1),TPi("_",Var(1,2),TSet)))
		
		//T-REFL
		case Refl => TPi("A",TSet,TPi("x",Var(0,1),App(App(App(TI,Var(1,2)),Var(0,2)),Var(0,2))))
		
		//T-SUBST
		case Subst => TPi("A",TSet,TPi("x",Var(0,1),TPi("y",Var(1,2),TPi("P",TPi("_",Var(2,3),TSet),TPi("_",App(App(App(TI,Var(3,4)),Var(2,4)),Var(1,4)),TPi("_",App(Var(1,5),Var(3,5)),App(Var(2,6),Var(3,6))))))))
  }
}