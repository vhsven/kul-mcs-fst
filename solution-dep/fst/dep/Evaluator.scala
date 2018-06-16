package fst.dep

import Syntax._;

/**
 * @author Sven Van Hove
 * @author Dieter van Loon
 */
class Evaluator {
	def shift(t:Term, d:Int, c:Int): Term = {  // shift de Bruijn indices (above cutoff c) by d (term-level)
          t match {
                  case Succ(t2) => Succ(shift(t2,d,c))
                  case Pred(t2) => Pred(shift(t2,d,c))
                  case IsZero(t2) => IsZero(shift(t2,d,c))
                  case If(t1,t2,t3) => If(shift(t1,d,c),shift(t2,d,c),shift(t3,d,c))                  
                  case App(t1,t2) =>  App(shift(t1,d,c), shift(t2,d,c))
                  case Var(i,n) => if (i<c) Var(i,n+d) else Var(i+d,n+d)
                  case Abs(varName,varType,term)			 => Abs(varName,shift(varType,d,c), shift(term,d,c+1))
                  case TPi(varName,fromType,toType)			 => TPi(varName,shift(fromType,d,c),shift(toType,d,c+1))
                  case Let(varName, varType, varValue, term) => Let(varName,shift(varType,d,c), shift(varValue,d,c), shift(term,d,c+1))              
                  case t => t
//                  case Zero => Zero
//                  case True => True
//                  case False => False
//                  case TBool => TBool
//                  case TNat => TNat
//                  case TSet => TSet
//                  case NatInd => NatInd
//                  case BoolElim => BoolElim
//                  case TI => TI
//                  case Refl => Refl
//                  case Subst => Subst
          }
    }

	def subst(t:Term, v:Int, s:Term) : Term = { // [v -> s] t
          t match {
                  case Succ(t2) => Succ(subst(t2,v,s))
                  case Pred(t2) => Pred(subst(t2,v,s))
                  case IsZero(t2) => IsZero(subst(t2,v,s))
                  case If(t1,t2,t3) => If(subst(t1,v,s),subst(t2,v,s),subst(t3,v,s))
                  case App(t1,t2) => App(subst(t1,v,s),subst(t2,v,s))
                  case Var(i,n) => if (i==v) s else Var(i,n)
                  case Abs(varName,varType,term)			 => Abs(varName,subst(varType,v,s), subst(term,v+1,s))
                  case TPi(varName,fromType,toType)			 => TPi(varName,subst(fromType,v,s),subst(toType,v+1,s))
                  case Let(varName, varType, varValue, term) => Let(varName,subst(varType,v,s), subst(varValue,v,s), subst(term,v+1,s))
                  case t => t
//                  case Zero => Zero
//                  case True => True
//                  case False => False
//                  case TBool => TBool
//                  case TNat => TNat
//                  case TSet => TSet
//                  case NatInd => NatInd
//                  case BoolElim => BoolElim
//                  case TI => TI
//                  case Refl => Refl
//                  case Subst => Subst
          }
    }
	
	def termSubstTop(source:Term, target:Term): Term = { // see Pierce p 385
		val shiftSource = shift(source,1, 0)
		val substTerm = subst(target,0, shiftSource)
		val shiftBack = shift(substTerm,-1,0)
		
		return shiftBack
    }

	case class NoRuleApplies extends Exception;

	def eval1(t:Term): Term = 
	{
		t match
		{
			case If(True, t1, t2) => t1												//E-IFTRUE
			case If(False, t1, t2) => t2											//E-IFFALSE
			case If(c, t1, t2) => 
			{
				try
				{
					If(eval1(c), t1, t2)											//E-IF1
				}
				catch
				{
					case ex:NoRuleApplies =>
						try
						{
							If(c, eval1(t1), t2)									//E-IF2
						}
						catch
						{
							case ex:NoRuleApplies => If(c, t1, eval1(t2))			//E-IF3
						}
				}
			}
			
			case Succ(e) => Succ(eval1(e))											//E-SUCC
			case Pred(Zero) => Zero													//E-PREDZERO
			case Pred(Succ(t)) => t													//E-PREDSUCC
			case Pred(e) => Pred(eval1(e))											//E-PRED
			case IsZero(Zero) => True												//E-ISZEROZERO
			case IsZero(Succ(t)) => False											//E-ISZEROSUCC
			case IsZero(e) => IsZero(eval1(e))										//E-ISZERO
			
			case Let(varName, varType, varVal, term) => 							//E-LET 
				val substTerm = termSubstTop(varVal, term)
				return substTerm
			
			case App(Abs(varName, varType, term), args) => 							//E-APPABS
				val substTerm = termSubstTop(args, term)
				return substTerm
			
			case Abs(varName, varType, term) => 
				try
				{
					val evaled = eval1(term)
					Abs(varName, varType, evaled)									//E-ABS1
				}
				catch
				{
					case ex:NoRuleApplies => 
						val evaledType = eval1(varType)
						Abs(varName, evaledType, term) 								//E-ABS2
				}
				
			case TPi(varName, fromType, toType) =>
				try
				{
					val evaled = eval1(toType)
					TPi(varName, fromType, evaled)									//E-PI1
				}
				catch
				{
					case ex:NoRuleApplies => 
						val evaledType = eval1(fromType)
						TPi(varName, evaledType, toType)							//E-PI2
				}
			
			// Natural Induction
			//                (NatInd P B I 3)
			//           (I 2 (NatInd P B I 2))
			//      (I 2 (I 1 (NatInd P B I 1)))
			// (I 2 (I 1 (I 0 (NatInd P B I 0))))
			// (I 2 (I 1 (I 0           B     )))
			case App(App(App(App(NatInd,predicate),base),induction), Zero) => base	//E-NATINDBASE
			case App(App(App(App(NatInd,predicate),base),induction), Succ(n)) => 	//E-NATINDIND
				 App(App(induction, n), App(App(App(App(NatInd,predicate),base),induction), n))
				
			case App(App(App(App(NatInd,predicate),base),induction), t) => 			//E-NATIND
				 App(App(App(App(NatInd,predicate),base),induction), eval1(t)) 
			
			// Boolean Elimination
			case App(App(App(App(BoolElim, predicate), baseTrue), baseFalse), True) => baseTrue				//E-BOOLELIMTRUE
			case App(App(App(App(BoolElim, predicate), baseTrue), baseFalse), False) => baseFalse			//E-BOOLELIMFALSE
			case App(App(App(App(BoolElim, predicate), baseTrue), baseFalse), c) => 						//E-BOOLELIM
				 App(App(App(App(BoolElim, predicate), baseTrue), baseFalse), eval1(c))
			
			// Singleton Types
			case App(App(App(App(App(App(Subst,_A1),x),y),p),App(App(Refl,_A2),z)),px) if _A1 == _A2 => px	//E-SUBST
			
			case App(function, args) =>
				try
				{
					val evaled = eval1(args)
					App(function, evaled)											//E-APP1
				}
				catch
				{
					case ex:NoRuleApplies =>
						val evaledF = eval1(function)
						App(evaledF, args)											//E-APP2
				}
				
			case _ => throw NoRuleApplies()
		}
	}

	def eval(t:Term): Term = { // evaluation until normal form
		try {
			val tt = eval1(t)
			eval(tt)
		}
		catch { 
		    case ex:NoRuleApplies => t 
		}
    }
}