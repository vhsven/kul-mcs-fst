package fst.stlcfix

import Syntax._;

/**
 * 
 * @author Dominique Devriese.
 */
class Evaluator {
    def shift(t:Term, d:Int, c:Int): Term = {  // shift de Bruijn indices (above cutoff c) by d (term-level)
          t match {
                  case Zero => Zero
                  case Succ(t2) => Succ(shift(t2,d,c))
                  case Pred(t2) => Pred(shift(t2,d,c))
                  case IsZero(t2) => IsZero(shift(t2,d,c))
                  case True => True
                  case False => False
                  case If(t1,t2,t3) => If(shift(t1,d,c),shift(t2,d,c),shift(t3,d,c))                  
                  case App(t1,t2) =>  App(shift(t1,d,c), shift(t2,d,c))
                  case Var(i,n) => if (i<c) Var(i,n+d) else Var(i+d,n+d)
                  case Abs(nh,ty,t1) => Abs(nh,ty,shift(t1,d,c+1))
                  case Fix(t) => Fix(shift(t,d,c))
          }
    }

    def subst(t:Term, v:Int, s:Term) : Term = {  // [v<-s] t
          t match {
                  case Zero => Zero
                  case Succ(t2) => Succ(subst(t2,v,s))
                  case Pred(t2) => Pred(subst(t2,v,s))
                  case IsZero(t2) => IsZero(subst(t2,v,s))
                  case True => True
                  case False => False
                  case If(t1,t2,t3) => If(subst(t1,v,s),subst(t2,v,s),subst(t3,v,s))
                  case App(t1,t2) => App(subst(t1,v,s),subst(t2,v,s))
                  case Var(i,n) => if (i==v) s else Var(i,n)
                  case Abs(nh,ty,t1) => Abs(nh,ty,subst(t1,v+1,shift(s,1,0)))
                  case Fix(t) => Fix(subst(t,v,s))
          }
    }
    
    def termSubstTop(s:Term, t:Term): Term = { // see Pierce p 385
          shift(subst(t,0,shift(s,1, 0)),-1,0)
    }
    
    case class NoRuleApplies extends Exception;
    
    def eval1(t:Term): Term = { // one step evaluation
    		t match {
    		// Pierce p. 34
    		case If(True,t2,t3) => t2                                                 // E-IFTRUE
    		case If(False,t2,t3) => t3                                                // E-IFFALSE
    		case If(t1,t2,t3) => If(eval1(t1),t2,t3)                  	              // E-IF
    		
    		// Pierce p. 41
    		case Succ(t1) => Succ(eval1(t1))                                          // E-SUCC
    		case Pred(Zero) => Zero                                                   // E-PREDZERO
    		case Pred(Succ(nv)) if nv.isNumVal => nv                                  // E-PREDSUCC
    		case Pred(t1) => Pred(eval1(t1))                                          // E-PRED
    		case IsZero(Zero) => True                                                 // E-ISZEROZERO
    		case IsZero(Succ(nv)) if nv.isNumVal => False                             // E-ISZEROSUCC
    		case IsZero(t1) => IsZero(eval1(t1))                                      // E-ISZERO

    		// Pierce p. 72
    		case App(Abs(nh,ty,t1),v2) if v2.isVal => termSubstTop(v2,t1)             // E-APPABS 
    		case App(v1,t2) if v1.isVal => App(v1,eval1(t2))                          // E-APP2
    		case App(t1,t2) => App(eval1(t1),t2)                                      // E-APP1

    		// Pierce p. 144
    		case Fix(Abs(v,ty,t)) => termSubstTop(Fix(Abs(v,ty,t)), t)  // E-FIXBETA
    		case Fix(t) => Fix(eval1(t)) // E-FIX

    		case _ => throw new NoRuleApplies()
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