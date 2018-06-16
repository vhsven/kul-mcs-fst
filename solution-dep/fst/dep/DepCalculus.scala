package fst.dep
import fst.common.Calculus
import fst.common.DepParser
import Syntax._

/**
 * This is the solution template for your dependently typed calculus.
 * 
 * @author Sven Van Hove
 * @author Dieter van Loon
 */
class DepCalculus extends Calculus[Term,Term,Unit] {
  
  type Term = Syntax.Term
  
  val parser = new DepParser(this);
  val eval = new Evaluator();
  
  // lambda stuff 
  override def mkVar(i : Int, n : Int) = Var(i,n);
  override def mkAbs(v: String, ty: Term, t: Term) = Abs(v, ty, t);
  override def mkApp(f : Term, a : Term) = App(f, a); 
  override def mkLet(v: String, ty: Term, vi: Term, t: Term) = Let(v, ty, vi, t);
  override def mkTArr(t1 : Term, t2: Term) = mkPi("_", t1, eval.shift(t2, 1, 0));
  override def mkPi(v: String, t1: Term, t2: Term) = TPi(v, t1, t2);
  override def mkSet = TSet;
  
  // booleans
  override def mkBool = TBool;
  override def mkTrue = True;
  override def mkFalse = False;
  override def mkIfThenElse(c: Term, e1: Term, e2: Term) = If(c, e1, e2);
  override def mkBoolElim = BoolElim;
  
  // naturals
  override def mkNat = TNat;
  override def mkZero = Zero;
  override def mkSucc(e: Term) = Succ(e);
  override def mkPred(e: Term) = Pred(e);
  override def mkIsZero(e: Term) = IsZero(e);
  override def mkNatInd = NatInd;
    
  // singleton types
  override def mkI = TI;
  override def mkRefl = Refl;
  override def mkSubst = Subst;
  
   	//0 + m = m
  	//(n+1) + m = 1 + n + m
  	//def Plus(t1 : Term, t2 : Term) : Term = t1 match 
	//{
	//	case Zero => t2								//basisgeval : Nat -> Nat = t2 -> t2
	//	case Succ(t) => Succ(Plus(t, t2))			//succ (plus t t2)
	//	case _ => null
	//}
  	
  	//0 x m = 0 
	//(n+1) x m = m + n x m
	//def Times(t1 : Term, t2 : Term) : Term = t1 match
	//{
	//	case Zero => Zero							//basisgeval : Nat -> Nat = t2 -> 0
	//	case Succ(t) => Plus(t2, Times(t, t2))		//plus t2 (times t t2)
	//	case _ => null
	//}
  
  	//def Pred(t : Term) : Term = t match
  	//{
  	//	case Zero => Zero							//basisgeval : Nat = 0
  	//	case Succ(t) => t
  	//	case _ => null
  	//}
	
  	def timesDefinition = """natInd (\n: Nat. Nat -> Nat) (\x : Nat. 0) (\n:Nat.\h:Nat -> Nat.\v:Nat.plus v (h v))"""
  	def pred2Definition = """natInd (\n:Nat. Nat) 0 (\n:Nat.\hyp:Nat.n)"""

  	//let prop : Nat -> Set = \ n:Nat. I Nat (plus n 0) n in
  	//let prf : (n: Nat) -> prop n =                  ...           in    	 
  	def proofTerm = """natInd (\n:Nat.I Nat (plus n 0) n)""" + 				//predikaat: te bewijzen 
  					"""  (refl Nat 0)""" +									//basisgeval: 0+0=0
  					"""  (\n:Nat.""" + 										//inductiehypothese:
  					"""    \hyp:I Nat (plus n 0) n.""" + 					//  n + 0 = n
  					"""    subst Nat (plus n 0) n""" + 						//inductiestap: 
  					"""     (\t:Nat.I Nat (succ(plus n 0)) (succ t))""" + 	//  succ(n + 0) = succ(n) + 0 = succ(n)
  					"""     hyp (refl Nat(succ(plus n 0)))""" + 
  					"""  )""" 

  	def ifXThenNatElseBoolDefinition = """(boolElim (\y:Bool. if y then Nat else Bool) 0 true)""";   
}