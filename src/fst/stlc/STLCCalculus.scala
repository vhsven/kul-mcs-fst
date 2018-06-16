package fst.stlc
import fst.common.Calculus
import fst.common.Parser
import Syntax._;

/**
 * A simply typed lambda calculus with booleans and naturals.
 * 
 * Based on previous code by Adriaan Moors.
 * 
 * @author Dominique Devriese.
 */
class STLCCalculus extends Calculus[Term,Type,Unit] {
  
  val parser = new Parser(this);
  
  type Term = Syntax.Term;
  type Type = Syntax.Type;
  
  override def mkVar(i : Int, n : Int) = Var(i,n);
  override def mkAbs(v : String, ty: Type, t: Term) = Abs(v, ty, t);
  override def mkApp(f : Term, a : Term) = App(f, a); 
  
  override def mkTArr(t1 : Type, t2: Type) = TArr(t1, t2);
  
  override def mkBool = TBool;
  override def mkTrue = True;
  override def mkFalse = False;
  override def mkIfThenElse(c: Term, e1: Term, e2: Term) = If(c,e1,e2);
  
  override def mkNat = TNat;
  override def mkZero = Zero;
  override def mkSucc(e: Term) = Succ(e);
  override def mkPred(e: Term) = Pred(e);
  override def mkIsZero(e: Term) = IsZero(e);
  
}