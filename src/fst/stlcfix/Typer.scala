package fst.stlcfix

import Syntax._

case class TypeException extends Exception;

class Typer {
  type Context = List[Type]
  def typeOf(t: Term,ctx: Context): Type = {
        t match {
                  case Zero => TNat                                          // T-ZERO
                  case Succ(t2) =>                                            // T-SUCC
                        if (typeOf(t2,ctx) == TNat) TNat 
                        else throw new TypeException()
                  case Pred(t2) =>                                            // T-PRED
                    if (typeOf(t2,ctx) == TNat) TNat 
                    else throw new TypeException()
                  case IsZero(t2) =>                                          // T-ISZERO
                    if (typeOf(t2,ctx) == TNat) TBool 
                    else throw new TypeException()
                  case True => TBool                                         // T-TRUE
                  case False => TBool                                        // T-FALSE
                  case If(t1,t2,t3) => {                                      // T-IF
                        if (typeOf(t1,ctx) == TBool) {
                           val T2= typeOf(t2,ctx)
                           val T3= typeOf(t3,ctx)
                           if (T2==T3) T2
                           else throw new TypeException()
                        } else throw new TypeException()
                  }
                  case App(t1,t2) => {                                          // T-APP
                        val tT1 = typeOf(t1,ctx)
                        val tT2 = typeOf(t2,ctx)
                        tT1 match {
                                case TArr(tT11,tT12) => 
                                   if (tT11 == tT2) tT12 
                                   else throw new TypeException()
                                case _ => throw new TypeException()
                        }
                  }
                  case Var(i,n) =>                                                      // T-VAR 
                  	ctx(i)
                  case Abs(nh,tT,t1) => {                                       // T-ABS
                        val tT1 = typeOf(t1,(tT :: ctx));
                        TArr(tT,tT1)
                  }
                  case Fix(t) => {
                    val tT = typeOf(t,ctx);
                    tT match {
                      case TArr(tT1,tT2) if tT1 equals tT2 => tT1
                      case _ => throw new TypeException()
                    }
                  }
        }
  }
  

}