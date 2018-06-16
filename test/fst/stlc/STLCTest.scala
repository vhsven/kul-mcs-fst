package fst.stlc

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import fst.common.Parser
import Syntax._;
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class STLCTest extends FunSuite with ShouldMatchers {
	val calc = new STLCCalculus();
	val parser = new Parser(calc);
	val eval = new Evaluator;
	val typer = new Typer;

	var i = 0;
	def parseTest(in: String, out: Term) {
		test("parse expression " + i + ": '" + in + "'") {
			val e = parser.parseTerm(in);
			e should be ===(out);
		}
		i = i + 1
	}

	def evaluateAndTypeTest(in:String, ty: Type, out: Term) {
		test("type and evaluate expression " + i + ": '" + in + "'") {
			val e : Term = parser.parseTerm(in);
			if( ty != null) {
				typer.typeOf(e,Nil) should be ===(ty);
				if(out != null) { eval.eval(e) should be ===(out) };
			} else { 
				evaluating {typer.typeOf(e,Nil)} should produce [TypeException];
			}
		}
		i = i + 1
	}

	parseTest(
			"""(\x : Bool -> (Bool -> Bool) -> Bool -> Bool.x)""",
			Abs("x",TArr(TBool,TArr(TArr(TBool,TBool),TArr(TBool,TBool))),Var(0,1)));
	parseTest(
			"succ 2",
			Succ(Succ(Succ(Zero))));
	parseTest(
			"if iszero true then 3 else false",
			If(IsZero(True),Succ(Succ(Succ(Zero))),False));
	parseTest(
			"let x : Bool = true in (if x then 3 else 2)",
			App(Abs("x",TBool,If(Var(0,1),Succ(Succ(Succ(Zero))),Succ(Succ(Zero)))),True));
	parseTest(
			""" (\b:Bool->Bool->Bool. \t:Bool. \f:Bool. b t f) (\x:Bool. \y:Bool.y) true false """,
			App(App(App(Abs("b",TArr(TBool,TArr(TBool,TBool)),Abs("t",TBool,Abs("f",TBool,App(App(Var(2,3),Var(1,3)),Var(0,3))))),Abs("x",TBool,Abs("y",TBool,Var(0,2)))),True),False));

	evaluateAndTypeTest("let x : Bool = true in (if x then 3 else 2)", TNat, Succ(Succ(Succ(Zero))));
	evaluateAndTypeTest("succ 0 ", TNat, Succ(Zero));
	evaluateAndTypeTest("succ pred 0", TNat, Succ(Zero));
	evaluateAndTypeTest("if true then false else true", TBool,False);
	evaluateAndTypeTest("iszero pred 0", TBool, True);
	evaluateAndTypeTest("if iszero pred 0 then succ 0 else pred succ 0", TNat, Succ(Zero));
	evaluateAndTypeTest("if iszero 0 then succ 0 else 0", TNat, Succ(Zero));
	evaluateAndTypeTest("if iszero succ 0 then true else false",TBool,False);
	evaluateAndTypeTest("0",TNat,Zero);
	evaluateAndTypeTest(""" \x:Bool. \y:Bool.x """, 
			TArr(TBool,TArr(TBool,TBool)),Abs("x",TBool,Abs("y",TBool,Var(1,2)))); // the encoded boolean true over the Bool type
	evaluateAndTypeTest(""" \x:Bool. \y:Bool.y """, 
			TArr(TBool,TArr(TBool,TBool)),Abs("x",TBool,Abs("y",TBool,Var(0,2)))); // the encoded boolean false over the Bool type
	evaluateAndTypeTest(""" \b:Bool->Bool->Bool. \t:Bool. \f:Bool. b t f """, 
			TArr(TArr(TBool,TArr(TBool,TBool)),TArr(TBool,TArr(TBool,TBool))),
			Abs("b",TArr(TBool,TArr(TBool,TBool)),Abs("t",TBool,Abs("f",TBool,App(App(Var(2,3),Var(1,3)),Var(0,3))))));  // the conditional test
	evaluateAndTypeTest(""" (\b:Bool->Bool->Bool. \t:Bool. \f:Bool. b t f) (\x:Bool. \y:Bool.y) true false """, TBool, False); // conditional applied to false
	evaluateAndTypeTest("""(\x:Bool. if x then 0 else 1) true""",TNat,Zero);
	evaluateAndTypeTest("""(\f:Nat->Nat. \n:Nat.f (f n)) (\n:Nat. succ n) 0""", TNat, Succ(Succ(Zero)));
	evaluateAndTypeTest("""(\f:Nat->Nat. \n:Nat.f (f n)) ((\f:Nat->Nat. \n:Nat.f (f n)) (\n:Nat. succ n)) 0""",
			TNat, Succ(Succ(Succ(Succ(Zero)))));
}