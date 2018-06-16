package fst.dep

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import fst.common.DepParser

import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class DepTest extends FunSuite with ShouldMatchers {
	val calc = new DepCalculus();
	val parser = new DepParser(calc);
	val eval = new Evaluator;
	val typer = new Typer(eval);

	type Term = calc.Term
	
	var i = 0;
	
	def parseTest(in: String, out: Term) {
		test("parse expression " + i + ": '" + in + "'") {
			val e = parser.parseTerm(in);
			println(e)
			e should be ===(out);
		}
		i = i + 1
	}

	def evaluateAndTypeTest(in:String, ty: Term, out: Term) {
		test("type and evaluate expression " + i + ": '" + "'") { // -- doesn't work? in + "'") {
			val e : Term = parser.parseTerm(in);
			if( ty != null) {
				val myType = typer.typeOf(e,Nil)
				myType should be ===(ty);
				if(out != null) {
					val myResult = eval.eval(e)
					myResult should be ===(out)
				};
			} else { 
				evaluating {
				  typer.typeOf(e,Nil)
				} should produce [TypeException];
			}
		}
		i = i + 1
	}
	
	// general lambda calculus stuff...
	/* 00 */ evaluateAndTypeTest("Nat", calc.mkSet, calc.mkNat);	
	/* 01 */ evaluateAndTypeTest("Bool", calc.mkSet, calc.mkBool);	
	/* 02 */ evaluateAndTypeTest("true", calc.mkBool, calc.mkTrue);	
	/* 03 */ evaluateAndTypeTest("false", calc.mkBool, calc.mkFalse);	
	/* 04 */ evaluateAndTypeTest("0", calc.mkNat, calc.mkZero);	
	/* 05 */ evaluateAndTypeTest("succ 0", calc.mkNat, calc.mkSucc(calc.mkZero));	
	/* 06 */ evaluateAndTypeTest("Set", calc.mkSet, calc.mkSet);	
	
	// t_1 -> t_2 === (_ : t_1) -> t_2
	/* 07 */ evaluateAndTypeTest("""Bool -> Bool""", calc.mkSet, calc.mkPi("_",calc.mkBool, calc.mkBool));
	// shift properly during the translation
	/* 08 */ evaluateAndTypeTest("""Bool -> (A : Set) -> A""", calc.mkSet, calc.mkPi("_",calc.mkBool, calc.mkPi("A", calc.mkSet, calc.mkVar(0,2))));
	/* 09 */ evaluateAndTypeTest("""(A : Set) -> Bool -> A""", calc.mkSet, calc.mkPi("A", calc.mkSet, calc.mkPi("_",calc.mkBool, calc.mkVar(1,2))));
	
	// alpha-equivalence
    /* 10 */ evaluateAndTypeTest("""(x:Nat) -> Nat""", calc.mkSet, calc.mkPi("y",calc.mkNat,calc.mkNat));
	/* 11 */ evaluateAndTypeTest("""\A:Set.A""",calc.mkPi("B",calc.mkSet,calc.mkSet), calc.mkAbs("B",calc.mkSet,calc.mkVar(0,1)));
	
	/* 12 */ evaluateAndTypeTest("""\x:Nat.0""", calc.mkTArr(calc.mkNat,calc.mkNat), calc.mkAbs("x",calc.mkNat,calc.mkZero));	
	/* 13 */ evaluateAndTypeTest("""\x:Nat.succ x""", calc.mkTArr(calc.mkNat,calc.mkNat), calc.mkAbs("x",calc.mkNat,calc.mkSucc(calc.mkVar(0,1))));
	/* 14 */ evaluateAndTypeTest("""Nat -> Nat""", calc.mkSet, calc.mkTArr(calc.mkNat,calc.mkNat));	
	/* 15 */ evaluateAndTypeTest("""(\x:Nat.0) 0""", calc.mkNat, calc.mkZero);	
	/* 16 */ evaluateAndTypeTest("let x : Bool = true in x", calc.mkBool, calc.mkTrue);
	/* 17 */ evaluateAndTypeTest("let x : Bool = true in (if x then 3 else 2)", calc.mkNat, calc.mkNatLit(3));
	/* 18 */ evaluateAndTypeTest("succ 0 ", calc.mkNat, calc.mkNatLit(1));
	/* 19 */ evaluateAndTypeTest("succ pred 0", calc.mkNat, calc.mkNatLit(1));
	/* 20 */ evaluateAndTypeTest("if true then false else true", calc.mkBool,calc.mkFalse);
	/* 21 */ evaluateAndTypeTest("iszero pred 0", calc.mkBool, calc.mkTrue);
	/* 22 */ evaluateAndTypeTest("if iszero pred 0 then succ 0 else pred succ 0", calc.mkNat, calc.mkNatLit(1));
	/* 23 */ evaluateAndTypeTest("if iszero 0 then succ 0 else 0", calc.mkNat, calc.mkNatLit(1));
	/* 24 */ evaluateAndTypeTest("if iszero succ 0 then true else false",calc.mkBool,calc.mkFalse);
	/* 25 */ evaluateAndTypeTest("0",calc.mkNat,calc.mkZero);
	/* 26 */ evaluateAndTypeTest(""" \x:Bool. \y:Bool.x """, 
			calc.mkPi("_",calc.mkBool,calc.mkPi("_",calc.mkBool,calc.mkBool)),calc.mkAbs("x",calc.mkBool,calc.mkAbs("y",calc.mkBool,calc.mkVar(1,2)))); // the encoded boolean true over the Bool type
	/* 27 */ evaluateAndTypeTest(""" \x:Bool. \y:Bool.y """, 
			calc.mkPi("_",calc.mkBool,calc.mkPi("_",calc.mkBool,calc.mkBool)),calc.mkAbs("x",calc.mkBool,calc.mkAbs("y",calc.mkBool,calc.mkVar(0,2)))); // the encoded boolean false over the Bool type
	/* 28 */ evaluateAndTypeTest(""" \b:Bool->Bool->Bool. \t:Bool. \f:Bool. b t f """, 
			calc.mkPi("_",calc.mkPi("_",calc.mkBool,calc.mkPi("_",calc.mkBool,calc.mkBool)),calc.mkPi("_",calc.mkBool,calc.mkPi("_",calc.mkBool,calc.mkBool))), 
			calc.mkAbs("b",calc.mkPi("_",calc.mkBool,calc.mkPi("_",calc.mkBool,calc.mkBool)),calc.mkAbs("t",calc.mkBool,calc.mkAbs("f",calc.mkBool,calc.mkApp(calc.mkApp(calc.mkVar(2,3),calc.mkVar(1,3)),calc.mkVar(0,3))))));  // the conditional test
	/* 29 */ evaluateAndTypeTest(""" (\b:Bool->Bool->Bool. \t:Bool. \f:Bool. b t f) (\x:Bool. \y:Bool.y) true false """, calc.mkBool, calc.mkFalse); // conditional applied to false
	/* 30 */ evaluateAndTypeTest("""(\x:Bool. if x then 0 else 1) true""",calc.mkNat,calc.mkZero);
	/* 31 */ evaluateAndTypeTest("""(\f:Nat->Nat. \n:Nat.f (f n)) (\n:Nat. succ n) 0""", calc.mkNat, calc.mkSucc(calc.mkSucc(calc.mkZero)));
	/* 32 */ evaluateAndTypeTest("""(\f:Nat->Nat. \n:Nat.f (f n)) ((\f:Nat->Nat. \n:Nat.f (f n)) (\n:Nat. succ n)) 0""",
			calc.mkNat, calc.mkNatLit(4));

	// dependent types: basics
    /* 33 */ evaluateAndTypeTest("""(x:Nat) -> Nat""", calc.mkSet, calc.mkPi("x",calc.mkNat,calc.mkNat));	
	/* 34 */ evaluateAndTypeTest("Set", calc.mkSet, calc.mkSet);
	
	// dependent types: polymorphism
	/* 35 */ evaluateAndTypeTest("""(\A:Set.\x:A.x) Nat 0""",calc.mkNat,calc.mkZero);
	/* 36 */ evaluateAndTypeTest("""(\A:Set.\x:A.x) Set Nat""",calc.mkSet,calc.mkNat);
	/* 37 */ evaluateAndTypeTest("""(\P:Bool -> Set.\x:Bool.P x) (\x: Bool. if x then Nat else Bool) true""",calc.mkSet,calc.mkNat);
	/* 38 */ evaluateAndTypeTest("""(\P:Bool -> Set.\x:P true.x) (\x: Bool. if x then Nat else Bool) 0""",calc.mkNat,calc.mkZero);
	/* 39 */ evaluateAndTypeTest("""(\P:Bool -> Set.\x:P false.x) (\x: Bool. if x then Nat else Bool) false""",calc.mkBool,calc.mkFalse);	    
	/* 40 */ evaluateAndTypeTest("""(\A:Set.\x:A.x) ((A : Set) -> A -> A) (\A:Set.\x:A.x) Nat 0""",calc.mkNat,calc.mkZero);
	
	// natural induction...
	/* 41 */ evaluateAndTypeTest("""let plus : Nat -> Nat -> Nat = natInd (\n: Nat. Nat -> Nat) (\x : Nat. x) (\n:Nat.\h:Nat -> Nat.\v:Nat.succ (h v)) in
							plus 0 0""",calc.mkNat,calc.mkZero);
	/* 42 */ evaluateAndTypeTest("""let plus : Nat -> Nat -> Nat = natInd (\n: Nat. Nat -> Nat) (\x : Nat. x) (\n:Nat.\h:Nat -> Nat.\v:Nat.succ (h v)) in
							plus 2 3""", calc.mkNat, calc.mkNatLit(5));
	/* 43 */ evaluateAndTypeTest("""let plus : Nat -> Nat -> Nat = natInd (\n: Nat. Nat -> Nat) (\x : Nat. x) (\n:Nat.\h:Nat -> Nat.\v:Nat.succ (h v)) in
						   let times : Nat -> Nat -> Nat = """ + calc.timesDefinition + """ in
						   times 3 (times 4 5)""", calc.mkNat, calc.mkNatLit(60));	
	/* 44 */ evaluateAndTypeTest("""let pred2 : Nat -> Nat = """ + calc.pred2Definition + """ in
						   pred2 0""", calc.mkNat, calc.mkZero);
	/* 45 */ evaluateAndTypeTest("""let pred2 : Nat -> Nat = """ + calc.pred2Definition + """ in
						   pred2 3""", calc.mkNat, calc.mkNatLit(2));
	/* 46 */ evaluateAndTypeTest("""let pred2 : Nat -> Nat = """ + calc.pred2Definition + """ in
						   pred2 10""", calc.mkNat, calc.mkNatLit(9));
	/* 47 */ evaluateAndTypeTest("""
	    let plus : Nat -> Nat -> Nat = natInd (\n: Nat. Nat -> Nat) (\x : Nat. x) (\n:Nat.\h:Nat -> Nat.\v:Nat.succ (h v)) in
		let oneton : Nat -> Nat = natInd (\n : Nat. Nat) 0 (\n : Nat. \s : Nat. plus n s) in
	    oneton 5
		""", calc.mkNat, calc.mkNatLit(10));
	
	// singleton types... 
	/* 48 */ evaluateAndTypeTest("""let eqprf2 : I Nat 0 0 = refl Nat 0 in
			eqprf2""", calc.mkApp(calc.mkApp(calc.mkApp(calc.mkI,calc.mkNat),calc.mkZero),calc.mkZero),
			calc.mkApp(calc.mkApp(calc.mkRefl,calc.mkNat),calc.mkZero));
	/* 49 */ evaluateAndTypeTest("""subst Nat 0 0 (I Nat 0) (refl Nat 0) (refl Nat 0)""", calc.mkApp(calc.mkApp(calc.mkApp(calc.mkI,calc.mkNat),calc.mkZero),calc.mkZero),
			calc.mkApp(calc.mkApp(calc.mkRefl,calc.mkNat),calc.mkZero));
	/* 50 */ evaluateAndTypeTest("""let plus : Nat -> Nat -> Nat = natInd (\n: Nat. Nat -> Nat) (\x : Nat. x) (\n:Nat.\h:Nat -> Nat.\v:Nat.succ (h v)) in
			subst Nat 0 (plus 0 0)""", 
			calc.mkPi("P",calc.mkPi("A",calc.mkNat,calc.mkSet),
					calc.mkPi("eqPrf",calc.mkApp(calc.mkApp(calc.mkApp(calc.mkI,calc.mkNat),calc.mkZero),calc.mkZero),
							calc.mkPi("px",calc.mkApp(calc.mkVar(1,2),calc.mkZero),
									calc.mkApp(calc.mkVar(2,3),calc.mkZero)))),
									null);
	/* 51 */ evaluateAndTypeTest("""let plus : Nat -> Nat -> Nat = natInd (\n: Nat. Nat -> Nat) (\x : Nat. x) (\n:Nat.\h:Nat -> Nat.\v:Nat.succ (h v)) in
			let eqprf : I Nat 0 (plus 0 0) = refl Nat 0 in
			let eqprf2 : I Nat 0 0 = refl Nat 0 in
			let eqprf3 : I Nat 0 (plus 0 0) = subst Nat 0 (plus 0 0) (I Nat 0) eqprf eqprf2 in
			eqprf3""", calc.mkApp(calc.mkApp(calc.mkApp(calc.mkI,calc.mkNat),calc.mkZero),calc.mkZero),
			calc.mkApp(calc.mkApp(calc.mkRefl,calc.mkNat),calc.mkZero));

	// plus is associative
	/* 52 */ evaluateAndTypeTest("""
	    let plus : Nat -> Nat -> Nat = natInd (\n: Nat. Nat -> Nat) (\x : Nat. x) (\n:Nat.\h:Nat -> Nat.\v:Nat.succ (h v)) in
	    let prf : (n : Nat) -> (m : Nat) -> (k : Nat) -> I Nat (plus n (plus m k)) (plus (plus n m) k) =
	    		natInd (\n : Nat. (m : Nat) -> (k : Nat) -> I Nat (plus n (plus m k)) (plus (plus n m) k))
					(\m : Nat. \k : Nat. refl Nat (plus m k)) 
	    			(\n : Nat. \hyp : (m : Nat) -> (k : Nat) -> I Nat (plus n (plus m k)) (plus (plus n m) k).
						 \m : Nat. \k: Nat. subst Nat (plus n (plus m k)) (plus (plus n m) k)
							(\t : Nat. I Nat (succ (plus n (plus m k))) (succ t)) (hyp m k) 
							(refl Nat (succ (plus n (plus m k))))) in
	    prf 2 3 1		
		""", calc.mkApp(calc.mkApp(calc.mkApp(calc.mkI, calc.mkNat), calc.mkNatLit(6)), calc.mkNatLit(6)), 
			calc.mkApp(calc.mkApp(calc.mkRefl, calc.mkNat), calc.mkNatLit(6)));
	// cleaner using cong..
	/* 53 */ evaluateAndTypeTest("""
	    let plus : Nat -> Nat -> Nat = natInd (\n: Nat. Nat -> Nat) (\x : Nat. x) (\n:Nat.\h:Nat -> Nat.\v:Nat.succ (h v)) in
	    let cong : (A : Set) -> (x : A) -> (y : A) -> (B : Set) -> (f : A -> B) -> I A x y -> I B (f x) (f y) =
				\A : Set. \x : A. \y: A. \B : Set. \f : A -> B. \xisy : I A x y.
	               subst A x y (\t : A. I B (f x) (f t)) xisy (refl B (f x)) in
	    let prf : (n : Nat) -> (m : Nat) -> (k : Nat) -> I Nat (plus n (plus m k)) (plus (plus n m) k) =
	    		natInd (\n : Nat. (m : Nat) -> (k : Nat) -> I Nat (plus n (plus m k)) (plus (plus n m) k))
					(\m : Nat. \k : Nat. refl Nat (plus m k)) 
	    			(\n : Nat. \hyp : (m : Nat) -> (k : Nat) -> I Nat (plus n (plus m k)) (plus (plus n m) k).
						 \m : Nat. \k: Nat. cong Nat (plus n (plus m k)) (plus (plus n m) k) Nat (\x : Nat. succ x) (hyp m k)) in
	    prf 2 3 1		
		""", calc.mkApp(calc.mkApp(calc.mkApp(calc.mkI, calc.mkNat), calc.mkNatLit(6)), calc.mkNatLit(6)), 
			calc.mkApp(calc.mkApp(calc.mkRefl, calc.mkNat), calc.mkNatLit(6)));

	
	// forall n. n + 0 = n
	/* 54 */ evaluateAndTypeTest(
	    """let plus : Nat -> Nat -> Nat = natInd (\n: Nat. Nat -> Nat) (\x : Nat. x) (\n:Nat.\h:Nat -> Nat.\v:Nat.succ (h v)) in
	       let prop : Nat -> Set = \ n:Nat. I Nat (plus n 0) n in
	       let prf : (n: Nat) -> prop n = """ + calc.proofTerm + """ 
	       in prf 1""", calc.mkApp(calc.mkApp(calc.mkApp(calc.mkI,calc.mkNat),calc.mkSucc(calc.mkZero)),calc.mkSucc(calc.mkZero)),
			calc.mkApp(calc.mkApp(calc.mkRefl,calc.mkNat),calc.mkSucc(calc.mkZero)));
	
	// dependent if tests...
	/* 55 */ evaluateAndTypeTest("""let test : (x : Bool) -> if x then Nat else Bool = """ + calc.ifXThenNatElseBoolDefinition + """ in
	                       test false""", calc.mkBool, null);
	/* 56 */ evaluateAndTypeTest("""let test : (x : Bool) -> if x then Nat else Bool = """ + calc.ifXThenNatElseBoolDefinition + """ in
	                       test true""", calc.mkNat, null);
	
	// boolElim Tests
	/* 57 */ evaluateAndTypeTest("""boolElim (\y : Bool. if y then (Bool -> Bool) else Bool) (\y:Bool.if y then false else true) true true true""", calc.mkBool, calc.mkFalse);
    /* 58 */ evaluateAndTypeTest("""boolElim (\y : Bool. if y then (Bool -> Bool) else Bool) (\y:Bool.if y then false else true) true false""", calc.mkBool, calc.mkTrue);
    
	/* 59 */ evaluateAndTypeTest("""(\y:Bool.(\ x: (if y then Bool else Nat). x ) ) true true""", calc.mkBool, calc.mkTrue);
    /* 60 */ evaluateAndTypeTest("""(\y:Bool.(\ x: (if y then Bool else Nat). x ) ) false 0""", calc.mkNat, calc.mkZero);
	
}