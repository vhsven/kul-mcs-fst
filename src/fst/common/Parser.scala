package fst.common

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical

/**
 * A generic parser, supports many standard lambda calculus constructs, and
 * constructs terms through an argument Calculus object.
 * 
 * To use this, define a subclass of Calculus[Term,Type,Constraint] 
 * and implement the required constructs. Then you can call parseTerm
 * below to parse strings to terms.   
 * 
 * Based on previous code by Adriaan Moors.
 * 
 * @author Dominique Devriese.
 */
class Parser[Term,Type,Constraint](calc : Calculus[Term,Type,Constraint]) extends StdTokenParsers {
	type Tokens = StdLexical
	val lexical = new StdLexical

	case class UnknownIdentifierException(s: String) extends Exception {
	  override def toString = "Unknown identifier: '" + s + "'";
	}
	def ctxindex(ls:List[String],s:String) : Int = {
	  val i = ls.indexOf(s);
	  if(i == -1) throw new UnknownIdentifierException(s);
	  else i;
	}
	
	type Context = List[String];
	
	lexical.delimiters ++= List("(", ")", ":", ".", "\\",",","->","[","]","!", ":=", "=", ";")
	lexical.reserved ++=List("true","false","if","then","else","succ","pred","iszero","Bool","Nat","All","let","fix","ref", "unit","Unit","Ref","in","natInd","Ref","raise","try","with", "class", "instance", "where","Eq","eq","Pi","Set","Bot")

	// TYPES

	def typ(ctx:Context): Parser[Type] =  // All X. typ, binds X in the context
		("All" ~> ident <~ ".") >> (tyvar => typ(tyvar::ctx) ^^ (t => calc.mkTAll(t,tyvar))) |
		(("All" ~> ident <~ ";") ~ (constraint(ctx) <~ ".")) >> { case tyvar ~ c => typ(tyvar::ctx) ^^ (t => calc.mkTAllC(t,c,tyvar))} |
		tyarrow(ctx)
	
	def constraint(ctx: Context) : Parser[Constraint] =
		"Eq" ^^ (_ => calc.mkCEq) |
	    ident ^^ (s => {calc.mkCVar(ctxindex(ctx,s),ctx.length)})
	    
	def tyarrow(ctx:Context): Parser[Type] =  // basetyp -> basetyp -> basetyp -> ... , right associative
		basetyp(ctx) ~ opt("->" ~> tyarrow(ctx)) ^^ { case t ~ None => t
													  case t1 ~ Some(t2) => calc.mkTArr(t1,t2)
													}
		
	def basetyp(ctx:Context): Parser[Type] =
		"Unit" ^^ (_ => calc.mkTUnit) |
		"Bool" ^^ (_ => calc.mkBool) |
		"Nat"  ^^ (_ => calc.mkNat) |
		"Set" ^^ (_ => calc.mkSet) |
		"Bot" ^^ (_ => calc.mkTBot) |
		ident  ^^ (s=> { calc.mkTVar(ctxindex(ctx,s),ctx.length)}) |
		"Ref" ~> typ(ctx) ^^ (t => calc.mkTRef(t)) |
		"("~>typ(ctx)<~")"
							 	

    // TERMS							 	
	def term(ctx:Context): Parser[Term] = 
		("\\" ~> ident <~ ".") >> (tyvar => term(tyvar::ctx) ^^ (t => calc.mkTAbs(t,tyvar))) |  						// \X. term, binds X in the context
		("\\" ~> ident <~ ":") ~ (typ(ctx) <~ ".") >> { case v~ty => term(v::ctx) ^^ (t => calc.mkAbs(v,ty,t)) } | 	    // \x:T. term, binds (x,T) in the context
		("\\" ~> ident <~ ";") ~ (constraint(ctx) <~ ".") >> { case v~ct => term(v::ctx) ^^ (t => calc.mkTAbsC(t,ct,v)) } | 	    // \x:T. term, binds (x,T) in the context
		("let" ~> ident <~ ":") ~ (typ(ctx) <~ "=") ~ (term(ctx) <~ "in") >> { case v~ty~vi => term(v::ctx) ^^ (t => calc.mkLet(v,ty,vi,t)) } | 	// let x: T = y in term, binds (x,T) in the context of term
		("try" ~> term(ctx) <~ "with") ~ term(ctx) ^^ { case t1~t2 => calc.mkTry(t1,t2) } | 	// try t1 with t2
		("class" ~> ident ~ ident <~ "where") ~ (ident <~ ":") >> {case cv~tyv~mv => (typ(tyv::ctx) <~ "in") ~ term(mv::cv::ctx) ^^ {case mty~e => calc.mkLetClass(e,mty,cv,mv,tyv) }} |   
		("instance" ~> constraint(ctx) ~ typ(ctx) <~ ("where" ~ term(ctx) ~ "=")) ~ (term(ctx) <~ "in") ~ term(ctx) ^^ { case c~ty~o~t => calc.mkLetInst(t,c,ty,o) } |
		app(ctx)
	
    def app(ctx:Context): Parser[Term] = 																			 	// t1 t2 t3 ... , left associative application
		typapp(ctx)~rep(baseterm(ctx)) ^^ { case t ~ Nil => t
    								   		case t ~ l => (t/:l)( (x,y) => calc.mkApp(x,y))
										  }

	def typapp(ctx:Context): Parser[Term] =																			 	// t [T1] [T2] ... , left assoc type application
		baseterm(ctx)~rep("["~> typ(ctx) <~"]") ^^ { case t ~ Nil => t
													 case t ~ l => (t/:l)( (x,y) => calc.mkTApp(x,y))
												   }
												   
	def baseterm(ctx:Context) = numericLit ^^ (s => calc.mkNatLit(s.toInt)) |
								"true" ^^ (_ => calc.mkTrue) |
								"eq" ^^ (_ => calc.mkEq) |
								"unit" ^^ (_ => calc.mkUnit) |
								"ref" ~> term(ctx) ^^ (t => calc.mkRef(t)) |
								"!" ~> term(ctx) ^^ (t => calc.mkDeref(t)) |
								"false" ^^ (_ => calc.mkFalse) |
								"if" ~> term(ctx) ~ "then" ~ term(ctx) ~ "else" ~ term(ctx) ^^ { case t1~"then"~t2~"else"~t3 => calc.mkIfThenElse(t1,t2,t3) } |
								"succ" ~> term(ctx) ^^ (t => calc.mkSucc(t)) |
								"fix" ~> term(ctx) ^^ (t => calc.mkFix(t)) |
								"pred" ~> term(ctx) ^^ (t => calc.mkPred(t)) |
								"iszero" ~> term(ctx) ^^ (t => calc.mkIsZero(t)) |
								"raise" ~> term(ctx) ^^ (t => calc.mkRaise(t)) |
								ident ^^ (s=> { calc.mkVar(ctxindex(ctx,s),ctx.length)}) |
								// for simplicity: parentheses around assignment.
								"(" ~> term(ctx) ~ ":=" ~ term(ctx) <~ ")" ^^ {case t1 ~ ":=" ~ t2 => calc.mkAssign(t1,t2)} | 
								"("~>term(ctx)<~")" 
	
	case class IncompleteParseException(parsedTerm:Term) extends Exception;
	case class FailedParseException(failure:ParseResult[Term]) extends Exception;

	def parseTerm(s : String) : Term = {
	  term(Nil)(new lexical.Scanner(s)) match {
                  case Success(r,next) if (next.atEnd) => r;
                  case Success(r,next) => throw new IncompleteParseException(r);
                  case failure => throw new FailedParseException(failure);
         }
	}
}
