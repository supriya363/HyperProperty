package parser


import model._
import scala.util.Try
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.token.StdTokens



object ScalaParser extends StdTokenParsers with StdTokens with PackratParsers{
        type Tokens = StdLexical
        val lexical = new StdLexical

        lexical.delimiters ++= List("+", "-", "*",";","==","=","<",">","++",":","(",")",",")
        lexical.reserved ++= List("print","if","else","then","endif","Int","String","var","while","do","endwhile","Str","Func","call","returns")
        
        def parseExpression(text: String) =                       // expression parser 
        {
                  val tokens = new PackratReader(new lexical.Scanner(text))
                  phrase(expr)(tokens) match 
                  {
                    case Success(exp, _) => Some(exp)
                    case NoSuccess(msg, next) => println("data typemismatched")
                                                 None
                 }
        }
        
         
        lazy val expr: PackratParser[Expression] = concatenate
        
          
        lazy val concatenate  : PackratParser[Expression] = 
            (t1) ~ "++" ~ (expr) ^^ {case a ~ b ~ c => Concat(a,c)} | t1
           
        lazy val t1: PackratParser[Expression] = 
           t2 ~ "+" ~ t1 ^^ { case a ~ "+" ~ b => Plus(a, b) } | 
           t2
       
        lazy val t2: PackratParser[Expression] =
           p1 ~ "-" ~ p1 ^^ { case a ~ "-" ~ b => Minus(a, b) } | p1
           
        lazy val p1: PackratParser[Expression] =  
           number  ~ "*" ~ p1 ^^ { case a ~ "*" ~ b => Product(a, b) } | number |   identifier | stringlit  | stoi | itos
         
        lazy val number:  PackratParser[Number] =  numericLit ^^ { n => Number(n.toInt) } 
       
        lazy val stringlit :   PackratParser[ExpStringLit] = stringLit ^^ { t =>ExpStringLit(t)}
        
        lazy val identifier :   PackratParser[ExpIdentifier] = ident ^^ { t =>ExpIdentifier(t)}
       
        lazy val stoi : PackratParser[Stoi] =
          "Int" ~ "(" ~> stringLit <~ ")" ^^ {case s =>  Stoi(ExpStringLit(s.toString()))}
        
        lazy val itos : PackratParser[Itos] = 
          "Str" ~ "(" ~> numericLit <~ ")" ^^ {case n => Itos(Number(n.toInt))}
          
        
        
        def parseStatement(text: String) =                          // statement parser 
        {
          val tokens = new PackratReader(new lexical.Scanner(text))
                  phrase(stmt)(tokens) match 
                  {
                      case Success(stmt, _) => Some(stmt)
                      case NoSuccess(msg, next) => println("Error: " + msg.toString())
                                                   None
                  }
        }
       
        lazy val stmt: PackratParser[List[Statement]] = blockstatement  
        
        lazy val blockstatement : PackratParser[List[Statement]] = rep(s1)  ^^ { a => a}
        
        lazy val s1: PackratParser[Statement] = declarationstmt | assignstmt | printstmt | ifStatement | whileloop | callstmt
          
        lazy val printstmt: PackratParser[PrintStatement] =  "print"  ~> (expr) <~  ";"    ^^ 
                  { case b => b match 
                              {
                                  case Number(n) =>  PrintStatement(Number(n)) 
                                  case ExpIdentifier(n) => PrintStatement(ExpIdentifier(n))
                                  case ExpStringLit(n) => PrintStatement(ExpStringLit(n))
                                  case Plus(a,b) => PrintStatement(Plus(a,b))
                                  case Minus(a,b) => PrintStatement(Minus(a,b))
                                  case Product(a,b) => PrintStatement(Product(a,b))
                                  case Concat(a,b) => PrintStatement(Concat(a,b))
                                  case Stoi(a) => PrintStatement(Stoi(a))
                                  case Itos(a) => PrintStatement(Itos(a))
                               }   
                  }

        lazy val callstmt: PackratParser[FunctionCall] =
                  "call" ~> identifier ~ "(" ~ parameters ~ ")" ~ opt("returns" ~ "(" ~> parameters) <~ ")" ~ ";" ^^ 
                    { case a ~ b ~ c ~ d ~ e => 
                        e match {
                                  case None => FunctionCall(a.toString, c, List())
                                  case _ => FunctionCall(a.toString, c, e.get)
                                } 
                    }

        lazy val parameters: Parser[List[Expression]] = 
                  expr ~ ( "," ~> parameters ) ^^ {
                    case a ~ c =>
                      a :: c
                  }| expr ^^ { a => List(a)}
     
        lazy val assignstmt: PackratParser[VariableDefinition] =
                  identifier ~ "=" ~ expr <~ ";" ^^ {case a ~ "=" ~ b =>  VariableDefinition(a,b)}
        
        lazy val declarationstmt: PackratParser[VariableDeclaration] =
                  "var" ~> identifier  ~ ":" ~  ("Int" | "String" | "Func") <~ ";" ^^ {case a ~ b ~ c => VariableDeclaration(a,c)}    
         
        lazy val  ifStatement :  PackratParser[IfStatement] = 
                 "if" ~> condition ~ "then" ~ blockstatement ~ opt("else" ~> blockstatement) <~ "endif" ^^ 
                  {case a ~ b ~ c ~ d =>
                   d match{
                           case None => new IfStatement(a, c, List())
                           case _ => new IfStatement(a, c, d.get)
                          }
                  }
        lazy val  whileloop :  PackratParser[WhileLoop] = 
               "while" ~> condition ~ "do" ~ blockstatement <~ "endwhile" ^^ {case a ~ b ~ c  => WhileLoop(a,c)}
        
        lazy val condition : PackratParser[Expression] =
                  (expr |identifier)  ~ ("<" | ">" | "==") ~ expr  ^^ 
                 {case a ~ b ~ c =>
                                  b match{
                                    case ">" => GreaterThan(a,c)
                                    case "<" => LessThan(a,c)
                                    case "==" =>EqualsTo(a,c)
                                  }
                         
                 }
       
       
        
          
        
}
