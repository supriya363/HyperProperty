package interpreter
import model._
import parser._

object ScalaInterpreter 
{
    var CurrentScope = new Scope()                                                      // scope map to store (variable - value)  
  
    //def calculateExpr(e: Expression):Either[String,Int] =                              //  expression interprete 
    def calculateExpr(e: Expression): Datatype ={
      e match {
        case ExpIdentifier(n) => CurrentScope.variableValue.get(n)  match {            //  identifier ---- get value from scope map
           case Some(int(e))=>  int(e) //Right(e)   ...refers to?
           case Some(string(e)) =>  string(e) //Left(e)
        }
        case Number(n) => int(n)                                                     // numberlit
        case ExpStringLit(n) => string(n)                                               //  stringlit
        case Stoi(ExpStringLit(s)) => int(s.toString().toInt)
        case Itos(Number(n)) => string(n.toString())
        case Plus(Number(a), Number(b)) =>   int(a+b)                             
        case Minus(Number(a), Number(b)) =>  int(a-b)                              
        case Product(Number(a), Number(b))=> int(a*b)                             
        case aOrbOrc @(Plus(_,_) | Minus(_,_) | Product(_,_) ) =>{                    // plus / minus / product  
           val  left = aOrbOrc.asInstanceOf[{def left : Expression}].left
                      "(Plus | Minus | Product)(" + left + ","
           val  right = aOrbOrc.asInstanceOf[{def right : Expression}].right
                      "(Plus | Minus | Product)("  + left + "," + right + ")"
                      
           val result : Datatype = calculateExpr(left)  match{
               case int(c) => {
                    calculateExpr(right) match { 
                            case int(d) => aOrbOrc.getClass.getSimpleName match{
                                 case "Plus" => int(c+d)
                                 case "Minus" => int(c-d)
                                 case "Product" => int(c*d)
                            }
    
                     }
              
               }
          }
       result
       }
        
       case aOrbOrc @(EqualsTo(_,_) | GreaterThan(_,_) | LessThan(_,_))   => {      // condition evaluation ( < , > , == )
         val  left = aOrbOrc.asInstanceOf[{def left : Expression}].left
                     "(EqualsTo | GreaterThan | LessThan)(" + left + "," 
         val  right = aOrbOrc.asInstanceOf[{def right : Expression}].right
                      "(EqualsTo | GreaterThan | LessThan)(" + left + "," + right + ")"
              
         val a = calculateExpr(left) match {
           case int(e) => e
           case _ => -1
         }
         val b = calculateExpr(right) match {
           case int(e) => e
           case _ => -1
         }
         var result : Boolean = aOrbOrc.getClass.getSimpleName  match{
                                    case "GreaterThan" => a> b
                                    case "LessThan" => a < b
                                    case "EqualsTo" => if( a != -1 &&  b != -1)
                                                        a==b
                                                      else{
                                                        if(a.equals(b) == 0)
                                                          true
                                                        else
                                                          false
                                                      }
                               }
         if(result)
           int(1)  // success
         else
           int(0) // failure
          
      }
      case Concat(a,b)=> {                                                      // concat  expression 
        val left = calculateExpr(a) match{
           case int(e) => e
           case string(e) => e
        }
        val right = calculateExpr(b) match{
          case int(e) =>e
          case string(e) =>e 
        }
        string(left.toString().concat(right.toString()))
      }
                              
   }
}
 
def interpretStmt(code :List[Statement]) {   
  if(! code.isEmpty){
        code.head match{
          case PrintStatement(value) => print(value)
                                        interpretStmt(code.tail)
          case VariableDefinition(name,value) => assignment(name,value)
                                                 interpretStmt(code.tail)
          case VariableDeclaration(name,datatype) => declaration(name,datatype)
                                                     interpretStmt(code.tail)
          case IfStatement(condition, truestmt, falsestmt) => ifelse(condition, truestmt, falsestmt)
                                                              interpretStmt(code.tail)
          case WhileLoop(condition, truestmt) => whileloop(condition, truestmt)
                                                 interpretStmt(code.tail)
      }
  }
}

def print(value: Expression) ={
  //println(calculateExpr(value))
  var result = calculateExpr(value) match{
                            case int(e) => println(e.toString())
                            case string(e) => println(e.toString())
                         }
  
}

def assignment(name:Expression , value: Expression) ={
 // println(calculateExpr(value))
  var result : Datatype = {
              calculateExpr(value)  match{
                case int(e) => int(e)
                case string(e) =>  string( e.toString())
              }
    }
  val varname = name.asInstanceOf[{def name : String}].name
                     "(ExpIdentifier)(" + name + ")" 
            CurrentScope.variableValue(varname) = result         // during assignment store the value in scope map
}


def declaration(name:Expression , datatype:String)={
   val varname = name.asInstanceOf[{def name : String}].name
                     "(ExpIdentifier)(" + name + ")" 
             var result :  Datatype= datatype match {
               case "Int" =>int(0)
               case "String" =>string("")
             }
            
            CurrentScope.variableValue(varname) = result 
}


def ifelse(condition: Expression, truestmt: List[Statement], falsestmt: List[Statement]) ={
  if( calculateExpr(condition) == int(1))            // 1 => success
             interpretStmt(truestmt)
           else if(! falsestmt.isEmpty)
             interpretStmt(falsestmt)
}


def whileloop(condition: Expression, truestmt: List[Statement]) ={
 
   while( calculateExpr(condition) == int(1))            // 1 => success
             interpretStmt(truestmt)
}


/*
    def interpretStmt(code :List[Statement]) {                         // program statement interpreter
      if(! code.isEmpty){
        code.head match{
          case PrintStatement(value) =>{                               // print statement{
            var result = calculateExpr(value) match{
                            case int(e) => println(e.toString())
                            case string(e) => println(e.toString())
                         }
            interpretStmt(code.tail)
          }
          case VariableDefinition(name,value) => {                   // assignment statement {
            var result : Datatype = {
              calculateExpr(value)  match{
                case int(e) => int(e)
                case string(e) =>  string( e.toString())
              }
            }
            val varname = name.asInstanceOf[{def name : String}].name
                     "(ExpIdentifier)(" + name + ")" 
            CurrentScope.variableValue(varname) = result         // during assignment store the value in scope map
            interpretStmt(code.tail)
          }
          case VariableDeclaration(name,datatype) =>{                    // assignment statement 
             val varname = name.asInstanceOf[{def name : String}].name
                     "(ExpIdentifier)(" + name + ")" 
             var result :  Datatype= datatype match {
               case "Int" =>int(0)
               case "String" =>string("")
             }
            
            CurrentScope.variableValue(varname) = result 
            interpretStmt(code.tail)
          }
         case IfStatement(condition, truestmt, falsestmt) =>{    // if else statement 
           if( calculateExpr(condition) == Right(1))            // 1 => success
             interpretStmt(truestmt)
           else if(! falsestmt.isEmpty)
             interpretStmt(falsestmt)
           
           interpretStmt(code.tail)
         }
         
         case WhileLoop(condition, truestmt) =>{                    // while loop
           while( calculateExpr(condition) == Right(1))            // 1 => success
             interpretStmt(truestmt)
           
           interpretStmt(code.tail)
         }
              
         case _ => ()
       }
            
      }
    }
  */        
          
         
}