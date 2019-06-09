package typecheck

import interpreter._
import model._
import parser._
import label._
import errorinfo._


object TypeCheckVisitor {
  var symboltable = new ParamTable()
  var err = new ErrorInfo()
  
  def getexptype(exp : Expression) :Int ={
    exp match{
      case Number(n) => 1                                   // number
      case ExpIdentifier(n) =>{                           // identifier --- check from symbol table;  1 => int , 2 => string, 0 => not declared{
         if( symboltable.paramSymTable.contains(n)){ 
            var datatype : String =     symboltable.paramSymTable.get(n).get.datatype 
            //println(datatype)
            if(datatype.equals("Int"))
              1
            else if(datatype.equals("String") )
              2
            else
              0
         }
         else{
            err.addinfo("variable  " + n + "  not declared")
           0
        }
     }
     case ExpStringLit(n) => 2                            //  stringlit
     case Itos(n) => 2
     case Stoi(s) => 1
     case Concat(left, right)=>{
        var leftType : Int = getexptype(left)
        var rightType : Int = getexptype(right)
        left match{
          case ExpStringLit(n) => right match{
            case Number(n) => err.addinfo("type mismatch" )
                                      0
            case _ => 1
          }
          case Number(n) => right match{
            case ExpStringLit(n) => err.addinfo("type mismatch" )
                                      0
            case _ => 1
          }
          case _ => 1
        }
     }
     case aOrbOrc @(EqualsTo(_,_) | GreaterThan(_,_) | LessThan(_,_) | Plus(_,_) | Minus(_,_) | Product(_,_)) =>{      // condition and arithmatic op{
        val  left = aOrbOrc.asInstanceOf[{def left : Expression}].left
                    "(EqualsTo | GreaterThan | LessThan | Plus | Minus | Product | Concat)(" + left + "," 
             
                   
        var leftType : Int = getexptype(left)            
        
        val  right = aOrbOrc.asInstanceOf[{def right : Expression}].right
        
        var rightType : Int = getexptype(right)  
       
       // println(leftType)
       // println(rightType)
        if (leftType != 0 &&  leftType == rightType)
          1
        else{
          err.addinfo("type mismatch" )
          0
       }
     }
     
   }
 }
  
  
  
 def validateLabel(left : Expression, right : Expression) : Boolean ={
   val leftvarname = left.asInstanceOf[{def name : String}].name
                       "(ExpIdentifier)(" + left + ")"
   var leftLabel = symboltable.paramSymTable(leftvarname).label.getLabel
   var leftOwner = leftLabel.keySet
  
   val readerset = Set("usr","chkr")
   val owner : String = "chkr"
   
   var rightLabelobj = new Label()
   rightLabelobj.addLabel(owner, readerset)
   
   var rightOwner = rightLabelobj.getLabel.keySet
  // println("left label = "+owner+": {" + leftLabel.get(owner).get.mkString(",") +"}")
   //println("right label = "+owner+": {" + rightLabelobj.getLabel.get(owner).get.mkString(",") +"}")
   var isValid : Boolean = true
   
   if ( rightOwner.subsetOf(leftOwner)){
     for( owner <- rightOwner)
     {
       
       
       var leftreader = leftLabel.get(owner).get
       var rightreader = rightLabelobj.getLabel.get(owner).get
       
       
       if(! leftreader.subsetOf(rightreader)){
           isValid = false
          
       }
       
     }
   }
   else{
     isValid = false
   }
   
   
   
   isValid
 }
  
  def typecheck(code : List[Statement]) : String  = {
    if(! code.isEmpty){
      code.head match{
        case PrintStatement(value) => {                    // print stmt  --- 0 => right exp type failed (arithmatic op case) , otherwise passed{
           getexptype(value) match {
            case 0 =>  err.addinfo("err at print statement")
            case _ =>  typecheck(code.tail)
          }
          err.getinfo().toString()  
         }
        
        case VariableDeclaration(name,datatype) =>{            // var declare stmt  ----- data type inseretd into symbol table {
          val varname = name.asInstanceOf[{def name : String}].name
                       "(ExpIdentifier)(" + name + ")"
          val readerset = Set("chkr")
          //println(datatype)
          val param = new ParamInfo()
          symboltable.paramSymTable(varname) = param
          symboltable.paramSymTable(varname).setDatatype(datatype)
          symboltable.paramSymTable(varname).label.addLabel("chkr", readerset)
          typecheck(code.tail)
        }
       case VariableDefinition(name,value) =>{            // assignment stmt  ----- 0 => variable not declared, 1 => int , 2 => string{
          val varname = name.asInstanceOf[{def name : String}].name
                       "(ExpIdentifier)(" + name + ")"
                       
          if(! symboltable.paramSymTable.contains(varname))
            err.addinfo("variable " + varname + " not declared")
          else{
            getexptype(value) match {
              case 0 =>  err.addinfo("err at assignment")
              case 1 =>  if(  symboltable.paramSymTable.get(varname).get.datatype.equals("Int"))
                           typecheck(code.tail)
                         else
                           err.addinfo("String value can only be assigned to " + varname)
                        
             case 2 =>  if( symboltable.paramSymTable.get(varname).get.datatype.equals("String"))
                           typecheck(code.tail)
                         else
                           err.addinfo("Int value can only be assigned to "+ varname)
            }
          }
          
          if(!validateLabel(name,value))
            err.addinfo("label mismatch for var " + varname)
          err.getinfo().toString()  
        }
         
        case IfStatement(condition, truestmt, falsestmt) =>{       // if else stmt   ---  0 => condition failed , 1 => condition check passed
          getexptype(condition) match {
            case 0 =>  err.addinfo("err at if else")
            case 1 =>  typecheck(truestmt)
                       typecheck(falsestmt)
          }
                
          err.getinfo().toString()
        }
        
        case WhileLoop(condition, truestmt) =>{
          getexptype(condition) match {
            case 0 =>  err.addinfo("err in while loop")
            case 1 =>  typecheck(truestmt)
          }
                
         err.getinfo().toString()
        }
       
        case _ => typecheck(code.tail)
        
      }
    }
    err.getinfo().toString()
    
  }
  


}