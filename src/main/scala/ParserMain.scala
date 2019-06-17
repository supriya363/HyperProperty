import scala.util.Try
import scala.io.Source
import interpreter._
import model._
import parser._
import typecheck._
import errorinfo._
import ast._

object ParserMain 
{
    def main(args : Array[String]): Unit = {  
      // var inputfile = Source.fromFile(args(0))                               // read code from input file 
      var inputfile = Source.fromFile("code/test2.txt")  
      val inputfilecontent   = inputfile.mkString
      ScalaParser.parseStatement(inputfilecontent) match{                                          //  Parser (AST generation){
         case Some(text) =>{
         //   println(text) 
                                                                                    // print AST
           var result = TypeCheckVisitor.typecheck(text)                                          //   Type check
           if( result == "")    
           {                                         
              ScalaInterpreter.interpretStmt(text.asInstanceOf[List[model.Statement]])          //   Interpreter
              println("Enter number of copies required: ")
              var noOfCopies = readInt()
              StatementProduct.constructStatementProduct(text, noOfCopies)
           }
           else
           {
              println(result)
           }
          }
         case _ => Nil
       }
    }
}  















































