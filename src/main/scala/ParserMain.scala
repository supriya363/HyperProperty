import scala.util.Try
import scala.io.Source
import interpreter._
import model._
import parser._
import typecheck._
import errorinfo._

object ParserMain 
{
    def main(args : Array[String]): Unit = {  
      // var inputfile = Source.fromFile(args(0))                               // read code from input file 
      var inputfile = Source.fromFile("code/sumton.txt")  
      val inputfilecontent   = inputfile.mkString
      ScalaParser.parseStatement(inputfilecontent) match{                                          //  Parser (AST generation){
         case Some(text) =>{
           println(text)                                                                          // print AST
           var result = TypeCheckVisitor.typecheck(text)                                          //   Type check
           if( result == "")                                             
               ScalaInterpreter.interpretStmt(text.asInstanceOf[List[model.Statement]])          //   Interpreter
           else
             println(result)
          }
         case _ => Nil
       }
    }
}  















































