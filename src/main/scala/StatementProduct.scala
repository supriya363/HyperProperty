package ast
import model._
import parser._
import scala.collection.mutable
import scala.util.Try


object StatementProduct
{
    def makeTwoCopies(ast : List[Statement]) {

        var newAST: List[Statement] = List()
        var oldAST: List[Statement] = List()
        /*
            HashMap of oldVariableName -> Array[ oldVaribaleDataType, RenamedVariable1, RenamedVariable2...]
            Example: for var a and 2 copies,  entry will be a -> Array(Int, a1, a2)
        */
        var mapOfRenamedVariables = mutable.Map[String, Array[String]]() 
        var mapOfActivationVariables = mutable.Map[Int, Array[String]]()
        val activationVariables: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()
        oldAST = oldAST ::: ast     //Contains the provided AST of single execution
        println("OldAST->\n" + oldAST)             
        val k = 2                   //Number of copies required
        var currentLevel = 0        //To keep note of current set of relevant activation variables
        newAST = createActivationVariables(newAST, k,mapOfActivationVariables, currentLevel)
        println(mapOfActivationVariables)
        newAST = getModifiedAST(oldAST, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k)
        
        // println(mapOfRenamedVariables)
        // for(arr <- mapOfRenamedVariables.values)
        // {    println(arr(0))
        //      println(arr(1))
        //      println(arr(2))
        // }n
        println("New AST\n->" + newAST)
        // println(mapOfRenamedVariables)
        // println(mapOfRenamedVariables("a")(0))
        // val activationVariables: Array[Boolean] = Array()
        // var countOfActivationVariables = 0
        // countOfActivationVariables = countNewActivationVariables(activationVariables, countOfActivationVariables, ast, 2)
        // println(countOfActivationVariables)
        // while(!ast.empty())
        // {
        //     ast.head match {
        //         case VariableDefinition(varName, varValue) =>   //add if condition
        //                                                         countOfActivationVariables+=2

                                                                

        //     }
        // }
        
    }

    def getModifiedAST(oldAST: List[Statement], newASTarg: List[Statement], mapOfRenamedVariables: mutable.Map[String, Array[String]],
                        mapOfActivationVariables: mutable.Map[Int, Array[String]], currentLevel: Int, k: Int): List[Statement] =
    {
        var newAST = newASTarg
        if(!oldAST.isEmpty)
        {
            oldAST.head match
            {
                case VariableDeclaration(ExpIdentifier(name), dataType) =>  
                                            println("Declaration Case")
                                            var keyArray = new Array[String](k+1)
                                            keyArray(0) = dataType
                                            var i = 0
                                            /*  
                                                Insert renamed variables to the map
                                                Create variable declaration Statement for them
                                                Add it to new AST 
                                            */
                                            for(i <- 1 to k)
                                            {
                                                keyArray(i) = name+i.toString
                                                val renamedVar = VariableDeclaration(ExpIdentifier(keyArray(i)),dataType)
                                                newAST = newAST ::: List(renamedVar)
                                            }
                                            mapOfRenamedVariables+=(name -> keyArray  )                                                                                                         
                                            // oldAST = oldAST.tail
                                            newAST = getModifiedAST(oldAST.tail, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k)

                
                case VariableDefinition(ExpIdentifier(name), value)     =>  
                                            println("Definition Case")
                                            var keyArray = mapOfRenamedVariables(name)
                                            var activationVariableArray = mapOfActivationVariables(currentLevel)
                                            //TODO - Add Skip to the grammar
                                            
                                            for(i <- 1 to k)
                                            {   

                                                var renamedExpr = getRenamedValue(value, mapOfRenamedVariables, i)
                                                // println(renamedExpr)
                                                val renamedStmt = VariableDefinition(ExpIdentifier(keyArray(i)),renamedExpr)
                                                var activationVariable = activationVariableArray(i)
                                                var actVarCheckCondition = GreaterThan(ExpIdentifier(activationVariable), Number(0))
                                                var newIfStmt = IfStatement(actVarCheckCondition, List(renamedStmt), List()) //change it to skip
                                                newAST = newAST ::: List(newIfStmt)
                                            }

                                            newAST = getModifiedAST(oldAST.tail, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k)
                
                
                case IfStatement(condition, trueStmt, falseStmt) => 
                                            /* Create Fresh Activation Variables ( 2*k required) */
                                            newAST = createActivationVariables(newAST, k, mapOfActivationVariables, currentLevel+1 )
                                            newAST = createActivationVariables(newAST, k, mapOfActivationVariables, currentLevel+2 )
                                            var i =0

                                            /* Insert Activation Variable Assignments Statements */
                                            var originalActivationVariableArray = mapOfActivationVariables(currentLevel)
                                            var newActivationVarArray1 = mapOfActivationVariables(currentLevel+1)
                                            var newActivationVarArray2 = mapOfActivationVariables(currentLevel+2)
                                            for(i <- 1 to k)
                                            {
                                                var currentActVarName = originalActivationVariableArray(i)
                                                var newActVarName1 = newActivationVarArray1(i)
                                                var newActVarName2 = newActivationVarArray2(i)
                                                var checkActVarCondition1 = GreaterThan(ExpIdentifier(currentActVarName), Number(0))
                                                var checkActVarCondition2 = GreaterThan(ExpIdentifier(currentActVarName), Number(0))
                                                var newCondition = getRenamedValue(condition, mapOfRenamedVariables, i)
                                                var andStmt1 = And(checkActVarCondition1 ,newCondition )
                                                var andStmt2 = And(checkActVarCondition2 ,Not(newCondition) )

                                                var newActVarAssignment1 = VariableDefinition(ExpIdentifier(newActVarName1), andStmt1)
                                                var newActVarAssignment2 = VariableDefinition(ExpIdentifier(newActVarName2), andStmt2)
                                                newAST = newAST ::: List(newActVarAssignment1, newActVarAssignment2)
                                            }
                                            newAST = getModifiedAST(oldAST.tail, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k)

                case _ => newAST = getModifiedAST(oldAST.tail, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k)
            }
        }
        return newAST
    }

    def getRenamedValue(expr: Expression, variableMap: mutable.Map[String, Array[String]], k: Int): Expression = 
    {
        //todo -> stoi and itos
        var renamedExpr = expr
        expr match {
            case ExpIdentifier(name) => var keyArray = variableMap(name)
                                        renamedExpr = ExpIdentifier(keyArray(k))
            case Plus(opr1, opr2) =>    renamedExpr = Plus(getRenamedValue(opr1, variableMap, k), getRenamedValue(opr2, variableMap, k))
            case Minus(opr1, opr2) =>   renamedExpr = Minus(getRenamedValue(opr1, variableMap, k), getRenamedValue(opr2, variableMap, k))
            case Product(opr1, opr2) => renamedExpr = Product(getRenamedValue(opr1, variableMap, k), getRenamedValue(opr2, variableMap, k))
            case GreaterThan(opr1, opr2) => renamedExpr = GreaterThan(getRenamedValue(opr1, variableMap, k), getRenamedValue(opr2, variableMap, k))
            case LessThan(opr1, opr2) =>    renamedExpr = LessThan(getRenamedValue(opr1, variableMap, k), getRenamedValue(opr2, variableMap, k))
            case Concat(opr1, opr2) => renamedExpr = Concat(getRenamedValue(opr1, variableMap, k), getRenamedValue(opr2, variableMap, k))
            case _ => renamedExpr = expr

        }
        return renamedExpr
    }

    def renameExpression()
    {

    }

    def createActivationVariables(newASTarg: List[Statement], k: Int, actVariableMap: mutable.Map[Int, Array[String]], level: Int): List[Statement] =
    {
        var i = 0
        var datatype = "Int" //CHANGE TO BOOLEAN LATER
        var keyArray = new Array[String](k+1)
        keyArray(0) = datatype
        // var newAST: List[Statement] = List()
        var newAST = newASTarg
        for ( i <- 1 to k)
        {
            val stmt = VariableDeclaration(ExpIdentifier("actVar"+level+"_"+i.toString), datatype)
            newAST = newAST ::: List(stmt)
            keyArray(i) = "actVar"+level+"_"+i.toString
            
        }
        actVariableMap+=(level -> keyArray )
        return newAST
    }

    // def countNewActivationVariables(actVarList: Array[Boolean], countOfActivationVariables: Int, ast: List[Statement], k: Int ): Int =
    // {   
    //     if(!ast.empty())
    //     {
    //         ast.head match {
    //             // case VariableDefinition(varName, varValue) => cou
    //             case IfStatement(condition, trueStmt, falseStmt) => countOfActivationVariables++
    //             case WhileLoop(condition, trueStmt) => countOfActivationVariables++
    //         }
    //     }
    //     // while(!ast.empty())
    //     // {
    //     //     ast.head match {
    //     //         case VariableDefinition(varName, varValue) =>   //countOfActivationVariables+=k
    //     //         case IfStatement(condition, trueStmt, falseStmt) => countOfActivationVariables+=k
    //     //         case WhileLoop(condition, trueStmt) => 

    //     // }
    //     val abc: Int = 10
    //     return abc
    // }
}