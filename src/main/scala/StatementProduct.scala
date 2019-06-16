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
            mapOfRenamedVariabls is a ...
            HashMap of oldVariableName -> Array[ oldVaribaleDataType, RenamedVariable1, RenamedVariable2...]
            Example: for var a and 2 copies,  entry will be a -> Array(Int, a1, a2)

            mapOfActivationVariables is a ...
            HashMap of currentSet(INT) -> Array[ actVariableDataType, actVariable1, actVariable2...]
            currentSet refers to the relevant activation variables for the current scope
        */
        var mapOfRenamedVariables = mutable.Map[String, Array[String]]() 
        var mapOfActivationVariables = mutable.Map[Int, Array[String]]()
        val activationVariables: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()
        oldAST = oldAST ::: ast     //Contains the provided AST of single execution
        println("OldAST->\n" + oldAST)             
        val k = 2                   //Number of copies required
        var currentLevel = 0        //To keep note of current set of relevant activation variables
        newAST = createActivationVariables(newAST, k,mapOfActivationVariables, currentLevel)
        // println(mapOfActivationVariables)
        newAST = getModifiedAST(oldAST, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k, currentLevel)
        
        // println(mapOfRenamedVariables)
        // for(arr <- mapOfRenamedVariables.values)
        // {    println(arr(0))
        //      println(arr(1))
        //      println(arr(2))
        // }n
    

        println("New AST->\n" + newAST)
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
                        mapOfActivationVariables: mutable.Map[Int, Array[String]], currentLevel: Int, k: Int, maxLevelArg: Int): List[Statement] =
    {
        var newAST = newASTarg
        var maxLevel = maxLevelArg
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
                                                println(renamedVar)
                                            }
                                            mapOfRenamedVariables+=(name -> keyArray  )                                                                                                         
                                            // oldAST = oldAST.tail
                                            newAST = getModifiedAST(oldAST.tail, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k, maxLevel)

                
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
                                                println(newIfStmt)
                                            }

                                            newAST = getModifiedAST(oldAST.tail, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k, maxLevel)
                
                
                case IfStatement(condition, trueStmt, falseStmt) => 
                                            /* Create Fresh Activation Variables ( 2*k required) */
                                            newAST = createActivationVariables(newAST, k, mapOfActivationVariables, maxLevel+1 )
                                            newAST = createActivationVariables(newAST, k, mapOfActivationVariables, maxLevel+2 )
                                            var i = 0
                                            var j = 0
                                            
                                            /* Insert Activation Variable Assignments Statements */
                                            var originalActivationVariableArray = mapOfActivationVariables(currentLevel)
                                            var newActivationVarArray1 = mapOfActivationVariables(maxLevel+1)
                                            var newActivationVarArray2 = mapOfActivationVariables(maxLevel+2)

                                            for(i <- 1 to k)
                                            {
                                                var currentActVarName = originalActivationVariableArray(i)
                                                var newActVarName1 = newActivationVarArray1(i)
                                                var newActVarName2 = newActivationVarArray2(i)
                                                var checkActVarCondition = GreaterThan(ExpIdentifier(currentActVarName), Number(0))
                                                var newCondition = getRenamedValue(condition, mapOfRenamedVariables, i)
                                                var andStmt1 = And(checkActVarCondition ,newCondition )
                                                var andStmt2 = And(checkActVarCondition ,Not(newCondition) )

                                                var newActVarAssignment1 = VariableDefinition(ExpIdentifier(newActVarName1), andStmt1)
                                                var newActVarAssignment2 = VariableDefinition(ExpIdentifier(newActVarName2), andStmt2)
                                                newAST = newAST ::: List(newActVarAssignment1, newActVarAssignment2)
                                                println(List(newActVarAssignment1, newActVarAssignment2))
                                                
                                            }
                                            var nextLevel = maxLevel+1
                                            var nextToNextLevel = maxLevel+2
                                            maxLevel+=2
                                            newAST = getModifiedAST(trueStmt, newAST, mapOfRenamedVariables, mapOfActivationVariables, nextLevel, k, maxLevel)
                                            // println(newAST)
                                            newAST = getModifiedAST(falseStmt, newAST, mapOfRenamedVariables, mapOfActivationVariables, nextToNextLevel, k, maxLevel)
                                            // println(newAST)
                                            newAST = getModifiedAST(oldAST.tail, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k, maxLevel)
                case WhileLoop(condition, trueStmt) =>
                                            /* Create k fresh activation Variables */
                                            newAST = createActivationVariables(newAST, k, mapOfActivationVariables, maxLevel+1 )
                                            var originalActivationVariableArray = mapOfActivationVariables(currentLevel)
                                            var newActivationVarArray = mapOfActivationVariables(maxLevel+1)
                                            var checkActVarCondition1 = GreaterThan(ExpIdentifier(originalActivationVariableArray(1)), Number(0))
                                            var checkActVarCondition2 = GreaterThan(ExpIdentifier(originalActivationVariableArray(2)), Number(0))
                                            var andStmt1 = And(checkActVarCondition1, getRenamedValue(condition, mapOfRenamedVariables, 1))
                                            var andStmt2 = And(checkActVarCondition2, getRenamedValue(condition, mapOfRenamedVariables, 2))
                                            var newWhileCondition = Or(andStmt1, andStmt2)
                                            var i=0
                                            var j=0

                                            for ( i <- 3 to k)
                                            {
                                                var checkActVarCondition = GreaterThan(ExpIdentifier(originalActivationVariableArray(i)), Number(0))
                                                var andStmt = And(checkActVarCondition, getRenamedValue(condition, mapOfRenamedVariables, i))
                                                newWhileCondition = Or(newWhileCondition, andStmt)
                                            }

                                            var newTrueStmt: List[Statement] = List()
                                            for ( i <- 1 to k)
                                            {
                                                var newActVarName = newActivationVarArray(i)
                                                var checkActVarCondition = GreaterThan(ExpIdentifier(originalActivationVariableArray(i)), Number(0))
                                                var andStmt = And(checkActVarCondition, getRenamedValue(condition, mapOfRenamedVariables, i))
                                                var renamedStmt = VariableDefinition(ExpIdentifier(newActVarName), andStmt)
                                                newTrueStmt = newTrueStmt ::: List(renamedStmt)
                                            }
                                            var newWhileStmt = WhileLoop(newWhileCondition, newTrueStmt)
                                            newAST = newAST ::: List(newWhileCondition)
                                            var newLevel = maxLevel + 1
                                            maxLevel+=1
                                            newAST = getModifiedAST(trueStmt, newAST, mapOfRenamedVariables, mapOfActivationVariables, newLevel, k, maxLevel)
                                            newAST = getModifiedAST(oldAST.tail, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k, maxLevel)

                case _ => newAST = getModifiedAST(oldAST.tail, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k, maxLevel)
            }
        }
        return newAST
    }

    def getRenamedValue(expr: Expression, variableMap: mutable.Map[String, Array[String]], k: Int): Expression = 
    {
        //todo -> stoi and itos, add and, or, not to this
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
            case EqualsTo(opr1, opr2) => renamedExpr = EqualsTo(getRenamedValue(opr1, variableMap, k), getRenamedValue(opr2, variableMap, k))
            case _ => renamedExpr = expr

        }
        return renamedExpr
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
}