package ast
import model._
import parser._
import scala.collection.mutable
import scala.util.Try


object StatementProduct
{
    def constructStatementProduct(ast : List[Statement], noOfCopies: Int) {

        var newAST: List[Statement] = List()
        var oldAST: List[Statement] = List()
        /*
            mapOfRenamedVariabls is a ...
            HashMap of oldVariableName -> Array[ oldVaribaleDataType, RenamedVariable1, RenamedVariable2...]
            Example: for var a and 2 copies,  entry will be a -> Array(Int, a1, a2)

            mapOfActivationVariables is a ...
            HashMap of Int Value denoting current Set -> Array[ actVariableDataType, actVariable1, actVariable2...]
            currentSet refers to the relevant activation variables for the current scope
        */
        var mapOfRenamedVariables = mutable.Map[String, Array[String]]() 
        var mapOfActivationVariables = mutable.Map[Int, Array[String]]()
        val activationVariables: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()
        var maxLevelMap = mutable.Map[Int, Int]() //used to keep track of maximum number of activation variables alreday created
        maxLevelMap+=(0 -> 0)
        oldAST = oldAST ::: ast     //Contains the provided AST of single execution
        println("OldAST->\n" + oldAST)             
        val k = noOfCopies                   //Number of copies required
        var currentLevel = 0        //To keep note of current set of relevant activation variables

        newAST = createActivationVariables(newAST, k,mapOfActivationVariables, currentLevel) 

        newAST = getModifiedAST(oldAST, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k, maxLevelMap)
        println("New AST->\n\n" + newAST)       
    }

    def getModifiedAST(oldAST: List[Statement], newASTarg: List[Statement], mapOfRenamedVariables: mutable.Map[String, Array[String]],
                        mapOfActivationVariables: mutable.Map[Int, Array[String]], currentLevel: Int, k: Int, maxLevelMap: mutable.Map[Int,Int]): List[Statement] =
    {
        var newAST = newASTarg
        var maxLevel = maxLevelMap(0)
        if(!oldAST.isEmpty)
        {
            oldAST.head match
            {
                case VariableDeclaration(ExpIdentifier(name), dataType) =>  
                                            println("\n******Declaration Case*******")

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
                                                keyArray(i) = name + i.toString
                                                val renamedVar = VariableDeclaration(ExpIdentifier(keyArray(i)),dataType)
                                                newAST = newAST ::: List(renamedVar)
                                                println(renamedVar)
                                            }
                                            mapOfRenamedVariables+=(name -> keyArray  )                                                                                                         
                                            newAST = getModifiedAST(oldAST.tail, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k, maxLevelMap)

                
                case VariableDefinition(ExpIdentifier(name), value)     =>  
                                            println("\n*******Definition Case*******")

                                            var keyArray = mapOfRenamedVariables(name)
                                            var activationVariableArray = mapOfActivationVariables(currentLevel)
                                            //TODO - Add Skip to the grammar if required
                                            /* Add If statement checking if relevant activation variable is active before the assignment 
                                                Change the original variable names to their renamed values */
                                            for(i <- 1 to k)
                                            {   

                                                var renamedExpr = getRenamedValue(value, mapOfRenamedVariables, i)
                                                val renamedStmt = VariableDefinition(ExpIdentifier(keyArray(i)),renamedExpr)
                                                var activationVariable = activationVariableArray(i)
                                                var actVarCheckCondition = GreaterThan(ExpIdentifier(activationVariable), Number(0)) //if actVar > 0
                                                var newIfStmt = IfStatement(actVarCheckCondition, List(renamedStmt), List()) //change it to skip if required
                                                newAST = newAST ::: List(newIfStmt)
                                                println("Added-> " + newIfStmt)
                                            }

                                            newAST = getModifiedAST(oldAST.tail, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k, maxLevelMap)
                
                
                case IfStatement(condition, trueStmt, falseStmt) => 
                                            println("\n*********If Case**********")
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
                                                print("Added-> ")
                                                println(List(newActVarAssignment1, newActVarAssignment2))
                                                
                                            }
                                            var nextLevel = maxLevel+1  // level corresponds to new set of activation variables relevant for the
                                                                        // currently executing code
                                            var nextToNextLevel = maxLevel+2
                                            maxLevel+=2
                                            maxLevelMap(0) = maxLevel
                                            newAST = getModifiedAST(trueStmt, newAST, mapOfRenamedVariables, mapOfActivationVariables, nextLevel, k, maxLevelMap)
                                            // println(newAST)
                                            newAST = getModifiedAST(falseStmt, newAST, mapOfRenamedVariables, mapOfActivationVariables, nextToNextLevel, k, maxLevelMap)
                                            // println(newAST)
                                            newAST = getModifiedAST(oldAST.tail, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k, maxLevelMap)


                case WhileLoop(condition, trueStmt) =>
                                            println("\n**********While Case********")
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
                                                println("\nNew While Condition -> " + newWhileCondition + "\n")
                                            }


                                            var newTrueStmt: List[Statement] = List()
                                            for ( i <- 1 to k)
                                            {
                                                var newActVarName = newActivationVarArray(i)
                                                var checkActVarCondition = GreaterThan(ExpIdentifier(originalActivationVariableArray(i)), Number(0))
                                                var andStmt = And(checkActVarCondition, getRenamedValue(condition, mapOfRenamedVariables, i))
                                                var renamedStmt = VariableDefinition(ExpIdentifier(newActVarName), andStmt)
                                                newTrueStmt = newTrueStmt ::: List(renamedStmt)
                                                println("New true stmt Added inside While-> " + renamedStmt)
                                            }
                                            var newLevel = maxLevel + 1
                                            maxLevel+=1
                                            maxLevelMap(0) = maxLevel
                                            var tempWhileAST : List[Statement] = List()
                                            println("Adding to temporary AST to be inserted into WHILE ")
                                            tempWhileAST = getModifiedAST(trueStmt, tempWhileAST, mapOfRenamedVariables, mapOfActivationVariables, newLevel, k, maxLevelMap)
                                            println("New true stmt Added inside while -> " + tempWhileAST + "\n")
                                            var finalTrueStmt = newTrueStmt ::: tempWhileAST
                                            var newWhileStmt = WhileLoop(newWhileCondition, finalTrueStmt)
                                            println("While statement formed")
                                            println("Added-> " + newWhileStmt)
                                            newAST = newAST ::: List(newWhileStmt)
                                        
                                            newAST = getModifiedAST(oldAST.tail, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k, maxLevelMap)

                case _ => newAST = getModifiedAST(oldAST.tail, newAST, mapOfRenamedVariables, mapOfActivationVariables, currentLevel, k, maxLevelMap)
            }
        }
        return newAST
    }


    /* It renames the expression to replace original variables with their relevant renamed identifiers */
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

    /* Creates k fresh activation variables, adds their declarations to the AST, adds them to the activationVariableMap */
    def createActivationVariables(newASTarg: List[Statement], k: Int, actVariableMap: mutable.Map[Int, Array[String]], level: Int): List[Statement] =
    {
        var i = 0
        var datatype = "Int" //change to boolean later if req
        var keyArray = new Array[String](k+1)
        keyArray(0) = datatype

        var newAST = newASTarg

        /* Activation Variables are named in this manner 
            actVari_j --> where i corresponds to level and j corresponds to the jth copy of execution
        */

        for ( i <- 1 to k)
        {
            val stmt = VariableDeclaration(ExpIdentifier("actVar"+level+"_"+i.toString), datatype)
            println("Added -> " + stmt)
            newAST = newAST ::: List(stmt)
            keyArray(i) = "actVar"+level+"_"+i.toString
            
        }
        actVariableMap+=(level -> keyArray )
        return newAST
    }
}