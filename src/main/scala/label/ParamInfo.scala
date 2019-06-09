package label
import model._

class ParamInfo{
  var datatype : String =""
  var label  = new Label()
  
  def setDatatype(datatype : String)={
    this.datatype = datatype
  }
  def setlabel(lebel : Set[String]) ={
    this.label = label
  }
}
