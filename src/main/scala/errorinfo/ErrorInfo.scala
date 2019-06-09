package errorinfo

class ErrorInfo()
{
    var info : String = ""
    
    def addinfo(newinfo :String) : String ={     // add error to list
        info = info + newinfo + "\n" 
        return info.toString()
    }
  
    def getinfo() : String = {                // get error from list{
        return info
    }
}