package System

import akka.actor.Actor
import akka.kernel.Bootable
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import akka.actor.Props
import akka.routing.RoundRobinRouter
import java.net.InetAddress

case class finalsolution(result:Array[Int])
case class range(start:Long,end:Long,size:Long)

class Worker extends Actor {


  var diff:Long = 0                            //Intialise variables needed
  var sqroot:Double = 0

  
  
  def sumnsq(n:Long):Long = {
  
    (n*(n+1)*(2*n+1))/6                              //Calculates the sum of first n square multiplied by six
  
  }
  

  def receive ={
  
    case range(start1,end1,size1)=>{ 

      var solution = Array[Long]()
     
      var start = start1
      var end = end1
      var size = size1
      

       while(start<=end)   {                                 //Loops through the range to find a sequence
        
        diff = sumnsq(start+size-1) - sumnsq(start-1)      // It used the mathematical formula
        sqroot = math.sqrt(diff)                                         
        if(sqroot.isValidInt){                                           //The square root is checked to be a valid 
          solution = solution :+ start                                          // integer and if so is sent to the supervisor
        }
        start = start + 1
     }
      


      sender ! finalsolution(solution)

      }
    
   } 
  
    
}




class ClientApplication extends Bootable {
  
  val numOfWorkers = 200
  
  val system = ActorSystem("ClientApplication",ConfigFactory.load.getConfig("clientapp"))


  val Worker = system.actorOf(Props[Worker].withRouter
      (RoundRobinRouter(nrOfInstances = numOfWorkers)),"worker") 
  
  
    
   def startup() {
  }

  def shutdown() {
    system.shutdown()
  }
}


object ClientApp {
  
  def main(args: Array[String]) {
    
    new ClientApplication
    println("ClientApplication Started")
  }
  

}