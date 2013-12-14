package System

import akka.actor.Actor
import akka.kernel.Bootable
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import akka.actor.Props
import java.net.InetAddress
import akka.routing.RoundRobinRouter

case class finalsolution(result:Array[Long])


class Server(val args:Array[String]) extends Actor {
 
 
  val end = args(0).toLong
  var jump = 1000000.toLong
  val packet = ((end.toDouble) / jump).ceil.toInt
  var size1 = args(1).toLong
  var noofresult = 0 
  val noOfWorker = 4
  var flag = 0
  
//  val remotePath = "akka.tcp://ClientApplication@localhost:2552/user/worker"
   val remotePath = "akka.tcp://ClientApplication@"
   val workerPath = ":2552/user/worker"
  
  var Worker = Array[akka.actor.ActorSelection]()                              // Initialise an
 
//    var Worker = context.actorSelection(remotePath)
   
    
     Worker = Worker :+context.actorSelection(remotePath+"128.227.248.167"+workerPath)                    // Workers
     Worker = Worker :+context.actorSelection(remotePath+"128.227.248.168"+workerPath)                    // Workers
     Worker = Worker :+context.actorSelection(remotePath+"128.227.248.169"+workerPath)
     Worker = Worker :+context.actorSelection(remotePath+"128.227.248.170"+workerPath)
    
    
    // for(a <- 0 until numOfWorkers)  {                                       // array of
  //   Worker = Worker :+context.actorSelection(remotePath)                    // Workers
 //}
   
   for(a<- 1 until packet){                                        //Send workers the range they need 
   if(flag==4)
     flag = 0
    val rng = getRange(a,jump,size1)
    val start = rng(0)
    val end = rng(1)
    val size = rng(2)
    Worker(flag) !  range(start,end,size)                                 // to calculate
    flag =flag + 1
   }
  
  
  
  Worker(flag) ! range(1+((packet-1)*jump),end,size1)      // The last worker is an exception
   

  def getRange(actor_no:Int,jump_size:Long,size:Long):List[Long] ={        // Calculates range for a worker
                                                                        // Using its index in the array
    List( 1+(actor_no-1)*jump_size, actor_no*jump_size, size)   
  
  }
  
  
 def receive ={                                                        //Supervisor receive method
   
   case finalsolution(result) =>
     
     for(a <- result)
      println(a)

     noofresult = noofresult + 1
   
     
     if(noofresult == packet)  {
       context.system.shutdown()
     }
      
 }


}


class ServerApplication(val args: Array[String]) extends Bootable {
 
  val system = ActorSystem("ServerApplication",ConfigFactory.load.getConfig("serverapp"))

  val server = system.actorOf(Props(new Server(args)), "server")

   def startup() {
  }

  def shutdown() {
    system.shutdown()
  }
}

object ServerApp {
  
  
   def main(args: Array[String]) {
    
    new ServerApplication(args)
    println("ServerApplication Started")
  }
  

}