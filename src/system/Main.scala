package system

import br.unb.cic.sma.sade.fipa._
import br.unb.cic.sma.sade.agent._
import agents._
import akka.actor._

object Main {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("CPGFMAS")
    val controlador = system.actorOf(Props(new Controlador(system)), name = "controlador")
    // start them going
    controlador ! ACLMessage(Map((ACLMessageParameter.PERFORMATIVE -> Performative.REQUEST),
      (ACLMessageParameter.SENDER -> system.deadLetters),
      (ACLMessageParameter.RECEIVER -> controlador),
      (ACLMessageParameter.CONTENT -> "C:\\Users\\gustavocgve\\Documents\\cartao\\cartao.csv")))
  }
}