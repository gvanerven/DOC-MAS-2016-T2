package agents

import br.unb.cic.sma.sade.agent._
import br.unb.cic.sma.sade.fipa._
import model._
import org.apache.commons.math3.ml.clustering._
import org.apache.commons.math3.stat.descriptive.{DescriptiveStatistics, _}

class Portador(estatisticasGlobais: EstatisticasTransacoes, transacoes: Array[TransacaoCPGF]) extends Agent {
  def receive = {
    case msg: ACLMessage =>
      println(msg.content.toString)
      println(estatisticasGlobais)
      println(transacoes.length)
      val reply = msg.reply(Performative.INFORM, "culpado")
      println(reply.sender)
      println(reply.receiver)
      reply.receiver ! reply
  }

}