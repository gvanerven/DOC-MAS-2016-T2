package agents

import br.unb.cic.sma.sade.agent._
import br.unb.cic.sma.sade.fipa._
import model._
import org.apache.commons.math3.ml.clustering._
import org.apache.commons.math3.stat.descriptive.{ DescriptiveStatistics, _ }
import stats.GeraEstatisticas

class Portador(estatisticasGlobais: EstatisticasTransacoes, transacoes: Array[TransacaoCPGF], vlCorte: Double) extends Agent {
  def receive = {
    case msg: ACLMessage =>
      println(msg.content.toString)
      println(estatisticasGlobais)
      println(transacoes.length)
      val estatisticasLocais = GeraEstatisticas.gerarEstatisticasTransacoes(transacoes)
      val estLocaisSaques = estatisticasLocais(0)
      val estLocaisCompras = estatisticasLocais(1)
      val perSaques = (100 * estLocaisSaques.totalElements) / (100 * (estLocaisSaques.totalElements + estLocaisCompras.totalElements))
      val perCompras = (100 * estLocaisCompras.totalElements) / (100 * (estLocaisSaques.totalElements + estLocaisCompras.totalElements))
      val perOutSaquesBaixo = (100 * transacoes.filter(_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao < estLocaisSaques.minWhisker)) / (100 * estLocaisSaques.totalElements)
      val perOutSaquesAlto = (100 * transacoes.filter(_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > estLocaisSaques.maxWhisker)) / (100 * estLocaisSaques.totalElements)
      println(transacoes.filter(_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > estLocaisSaques.maxWhisker))
      val perOutComprasBaixo = (100 * transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao < estLocaisCompras.minWhisker)) / (100 * estLocaisCompras.totalElements)
      val perOutComprasAlto = (100 * transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > estLocaisCompras.maxWhisker)) / (100 * estLocaisCompras.totalElements)
      val perOpComprasCorte = (100 * transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > 800)) / (100 * estLocaisCompras.totalElements)
      println(transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > 800))
      val reply = msg.reply(Performative.INFORM, "culpado")
      println(reply.sender)
      println(reply.receiver)
      reply.receiver ! reply
  }

}