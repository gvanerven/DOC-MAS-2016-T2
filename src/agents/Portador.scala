package agents

import br.unb.cic.sma.sade.agent._
import br.unb.cic.sma.sade.fipa._
import model._
import org.apache.commons.math3.ml.clustering._
import org.apache.commons.math3.stat.descriptive.{ DescriptiveStatistics, _ }
import stats.GeraEstatisticas

class Portador(estatisticasGlobais: EstatisticasTransacoes, transacoes: Array[TransacaoCPGF], vlCorte: Double) extends Agent {
  val bpmSaques = estatisticasGlobais.bloxPlotMarksSaques
  val bpmCompras = estatisticasGlobais.bloxPlotMarksCompras
  
  def receive = {
    case msg: ACLMessage =>
      //println(msg.content.toString)
      //println(estatisticasGlobais)
      println(this.self + " num tran:" + transacoes.length)
      val estatisticasLocais = GeraEstatisticas.gerarEstatisticasTransacoes(transacoes)
      val estLocaisSaques = estatisticasLocais(0)
      val estLocaisCompras = estatisticasLocais(1)
      val perSaques = (100 * estLocaisSaques.totalElements) / (100 * (estLocaisSaques.totalElements + estLocaisCompras.totalElements))
      val perCompras = (100 * estLocaisCompras.totalElements) / (100 * (estLocaisSaques.totalElements + estLocaisCompras.totalElements))
      val perOutSaquesBaixo = (100 * transacoes.filter(_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao < estLocaisSaques.minWhisker)) / (100 * estLocaisSaques.totalElements)
      val perOutSaquesAlto = (100 * transacoes.filter(_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > estLocaisSaques.maxWhisker)) / (100 * estLocaisSaques.totalElements)
      val perOutComprasBaixo = (100 * transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao < estLocaisCompras.minWhisker)) / (100 * estLocaisCompras.totalElements)
      val perOutComprasAlto = (100 * transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > estLocaisCompras.maxWhisker)) / (100 * estLocaisCompras.totalElements)
      val perOpComprasCorte = (100 * transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > vlCorte)) / (100 * estLocaisCompras.totalElements)
      val estLocaisFinal = EstatisticasTransacoes(
        perSaques,
        perCompras,
        perOutSaquesBaixo,
        perOutSaquesAlto,
        perOutComprasBaixo,
        perOutComprasAlto,
        perOpComprasCorte,
        estLocaisSaques,
        estLocaisCompras)
        
      val perSaquesLG = (100 * estLocaisSaques.totalElements) / (100 * bpmSaques.totalElements)
      val perComprasLG = (100 * estLocaisCompras.totalElements) / (100 * bpmCompras.totalElements)
      val perOutSaquesBaixoLG = (100 * transacoes.count(e => e.tipoTransacao.contains("SAQUE") && e.valorTransacao < bpmSaques.minWhisker)) / (100 * estLocaisSaques.totalElements)
      val perOutSaquesAltoLG = (100 * transacoes.count(e => e.tipoTransacao.contains("SAQUE") && e.valorTransacao > bpmSaques.maxWhisker)) / (100 * estLocaisSaques.totalElements)
      val perOutComprasBaixoLG = (100 * transacoes.count(e => !e.tipoTransacao.contains("SAQUE") && e.valorTransacao < bpmCompras.minWhisker)) / (100 * estLocaisCompras.totalElements)
      val perOutComprasAltoLG = (100 * transacoes.count(e => !e.tipoTransacao.contains("SAQUE") && e.valorTransacao > bpmCompras.maxWhisker)) / (100 * estLocaisCompras.totalElements)
      val perOpComprasCorteLG = (100 * transacoes.count(e => !e.tipoTransacao.contains("SAQUE") && e.valorTransacao > vlCorte)) / (100 * estLocaisCompras.totalElements)
      val estLGFinal = EstatisticasTransacoes(
        perSaquesLG,
        perComprasLG,
        perOutSaquesBaixoLG,
        perOutSaquesAltoLG,
        perOutComprasBaixoLG,
        perOutComprasAltoLG,
        perOpComprasCorteLG,
        estLocaisSaques,
        estLocaisCompras)        
      println(estLGFinal)
      
      val score = (perSaques +  perCompras + perOutSaquesBaixo + perOutSaquesAlto + perOutComprasBaixo + perOutComprasAlto + perOpComprasCorteLG)/7
      val reply = if(score >= 1.5) { println(estLGFinal); msg.reply(Performative.INFORM, "culpado")} else {msg.reply(Performative.INFORM, "inocente")}
      println(reply.sender)
      println(reply.receiver)
      reply.receiver ! reply
  }

}