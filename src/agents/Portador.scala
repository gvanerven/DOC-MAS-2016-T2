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
  
  def max(a: Double, b: Double): Double = {
    if(a > b) a else b
  }
  
  def receive = {
    case msg: ACLMessage =>
      //println(msg.content.toString)
      //println(estatisticasGlobais)
      val estatisticasLocais = GeraEstatisticas.gerarEstatisticasTransacoes(transacoes)
      val estLocaisSaques = estatisticasLocais(0)
      val estLocaisCompras = estatisticasLocais(1)
      val perSaques = (100 * estLocaisSaques.totalElements) / (100 * max((estLocaisSaques.totalElements + estLocaisCompras.totalElements),1))
      val perCompras = (100 * estLocaisCompras.totalElements) / (100 * max((estLocaisSaques.totalElements + estLocaisCompras.totalElements),1))
      val perOutSaquesBaixo = (100 * transacoes.filter(_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao < estLocaisSaques.minWhisker)) / (100 * max(estLocaisSaques.totalElements,1))
      val perOutSaquesAlto = (100 * transacoes.filter(_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > estLocaisSaques.maxWhisker)) / (100 * max(estLocaisSaques.totalElements,1))
      val perOutComprasBaixo = (100 * transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao < estLocaisCompras.minWhisker)) / (100 * max(estLocaisCompras.totalElements,1))
      val perOutComprasAlto = (100 * transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > estLocaisCompras.maxWhisker)) / (100 * max(estLocaisCompras.totalElements,1))
      val perOpComprasCorte = (100 * transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > vlCorte)) / (100 * max(estLocaisCompras.totalElements,1))
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
        
      val perSaquesG = (100 * estLocaisSaques.totalElements) / (100 * max(bpmSaques.totalElements,1))
      val perComprasG = (100 * estLocaisCompras.totalElements) / (100 * max(bpmCompras.totalElements,1))
      val perOutSaquesBaixoG = (100 * transacoes.count(e => e.tipoTransacao.contains("SAQUE") && e.valorTransacao < bpmSaques.minWhisker)) / (100 * max(estLocaisSaques.totalElements,1))
      val perOutSaquesAltoG = (100 * transacoes.count(e => e.tipoTransacao.contains("SAQUE") && e.valorTransacao > bpmSaques.maxWhisker)) / (100 * max(estLocaisSaques.totalElements,1))
      val perOutComprasBaixoG = (100 * transacoes.count(e => !e.tipoTransacao.contains("SAQUE") && e.valorTransacao < bpmCompras.minWhisker)) / (100 * max(estLocaisCompras.totalElements,1))
      val perOutComprasAltoG = (100 * transacoes.count(e => !e.tipoTransacao.contains("SAQUE") && e.valorTransacao > bpmCompras.maxWhisker)) / (100 * max(estLocaisCompras.totalElements,1))
      val perOpComprasCorteG = (100 * transacoes.count(e => !e.tipoTransacao.contains("SAQUE") && e.valorTransacao > vlCorte)) / (100 * max(estLocaisCompras.totalElements,1))

      val estGFinal = EstatisticasTransacoes(
        perSaquesG,
        perComprasG,
        perOutSaquesBaixoG,
        perOutSaquesAltoG,
        perOutComprasBaixoG,
        perOutComprasAltoG,
        perOpComprasCorteG,
        estLocaisSaques,
        estLocaisCompras)        
      //println(this.self + ":" + estLGFinal +  " num tran:" + transacoes.length)
      
      val scoreL = perSaques +  perCompras + perOutSaquesBaixo + perOutSaquesAlto + perOutComprasBaixo + perOutComprasAlto + perOpComprasCorte
      val scoreG = perSaquesG +  perComprasG + perOutSaquesBaixoG + perOutSaquesAltoG + perOutComprasBaixoG + perOutComprasAlto + perOpComprasCorteG
      val score = scoreL + scoreG
      println(this.self.toString.split("/")(4) + " score: " + score)
      val reply = if(score >= 1.3) { println(this.self.toString.split("/")(4) + estGFinal) ; msg.reply(Performative.INFORM, "culpado") } else { msg.reply(Performative.INFORM, "inocente") }
      //println(reply.sender)
      //println(reply.receiver)
      reply.receiver ! reply
  }

}