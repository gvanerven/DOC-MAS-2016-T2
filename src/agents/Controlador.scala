package agents

import br.unb.cic.sma.sade.agent._
import br.unb.cic.sma.sade.fipa._
import model._
import akka.actor._
import java.util.GregorianCalendar
import org.apache.commons.math3.ml.clustering._
import org.apache.commons.math3.stat.descriptive.{ DescriptiveStatistics, _ }
import stats._
import scala.util.Random

class Controlador(system: ActorSystem) extends Agent {
  var transacoesSAQUES: Array[TransacaoCPGF] = Array[TransacaoCPGF]()
  var transacoesCOMPRAS: Array[TransacaoCPGF] = Array[TransacaoCPGF]()

  def receive = {
    case msg: ACLMessage =>
      if (msg.sender == system.deadLetters && msg.performative == Performative.REQUEST) {
        val vlCorte = 800.0
        val transacoes = startBehavior(msg.content.toString())
        println(transacoes.length)
        val estatisticasGlobais = gerarEstatisticasGlobais(transacoes)
        val estGlobaisSaques = estatisticasGlobais(0)
        val estGlobaisCompras = estatisticasGlobais(1)
        val perSaques = (100 * estGlobaisSaques.totalElements) / (100 * (estGlobaisSaques.totalElements + estGlobaisCompras.totalElements))
        val perCompras = (100 * estGlobaisCompras.totalElements) / (100 * (estGlobaisSaques.totalElements + estGlobaisCompras.totalElements))
        val perOutSaquesBaixo = (100 * transacoes.filter(_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao < estGlobaisSaques.minWhisker)) / (100 * estGlobaisSaques.totalElements)
        val perOutSaquesAlto = (100 * transacoes.filter(_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > estGlobaisSaques.maxWhisker)) / (100 * estGlobaisSaques.totalElements)
        println(transacoes.filter(_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > estGlobaisSaques.maxWhisker))
        val perOutComprasBaixo = (100 * transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao < estGlobaisCompras.minWhisker)) / (100 * estGlobaisCompras.totalElements)
        val perOutComprasAlto = (100 * transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > estGlobaisCompras.maxWhisker)) / (100 * estGlobaisCompras.totalElements)
        val perOpComprasCorte = (100 * transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > vlCorte)) / (100 * estGlobaisCompras.totalElements)
        println(transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > vlCorte))
        val estGlobaisFinal = EstatisticasTransacoes(perSaques,
          perCompras,
          perOutSaquesBaixo,
          perOutSaquesAlto,
          perOutComprasBaixo,
          perOutComprasAlto,
          perOpComprasCorte,
          estGlobaisSaques,
          estGlobaisCompras)
        println(estGlobaisFinal)
        val selecionado = Random.shuffle(transacoes.filter(e => !e.tipoTransacao.contains("SAQUE") && e.valorTransacao > estGlobaisCompras.maxWhisker).toList).take(1)(0)
        val transacoesSel = transacoes.filter(e => e.idPortador == selecionado.idPortador)
        println(selecionado)
        val portador = system.actorOf(Props(new Portador(estGlobaisFinal, transacoesSel)), name = "portador-" + selecionado.idPortador)
        portador ! ACLMessage(Map((ACLMessageParameter.PERFORMATIVE -> Performative.REQUEST),
          (ACLMessageParameter.SENDER -> this.self),
          (ACLMessageParameter.RECEIVER -> portador),
          (ACLMessageParameter.CONTENT -> "verifica-culpa")))
      }else if (msg.sender.toString().contains("portador") && msg.performative == Performative.INFORM){
        println(msg.sender + " declarou-se " + msg.content.toString)
        system.shutdown()
      }      
  }

  def gerarEstatisticasGlobais(transacoes: Array[TransacaoCPGF]): Array[BoxPlotMarks] = {
    GeraEstatisticas.gerarEstatisticasGlobais(transacoes)
  }

  def startBehavior(file: String): Array[TransacaoCPGF] = {
    println(file)
    val bufferedSource = io.Source.fromFile(file)
    val transacoes = bufferedSource.getLines.map(l => l.split(",")).map(
      e => TransacaoCPGF(
        e(0).trim,
        e(1).trim,
        e(2).trim.toInt,
        e(3).trim,
        e(4).trim.toInt,
        e(5).trim,
        e(6).trim.toInt,
        e(7).trim,
        e(8).trim.toInt,
        e(9).trim.toInt,
        e(10).trim,
        e(11).trim,
        e(12).trim,
        new GregorianCalendar(e(13).trim.split("-")(0).toInt, (e(13).trim.split("-")(1).toInt) - 1, e(13).trim.split("-")(2).toInt, 0, 0, 0),
        e(14).trim,
        e(15).trim,
        e(16).trim.toString.toDouble)).filter(_.valorTransacao > 0).toArray
    bufferedSource.close
    transacoes
  }

}