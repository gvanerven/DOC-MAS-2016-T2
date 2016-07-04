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
  var index = 0
  var numAtivos = 0
  //change to use Options
  var transacoes: Array[TransacaoCPGF] = null
  var estGlobaisFinal: EstatisticasTransacoes = null
  var selecionados: Array[String] = null
  val vlCorte = 800.0

  def max(a: Double, b: Double): Double = {
    if(a > b) a else b
  }
  
  def receive = {
    case msg: ACLMessage =>
      if (msg.sender == system.deadLetters && msg.performative == Performative.REQUEST) {
        transacoes = startBehavior(msg.content.toString())
        val saques = transacoes.filter(e => e.tipoTransacao.contains("SAQUE"))
        val compras = transacoes.filter(e => !e.tipoTransacao.contains("SAQUE"))
        //println(transacoes.length)
        val estatisticasGlobais = gerarEstatisticasGlobais(transacoes)
        val estGlobaisSaques = estatisticasGlobais(0)
        val estGlobaisCompras = estatisticasGlobais(1)
        val perSaques = (100 * estGlobaisSaques.totalElements) / (100 * (estGlobaisSaques.totalElements + estGlobaisCompras.totalElements))
        val perCompras = (100 * estGlobaisCompras.totalElements) / (100 * (estGlobaisSaques.totalElements + estGlobaisCompras.totalElements))
        val perOutSaquesBaixo = (100 * saques.count(e => e.valorTransacao < estGlobaisSaques.minWhisker)) / (100 * max(estGlobaisSaques.totalElements,1))
        val perOutSaquesAlto = (100 * saques.count(e => e.valorTransacao > estGlobaisSaques.maxWhisker)) / (100 * max(estGlobaisSaques.totalElements,1))
        //println(transacoes.filter(_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > estGlobaisSaques.maxWhisker))
        val perOutComprasBaixo = (100 * compras.count(e => e.valorTransacao < estGlobaisCompras.minWhisker)) / (100 * max(estGlobaisCompras.totalElements,1))
        val perOutComprasAlto = (100 * compras.count(e => e.valorTransacao > estGlobaisCompras.maxWhisker)) / (100 * max(estGlobaisCompras.totalElements,1))
        val perOpComprasCorte = (100 * compras.count(e => e.valorTransacao > vlCorte)) / (100 * max(estGlobaisCompras.totalElements,1))
        //println(transacoes.filter(!_.tipoTransacao.contains("SAQUE")).count(e => e.valorTransacao > vlCorte))
        estGlobaisFinal = EstatisticasTransacoes(perSaques,
          perCompras,
          perOutSaquesBaixo,
          perOutSaquesAlto,
          perOutComprasBaixo,
          perOutComprasAlto,
          perOpComprasCorte,
          estGlobaisSaques,
          estGlobaisCompras)
        println("Estatisticas finais: " + estGlobaisFinal)
        //val selecionados = Random.shuffle(compras.filter(e => e.valorTransacao > estGlobaisCompras.maxWhisker).toList).take(5)
        selecionados = compras.filter(e => e.valorTransacao > estGlobaisCompras.maxWhisker).map(e => e.idPortador).distinct
        portadores()

      } else if (msg.sender.toString().contains("portador") && msg.performative == Performative.INFORM) {
        println(msg.sender.toString.split("/")(4)  + " declarou-se " + msg.content.toString)
        numAtivos -= 1
        if (numAtivos == 0) {
          if (index < (selecionados.length - 1)) {
            portadores()
          } else {
            system.shutdown()
          }
        }
      }
  }

  def portadores(acc: Int = 0) { //refact to make functional
    if(acc < 2 && index < (selecionados.length - 1)){
      iniciaPortadores(selecionados(index))
      index += 1
      numAtivos += 1
      portadores(acc + 1)
    }    
  }

  def iniciaPortadores(selecionado: String) {
    val transacoesSel = transacoes.filter(e => e.idPortador == selecionado)
    val portador = system.actorOf(Props(new Portador(estGlobaisFinal, transacoesSel, vlCorte)), name = "portador-" + selecionado)
    portador ! ACLMessage(Map((ACLMessageParameter.PERFORMATIVE -> Performative.REQUEST),
      (ACLMessageParameter.SENDER -> this.self),
      (ACLMessageParameter.RECEIVER -> portador),
      (ACLMessageParameter.CONTENT -> "verifica-culpa")))
  }

  def gerarEstatisticasGlobais(transacoes: Array[TransacaoCPGF]): Array[BoxPlotMarks] = {
    GeraEstatisticas.gerarEstatisticasTransacoes(transacoes)
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