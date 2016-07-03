package stats

import model._
import org.apache.commons.math3.ml.clustering._
import org.apache.commons.math3.stat.descriptive.{ DescriptiveStatistics, _ }

object GeraEstatisticas {
  def gerarEstatisticasGlobais(transacoes: Array[TransacaoCPGF]): Array[BoxPlotMarks] = {
    val transacoesSAQUES = transacoes.filter(_.tipoTransacao.contains("SAQUE"))
    val transacoesCOMPRAS = transacoes.filter(!_.tipoTransacao.contains("SAQUE"))
    val stSAQUE = calcBoxPlotVals(new DescriptiveStatistics(transacoesSAQUES.map(_.valorTransacao)))
    val stCOMPRA = calcBoxPlotVals(new DescriptiveStatistics(transacoesCOMPRAS.map(_.valorTransacao)))
    Array[BoxPlotMarks](stSAQUE, stCOMPRA)
  }

  def calcBoxPlotVals(sts: DescriptiveStatistics): BoxPlotMarks = {
    if (sts.getN > 0) {
      val sVal = sts.getSortedValues
      val median = sVal(Math.floor(sVal.length / 2).toInt)
      val fQuart = sVal(Math.floor(sVal.length / 4).toInt)
      val tQuart = sVal(3 * Math.floor(sVal.length / 4).toInt)
      val factor = 1.5 * (tQuart - fQuart)
      BoxPlotMarks(fQuart - factor, sts.getMin, fQuart, median, tQuart, tQuart + factor, sts.getMax, sts.getMean, sts.getVariance, sts.getStandardDeviation, sts.getN)
    } else {
      BoxPlotMarks(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1)
    }

  }

}