package model

case class EstatisticasTransacoes(
        perSaques: Double,
        perCompras: Double,
        perOutSaquesBaixo: Double,
        perOutSaquesAlto: Double,
        perOutComprasBaixo: Double,
        perOutComprasAlto: Double,
        perComprasCorte: Double,
        bloxPlotMarksSaques: BoxPlotMarks,
        bloxPlotMarksCompras: BoxPlotMarks
    )