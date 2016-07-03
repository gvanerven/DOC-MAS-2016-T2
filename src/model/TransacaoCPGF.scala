package model

import java.util.GregorianCalendar
import java.util.Calendar

case class TransacaoCPGF(
                          idPortador: String,
                          idFavorecido: String,
                          codigoOrgaoSuperior: Int,
                          nomeOrgaoSuperior: String,
                          codigoOrgaoSubordinado: Int,
                          nomeOrgaoSubordinado: String,
                          codigoUnidadeGestora: Int,
                          nomeUnidadeGestora: String,
                          anoExtrato: Int,
                          mesExtrato: Int,
                          CPFPortador: String,
                          nomePortador: String,
                          tipoTransacao: String,
                          dataTransacao: GregorianCalendar,
                          CNPJCPFFavorecido: String,
                          nomeFavorecido: String,
                          valorTransacao: Double){
  def dateToString(cal: Calendar) = cal.get(Calendar.YEAR) + "-" + (cal.get(Calendar.MONTH) + 1) + "-" + cal.get(Calendar.DAY_OF_MONTH)
  def getDateFormated = dateToString(dataTransacao)
}