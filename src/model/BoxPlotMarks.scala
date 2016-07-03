package model

case class BoxPlotMarks(
    minWhisker: Double, 
    min: Double, 
    fQuart: Double, 
    median: Double, 
    tQuart: Double,  
    maxWhisker: Double, 
    max: Double, 
    mean: Double, 
    variance: Double, 
    stdDeviation: Double, 
    totalElements: Long)