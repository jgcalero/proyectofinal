package Grapichs

import com.github.tototoshi.csv.*
import org.nspl.*
import org.nspl.awtrenderer.*
import org.nspl.data.HistogramData
//import org.xchart.QuickChart
//import org.xchart.Series
//import org.xchart.SwingWrapper

import java.io.File



implicit object MyFormat extends DefaultCSVFormat {
  override val delimiter = ';'
}
object App {
  @main
  def ProyectoFinal(): Unit = {
    val pathDataFile = "C://Personal//ArchivoPIntegrador//dsPartidosYGoles.csv" //Directorio del archivo
    val pathDataFile2 = "C://Personal//ArchivoPIntegrador//dsAlineacionesXTorneo.csv"
    val reader = CSVReader.open(new File(pathDataFile)) //lectura del archivo
    val reader2 = CSVReader.open(new File(pathDataFile2))
    val contentFile: List[Map[String, String]] = reader.allWithHeaders() //lista de mapeo de las columnas
    val contentFile2: List[Map[String, String]] = reader2.allWithHeaders()
    reader.close() //cierre de lectura de archivo
    reader2.close()

    println("_- PREGUNTAS DE GRAFICOS DESCRIPTIVOS")
    println(frecuenciaGolMinuto(contentFile))
    println(equiposxTorneo(contentFile))
    println(golesMundial(contentFile))

    //Función que gráfica los minutos en los que se marcaron más goles mundial 2022
    def frecuenciaGolMinuto(data: List[Map[String, String]]): Unit = {
      val contarGolMinuto: List[(Double, Double)] =  data
        .filter(_("goals_minute_regulation") != "NA")
        .filter(_("matches_tournament_id") == "WC-2022")
        .map(_("goals_minute_regulation").toInt)
        .groupBy(identity)
        .view //Mejora la eficiencia en el cálculo
        .mapValues(_.size.toInt)
        .toList
        .sortBy(-_._2) // Ordenar descendente
        .take(10)
        .map { case (minuto, frecuencia) => (minuto, frecuencia) }
      println(contarGolMinuto)
      //grafica los datos obtenidos
      val lineal = xyplot(
        contarGolMinuto -> line())(
        par
          .xlab("Minutos")
          .ylab("Goles")
      )
      pngToFile(new File("C://Personal//ArchivoPIntegrador//GolesXminuto1.png"), lineal.build, 5000)
    }

    //Función que grafica la frecuencua de participaciones por año de torneo
    def equiposxTorneo(data: List[Map[String, String]]): Unit = {
      val conteoEquipos = data
        .map(row => (row("tournaments_year").toDouble, row("tournaments_count_teams").toDouble))
        .distinct
        .groupBy(_._2)
        .view.mapValues(_.head)
        .values
        .toList
        .sortBy(_._2)
      println(conteoEquipos)
      val barras = xyplot(
        conteoEquipos -> bar(horizontal = false,
          width = 2.5,
          fill = Color.BLUE)
      )(
        par
          .xlab("Año")
          .ylab("Participaciones Equipo")
      )
      pngToFile(new File("C://Personal//ArchivoPIntegrador//EquiposxTorneo2.png"), barras.build, 5000)
    }

    //función de grafica goles por mundial
    def golesMundial(data: List[Map[String, String]]): Unit = {
      val golexMundial = data
        .map(row => (
          row("matches_tournament_id").stripPrefix("WC-").toDouble,
          row("matches_match_id"),
          row("matches_home_team_score"),
          row("matches_away_team_score")
        ))
        .distinct
        .map(t4 => (t4._1, t4._3.toDouble + t4._4.toDouble))
        .groupBy(_._1)
        .map(t2 => (t2._1, t2._2.map(_._2).sum))
        .toList
        .sortBy(-_._2)
      println(golexMundial)
      val puntos = xyplot(golexMundial -> point())(
        par
          .xlab("Años de los Mundiales")
          .ylab("Goles")
      )
      pngToFile(new File("C://Personal//ArchivoPIntegrador//GolescMundial3.png"), puntos.build, 2000)

    }

  }
}
