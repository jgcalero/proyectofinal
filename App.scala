package ec.utpl.edu.ec
import com.github.tototoshi.csv.*

import java.io.File
import org.nspl.*
import org.nspl.awtrenderer.*
import org.nspl.data.HistogramData

import java.util.regex.Pattern.matches
import scala.collection.immutable.ListMap


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

    //println(contentFile.take(1))      //imprime las cabeceras del archivo
    //println(s"Filas: ${contentFile.length} y Columnas: ${contentFile(1).keys.size}") //Imprime # de filas y columnas
    //println(s"Filas: ${contentFile2.length} y Columnas: ${contentFile2(1).keys.size}")

    println("PREGUNTAS DE DATOS ESTADISTICOS DESCRIPTIVOS")
    println(s"Total de partidos jugados: ${getTotalPartidos(contentFile)} donde el resultado fue diferente de empate")
    println(s"Media de goles por partido: ${mediaGolesporPartido(contentFile)}")
    println(s"Estadio capacidad maxima: ${estadioMaxCapacidad(contentFile)}")
    println(s"Estadio capacidad minima: ${estadioMinCapacidad(contentFile)}")
    println(s"Pais con más mundiales: ${maxCampeon(contentFile)}")


    //Función para obtener el total de partidos que tuvieron un resultado diferente de empate
    def getTotalPartidos(data: List[Map[String, String]]): Int = {
      // Filtra las filas con datos faltantes o en blanco
      val filteredData = data.filter(row =>
        row.contains("matches_match_id") &&
          row.contains("matches_result")
      )
      filteredData.count(row => row("matches_result") != "draw")
      //Cuenta el número de filas que el resultado es diferente de empate
    }
    getTotalPartidos(contentFile)

    //Promedio de goles por partido
    def mediaGolesporPartido(data: Seq[Map[String, String]]): Double = {
      // Filtra los datos que no tienen goles en información
      val filteredData = data.filter(row => row.get("matches_home_team_score").isDefined && row.get("matches_away_team_score").isDefined)
      //Convierte la data a un tipo de dato double
      val golesData = filteredData.map(row => {
        val homeTeamGoals = row("matches_home_team_score").toDouble
        val awayTeamGoals = row("matches_away_team_score").toDouble
        (homeTeamGoals, awayTeamGoals)
      })
      //println(goalsData)
      //Calcula el número total de goles y partidos
      val totalGoles = golesData.map(_.toDouble + _.toDouble).sum
      val totalPartidos = golesData.size

      // Calcula y retorna la media de goles por partido
      totalGoles.toDouble / totalPartidos
    }

    //Función para obtener el estadio con mayor capacidad//
    def estadioMaxCapacidad(data: List[Map[String, String]]): (Int, String) =
      data.map(tupla2 => (tupla2("stadiums_stadium_capacity").toInt, tupla2("stadiums_stadium_name"))).max

    //Función para obtener el estadio con minima capacidad
    def estadioMinCapacidad(data: List[Map[String, String]]): (Int, String) =
      data.map(tuple3 => (tuple3("stadiums_stadium_capacity").toInt, tuple3("stadiums_stadium_name"))).min

    //Función para encontrar el maximo campeón de los mundiiles
    def maxCampeon(campeonMundial: List[Map[String, Any]]): (String, Int) = {
      val campeonesmundiales = campeonMundial.groupBy(campeon => campeon("tournaments_winner").toString)
      val campeonPais = campeonesmundiales.mapValues(_.size)
      val campeon = campeonPais.maxBy(_._2)
      (campeon._1, campeon._2)
    }


  }
}
