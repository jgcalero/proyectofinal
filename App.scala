package ec.utpl.edu.ec
import com.github.tototoshi.csv.*

import java.io.File
import org.nspl.*
import org.nspl.awtrenderer.*
import org.nspl.data.HistogramData
import java.util.regex.Pattern.matches
import scala.collection.immutable.ListMap
import java.time.LocalDate
import java.time._
import java.time.format.DateTimeFormatter

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
    println(s"Filas: ${contentFile.length} y Columnas: ${contentFile(1).keys.size}") //Imprime # de filas y columnas
    //println(contentFile2.take(1))
    println(s"Filas: ${contentFile2.length} y Columnas: ${contentFile2(1).keys.size}")

    println("PREGUNTAS DE DATOS ESTADISTICOS DESCRIPTIVOS")
    println(s"Total de partidos jugados: ${TotalPartidos(contentFile)} donde el resultado fue diferente de empate")
    println(s"Media de goles por partido: ${mediaGolesporPartido(contentFile)}")
    println(s"Estadio capacidad maxima: ${estadioMaxCapacidad(contentFile)}")
    println(s"Estadio capacidad minima: ${estadioMinCapacidad(contentFile)}")
    println(s"Pais con más mundiales: ${maxCampeon(contentFile)}")
    println(s"desviación estandar: ${stdDevTeams(contentFile2)}")
    println(s"Total de jugadores en los mundiales: ${totalJugadores(contentFile2)}")
    println(s"Media del número de la camiseta de los jugadores: ${mediaNumeroCamiseta(contentFile2)}")

    def stdDevTeams(torneos: List[Map[String, String]]): Double = {
      // Convierte la lista de mapas en una lista de tuplas
      val equipos = torneos.flatMap { row =>
        row.get("tournaments_teams").map(_.toInt).map((row("tournaments_name"), _))
      }
      val equipoValido = equipos.filter(_._2 > 0)       // Filtra torneos equipos válidos
      val equipoMedio = equipoValido.map(_._2).sum.toDouble / equipoValido.size
      // Calcula las desviaciones al cuadrado de la media.
      val desviacionStandar = equipoValido.map { case (_, teamCount) =>
        Math.pow(teamCount - equipoMedio, 2)
      }
      val varianza = desviacionStandar.sum / equipoValido.size //Calcula la varianza
      Math.sqrt(varianza)     //Calcula y retornala desviación estandar
    }

    //Función para obtener el total de partidos que tuvieron un resultado diferente de empate
    def TotalPartidos(data: List[Map[String, String]]): Int = {
      // Filtra las filas con datos faltantes o en blanco
      val filtraData = data.filter(row =>
        row.contains("matches_match_id") &&
          row.contains("matches_result")
      )
      filtraData.count(row => row("matches_result") != "draw")
      //Cuenta el número de filas que el resultado es diferente de empate
    }

    //Promedio de goles por partido
    def mediaGolesporPartido(data: Seq[Map[String, String]]): Double = {
      // Filtra los datos que no tienen goles en información
      val filtraData = data.filter(row => row.get("matches_home_team_score").isDefined && row.get("matches_away_team_score").isDefined)
      //Convierte la data a un tipo de dato double
      val golesData = filtraData.map(row => {
        val golLocales = row("matches_home_team_score").toDouble
        val golVisitante = row("matches_away_team_score").toDouble
        (golLocales, golVisitante)
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

    //Función para encontrar el maximo campeón de los mundiales
    def maxCampeon(campeonMundial: List[Map[String, Any]]): (String, Int) = {
      val campeonesmundiales = campeonMundial.groupBy(campeon => campeon("tournaments_winner").toString)
      val campeonPais = campeonesmundiales.mapValues(_.size)
      val campeon = campeonPais.maxBy(_._2)
      (campeon._1, campeon._2)
    }

    //función para obtener el número de jugadores en los mundiales
    def totalJugadores(data: List[Map[String, String]]): Int = {
      //Cuenta el número distinto de jugadores
      data.map(_.get("squads_player_id").get).distinct.size
    }

    //función para calcular el premdio del número de la camiseta de los jugadores
    def mediaNumeroCamiseta(players: List[Map[String, String]]): Double = {
      val jugadorSinNumero = players.filter(_.get("squads_shirt_number").isDefined)  //Filtra jugadores sin # en la camiseta
      val numEnCamisera = jugadorSinNumero.map(_.get("squads_shirt_number").get.toInt)   //Convierte el # de camiseta a entero
      numEnCamisera.sum.toDouble / numEnCamisera.size     //Calcula y retorna la media
    }


  }
}
