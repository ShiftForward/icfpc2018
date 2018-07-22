package icfpc2018

import java.io.{ BufferedOutputStream, FileOutputStream }
import java.nio.file.{ Files, Paths }

import scala.collection.JavaConverters._

import icfpc2018.solver._
import scala.util.{ Failure, Success, Try }

object Main extends App {

  val models = Files.list(Paths.get("models", "lightning")).iterator().asScala.toList
    .filter(_.toString.endsWith(".mdl"))
    .sortBy(_.toString)
  val solver: Solver = GreedySolver
  def validate(model: Matrix, solution: List[Command]) = Try(Simulator.runAndValidate(model, solution))
  //def validate(model: Matrix, solution: List[Command]) = true

  def time[T](f: => T): (T, Long) = {
    val startTime = System.currentTimeMillis()
    val res = f
    val endTime = System.currentTimeMillis()
    (res, endTime - startTime)
  }

  def export(commands: List[Command], filename: String): String = {
    val output = commands.flatMap(_.encoded).toArray
    val outputFilename = "output/" + filename.take(5) + ".nbt"
    val bos = new BufferedOutputStream(new FileOutputStream(outputFilename))
    bos.write(output)
    bos.close()
    outputFilename
  }

  models.foreach { modelPath =>
    println("Parsing model " + modelPath.toString)
    val (model, parseTime) = time(Matrix.fromMdl(modelPath.toFile))
    println(s"Parsed ${model.voxels.size} in ${parseTime}ms")
    val (solution, solvedTime) = time(solver.solve(model))
    println(s"Solved with ${solution.size} commands in ${solvedTime}ms")
    val (validModel, validationTime) = time(validate(model, solution))
    validModel match {
      case Success(st) =>
        println(s"Validated solution in ${validationTime}ms. Energy was ${st.energy}")
      case Failure(ex) =>
        println(s"Failed validation in ${validationTime}ms with ${ex.getClass.getName}")
    }
    val (outputFilename, exportedTime) = time(export(solution, modelPath.getFileName.toString))
    println(s"Exported to $outputFilename in ${exportedTime}ms")
    println("-----------------------------------------------------------")
  }
}
