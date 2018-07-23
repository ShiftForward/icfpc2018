package icfpc2018

import java.io.{ BufferedOutputStream, FileOutputStream }
import java.nio.file.{ Files, Paths }

import scala.collection.JavaConverters._

import icfpc2018.solver._
import scala.util.{ Failure, Success, Try }

trait Main extends App {
  def validate(model: Matrix, solution: List[Command]) = Try(Simulator.runAndValidate(model, solution))

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
}

object LightningMain extends Main {

  val models = Files.list(Paths.get("models", "lightning")).iterator().asScala.toList
    .filter(_.toString.endsWith(".mdl"))
    .sortBy(_.toString)
  val solver: Solver = ASolver

  models.foreach { modelPath =>
    println("Parsing model " + modelPath.toString)
    val (model, parseTime) = time(Matrix.fromMdl(modelPath.toFile))
    println(s"Parsed ${model.voxels.size} voxels in ${parseTime}ms")
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

object FullMain extends Main {

  val models = Files.list(Paths.get("models", "full")).iterator().asScala.toList
    .filter(_.toString.endsWith(".mdl"))
    .sortBy(_.toString)
  val assemblyModels = models.filter(_.getFileName.toString.startsWith("FA"))
  val disassemblyModels = models.filter(_.getFileName.toString.startsWith("FD"))
  val reassemblyModels =
    models.filter(_.getFileName.toString.startsWith("FR"))
      .groupBy(_.getFileName.toString.take(5)).values.collect {
        case f1 :: f2 :: Nil => if (f1.getFileName.toString.contains("src")) (f1, f2) else (f2, f1)
      }.toList.sortBy(_.toString)

  val assemblySolver: Solver = ASolver
  val disassemblySolver: RebuilderSolver = TracerSolver(true)
  val reassemblySolver: RebuilderSolver = TracerSolver(false)

  println("Running Assembly Problems...")
  assemblyModels.foreach { modelPath =>
    println("Parsing model " + modelPath.toString)
    val (model, parseTime) = time(Matrix.fromMdl(modelPath.toFile))
    println(s"Parsed ${model.voxels.size} voxels in ${parseTime}ms")
    val (solution, solvedTime) = time(assemblySolver.solve(model))
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

  println("Running Disassembly Problems...")
  disassemblyModels.foreach { modelPath =>
    println("Parsing model " + modelPath.toString)
    val (model, parseTime) = time(Matrix.fromMdl(modelPath.toFile))
    println(s"Parsed ${model.voxels.size} voxels in ${parseTime}ms")
    val (solution, solvedTime) = time(disassemblySolver.solve(model, Matrix(model.dimension)))
    println(s"Solved with ${solution.size} commands in ${solvedTime}ms")
    // TODO validate this
    val (outputFilename, exportedTime) = time(export(solution, modelPath.getFileName.toString))
    println(s"Exported to $outputFilename in ${exportedTime}ms")
    println("-----------------------------------------------------------")
  }

  println("Running Reassembly Problems...")
  reassemblyModels.foreach {
    case (srcModelPath, dstModelPath) =>
      println("Parsing model " + srcModelPath.toString)
      val (srcModel, srcParseTime) = time(Matrix.fromMdl(srcModelPath.toFile))
      println(s"Parsed ${srcModel.voxels.size} voxels in ${srcParseTime}ms")
      println("Parsing model " + dstModelPath.toString)
      val (dstModel, dstParseTime) = time(Matrix.fromMdl(dstModelPath.toFile))
      println(s"Parsed ${dstModel.voxels.size} voxels in ${dstParseTime}ms")
      val (solution, solvedTime) = time(reassemblySolver.solve(srcModel, dstModel))
      // TODO validate this
      println(s"Solved with ${solution.size} commands in ${solvedTime}ms")
      val (outputFilename, exportedTime) = time(export(solution, srcModelPath.getFileName.toString))
      println(s"Exported to $outputFilename in ${exportedTime}ms")
  }
}
