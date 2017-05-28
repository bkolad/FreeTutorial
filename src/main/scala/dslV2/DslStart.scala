package dslV2

/**
  * Created by blaze on 23/02/17.
  */

import cats.free.Free.liftF
import cats.free.{Free, Inject}
import cats.{Id, ~>}
object DslStart {


  object LogOpt {

    sealed trait LogA[A]
    case class Log(l: String) extends LogA[Unit]


    type Logger[A] = Free[LogA, A]


    def log[F[_]](msg: String)(implicit I: Inject[LogA, F]): Free[F, Unit] =
      Free.inject[LogA, F](Log(msg))

    //implicit def interacts[F[_]](implicit I: Inject[LogA, F]): LogA[F] = new Interacts[F]

  }

  object DataOpt {

    sealed trait DataA[A]

    case class GetData(id: String) extends DataA[String]

    case class SaveData(d: String) extends DataA[Unit]

    type Data[A] = Free[DataA, A]


    def getData[F[_]](msg: String)(implicit I: Inject[DataA, F]): Free[F, String] =
      Free.inject[DataA, F](GetData(msg))(I)

    def saveData[F[_]](msg: String)(implicit I: Inject[DataA, F]): Free[F, Unit] =
      Free.inject[DataA, F](SaveData(msg))


  }

  object CalcOpt {

    sealed trait CalculationA[A]

    case class Calculate[T](a: T) extends CalculationA[Int]

    type Calculation[A] = Free[CalculationA, A]

    def calculate[T,F[_]](d: T)(implicit I: Inject[CalculationA, F]): Free[F, Int] =
      Free.inject[CalculationA, F](Calculate(d))

  }


  object Interpreters1{

    import LogOpt._

    def logITR : LogA ~> Id = new (LogA ~> Id) {
      def apply[A](fa: LogA[A]) : Id[A] = fa match {
        case Log(str) => println("Log "++ str)
      }
    }

    import DataOpt._

    def dataITR: DataA ~> Id = new (DataA ~> Id){
      def apply[A](fa: DataA[A]) : Id[A] = fa match {
        case GetData(str) => "Data 09"
        case SaveData(data) => println("Saved " ++ data)
      }
    }

    import CalcOpt._

    def calcITR: CalculationA ~> Id = new (CalculationA ~> Id){
      def apply[A](fa: CalculationA[A]): Id[A] = fa match{
        case Calculate(t) => 1098
      }
    }

    import cats.data.Coproduct

    type LogData[A] = Coproduct[LogA, DataA, A]

    val logDataInterpreter: LogData ~> Id = (logITR or dataITR)



    type App[A] = Coproduct[CalculationA, LogData, A]

    val interpreter: App ~> Id = calcITR or logDataInterpreter

    //implicit val inj2: Inject[DataA, App] = implicitly
   // implicit val inj3: Inject[CalculationA, App] = implicitly
  }

  def main(args: Array[String]): Unit = {

    import LogOpt._
    import DataOpt._
    import CalcOpt._


    println("Hello")


    import Interpreters1._



    def prog3(implicit L: Inject[LogA, App]
              , D: Inject[DataA, App]
              , C: Inject[CalculationA, App]
              ): Free[App, Unit]= for {
      _ <- log("lalal")
      _ <- getData("uuu")
      _ <- saveData("ii")
      c <- calculate(99)
      _<- saveData(c.toString)


    } yield ()

    //implicit def inj1: Inject[LogA, App] = implicitly

    prog3.foldMap(interpreter)

    //val evaled2 = prog3.foldMap(interpreter)


  }

}
