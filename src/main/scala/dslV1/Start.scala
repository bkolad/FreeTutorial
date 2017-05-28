package dslV1

/**
  * Created by blaze on 28/05/17.
  *
  */


import cats.free.{Free, Inject}
import cats.{Id, ~>}

object  DSLOpt {

  sealed trait DSL[A]
  case class Print(l: String) extends DSL[Unit]
  case class GetData(id: String) extends DSL[String]
  case class SaveData(d: String) extends DSL[Unit]
  case class PriceOption(data: String) extends DSL[Int]


  type MyProgrammingLanguage[A] = Free[DSL, A]


  def log[F[_]](msg: String)(implicit I: Inject[DSL, F]): Free[F, Unit] =
    Free.inject[DSL, F](Print(msg))


  def getData[F[_]](dataId: String)(implicit I: Inject[DSL, F]): Free[F, String] =
    Free.inject[DSL, F](GetData(dataId))(I)

  def saveData[F[_]](msg: String)(implicit I: Inject[DSL, F]): Free[F, Unit] =
    Free.inject[DSL, F](SaveData(msg))


  def calculate[F[_]](d: String)(implicit I: Inject[DSL, F]): Free[F, Int] =
    Free.inject[DSL, F](PriceOption(d))


  type IO[A] = Id[A]

  def interpreter : DSL ~> IO = new (DSL ~> IO) {
    def apply[A](fa: DSL[A]) : IO[A] = fa match {

      case Print(str) => println("Log "++ str)



    }
  }

}



object Start {

  import DSLOpt._


  def main(args: Array[String]): Unit = {

    print("hello")

  }

}
