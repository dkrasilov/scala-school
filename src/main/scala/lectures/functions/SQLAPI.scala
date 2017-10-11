package lectures.functions

/**
  * Представим себе, как бы мог выглядеть API для работы, например, с БД
  * Строить методы этого API будем через композицию уже определенных методов.
  *
  * Ваша задача реализовать метод execute, композируя методы из объекта SQLAPI
  * Метод execute из объекта SQLAPI должен выполнить следующие действия
  * * * * * залогировать ресурс
  * * * * * получить соединение с помощью метода connection
  * * * * * залогировать запрос
  * * * * * открыть соединение
  * * * * * выполнить SQL
  * * * * * залогировать результат
  *
  * В результате в консоль должно быть выведено сообщение
  *    some DB
  *    some SQL
  *    SQL has been executed. Congrats!
  *
  *
  * Обратите внимание на то, что композиция функций учит писать код в декларативном виде
  * Благодаря этому мы можем отделить реализацию методов от их применения и, в конечном итоге, иметь переиспользуемую
  * декларацию, которую можно применить, например, для настоящей БД
  *
  *
  */
class SQLAPI(resource: String) {

  case class Connection(resource: String, opened: Boolean = false) {

    private val result = "SQL has been executed. Congrats!"

    def open(): Connection = this.copy(opened = true)

    def execute(sql: String): String = if (opened) result else throw new Exception("You have to open connection before execute")

  }

  private def logParameter[T](prm: T): T = {
    println(prm)
    prm
  }

  val connection: (String) => Connection = (resource: String) => Connection(resource)

  def execute(sql: String): String = logParameter(connection(logParameter(resource)).open() execute logParameter(sql))

  def execute2(sql: String): String = logParameter {
    openConnection(connection(logParameter(resource)))(logParameter(sql))
  }

  def execute3(sql: String): String =
    openConnection(
      (logParameter[String] _ andThen connection) (resource))
      .andThen(logParameter[String])
      .compose(logParameter[String])(sql)

  def openConnection(connection: Connection): (String) => String =
    (sql: String) => {
      connection.open execute sql
  }

}

object SQLCheck extends App {

  new SQLAPI("some DB").execute("some SQL")
  new SQLAPI("some DB").execute2("some SQL")
  new SQLAPI("some DB").execute3("some SQL")

}
