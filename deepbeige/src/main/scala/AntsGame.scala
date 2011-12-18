import annotation.tailrec
import io.Source
import java.io._

class AntsGame(in: InputStream = System.in, out: OutputStream = System.out) {

  val source = new BufferedSource(in, Source.DefaultBufSize)
  val writer = new BufferedWriter(new OutputStreamWriter(out))

  def run(bot: Bot) = {
    try {

      def playNextTurn(game: Game, lastOrders: Set[Order], targets: Map[Tile,Option[MyAnt]]): Unit = {
        val newGameState = Parser.parse(source, game.parameters, game.board.water,game.board.exploredTiles, lastOrders, targets)
        if (newGameState.gameOver) Unit
        else {
	  val result = bot.ordersFrom(newGameState)
          val orders = result._1
	  val targets = result._2
          orders.map(_.inServerSpeak).foreach(writer.write)
          writer.write("go\n")
          writer.flush
          playNextTurn(newGameState,orders,targets)
        }
      }
      playNextTurn(GameInProgress(),Set[Order](),Map[Tile,Option[MyAnt]]())

    } catch {
      case t => t.printStackTrace
    }
  }
}
