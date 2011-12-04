import annotation.tailrec
import io.Source
import java.io._

class AntsGame(in: InputStream = System.in, out: OutputStream = System.out) {

  val source = new BufferedSource(in, Source.DefaultBufSize)
  val writer = new BufferedWriter(new OutputStreamWriter(out))

  def run(bot: Bot) = {
    try {

      def playNextTurn(game: Game, lastOrders: Set[Order]): Unit = {
        val newGameState = Parser.parse(source, game.parameters, game.board.water,game.board.exploredTiles, lastOrders)
        if (newGameState.gameOver) Unit
        else {
          val orders = bot.ordersFrom(newGameState)
          orders.map(_.inServerSpeak).foreach(writer.write)
          writer.write("go\n")
          writer.flush
          playNextTurn(newGameState,orders)
        }
      }
      playNextTurn(GameInProgress(),Set[Order]())

    } catch {
      case t => t.printStackTrace
    }
  }

}
