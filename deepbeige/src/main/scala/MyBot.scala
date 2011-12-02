
object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  def ordersFrom(game: Game): Set[Order] = {
    val ants = game.board.myAnts.values
    val food = game.board.food.values
    getFoodOrders(game,ants,food,List[Tile]())			   
  }

  private def getFoodOrders(game: Game, spare_ants: Iterable[MyAnt], remaining_food: Iterable[Food], new_locations: List[Tile] ) : Set[Order] = {
    if ((spare_ants.size == 0) || (remaining_food.size == 0)) {
      Set[Order]()
    } else { 
      val morsel = remaining_food.head
      val gatherer = spare_ants.reduceLeft[MyAnt]{ (ant1 , ant2) =>
	val ant1_distance = game.distanceFrom(ant1.tile).to(morsel.tile)
	val ant2_distance = game.distanceFrom(ant2.tile).to(morsel.tile)
        if (ant1_distance < ant2_distance) { ant1 } else { ant2 }						
      }
      val order = getOrder(game,gatherer,morsel,new_locations)
      System.err.println(order.toString)
      order._1.toSet ++ getFoodOrders(game,spare_ants.filterNot(x => x == gatherer),remaining_food.filterNot(x => x == morsel),order._2)
    }
  }

  private def getOrder(game: Game, ant: MyAnt, target: Positionable, locations: List[Tile]) = {
    val directions = List(North,East,South,West) 
    val direction: Option[CardinalPoint] = directions.filter( d => game.directionFrom(ant.tile).to(target.tile).contains(d)).find { aim =>
      val targetTile = game.tile(aim).of(target.tile)
      !game.board.water.contains(targetTile) && !locations.contains(targetTile) }
    val order = direction.map{ d => Order(ant.tile,d)}
    val new_locations = direction match {
      case Some(aim) => { locations :+ game.tile(aim).of(ant.tile) }
      case None => locations
    }
    (order,new_locations)
  }
}
