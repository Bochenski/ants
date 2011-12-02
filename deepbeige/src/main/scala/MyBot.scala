
object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  def ordersFrom(game: Game): Set[Order] = {
    val ants = game.board.myAnts.values

    System.err.println("Deep Beige is Hungry")
    val my_hill_tiles = game.board.myHills.values.map { hill => hill.tile }.toList

    val food_orders = getFoodOrders(game,ants,game.board.food.values,my_hill_tiles)
    val left_overs = getLeftOvers(game,food_orders._2,food_orders._3)
    food_orders._1 ++ left_overs
  }

  private def getLeftOvers(game: Game, available_ants: Iterable[MyAnt], locations: List[Tile] ) : Set[Order] = {
    if (available_ants.size == 0) { Set[Order]() 
    } else {
      val directions = List(North,East,South,West)
      val ant = available_ants.head
      
      val order = getOrder(game,ant,directions,locations)
      order._1.toSet ++ getLeftOvers(game,available_ants.filterNot(x => x == ant),order._2)
    }
  }

  private def getFoodOrders(game: Game, available_ants: Iterable[MyAnt], remaining_food: Iterable[Food], new_locations: List[Tile] ) : (Set[Order],Iterable[MyAnt],List[Tile]) = {
    if ((available_ants.size == 0) || (remaining_food.size == 0)) {
      (Set[Order](),available_ants,new_locations)
    } else { 
      val morsel = remaining_food.head
      val gatherer = available_ants.reduceLeft[MyAnt]{ (ant1 , ant2) =>
	val ant1_distance = game.distanceFrom(ant1.tile).to(morsel.tile)
	val ant2_distance = game.distanceFrom(ant2.tile).to(morsel.tile)
        if (ant1_distance < ant2_distance) { ant1 } else { ant2 }						
      }
      val directions = List(North,East,South,West)
      val possible_locations = directions.filter( d => game.directionFrom(gatherer.tile).to(morsel.tile).contains(d))
      val order = getOrder(game,gatherer,possible_locations,new_locations)
      
      val remaining_ants = available_ants.filterNot(x => x == gatherer)
      (order._1.toSet ++ getFoodOrders(game,remaining_ants,remaining_food.filterNot(x => x == morsel),order._2)._1,remaining_ants,order._2)
    }
  }

  private def getOrder(game: Game, ant: MyAnt, possible_directions: List[CardinalPoint], used_locations: List[Tile]) = {

    val direction: Option[CardinalPoint] = possible_directions.find { aim =>
      val targetTile = game.tile(aim).of(ant.tile)
      !game.board.water.contains(targetTile) && !used_locations.contains(targetTile) }
    val order = direction.map{ d => Order(ant.tile,d)}
    val new_used_locations = direction match {
      case Some(aim) => { used_locations :+ game.tile(aim).of(ant.tile) }
      case None => used_locations
    }
    (order,new_used_locations)
  }
}
