
object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  def ordersFrom(game: Game): Set[Order] = {
    val ants = game.board.myAnts.values
   
    val unexplored = {
      (0 until game.parameters.rows).foldLeft(Set[Tile]()) {(set, row) =>
	set ++ { 
	  (0 until game.parameters.columns).foldLeft(Set[Tile]()) { (subset, column) => 
	    subset + Tile(column,row)
	  }
	}
      }
    }.&~(game.board.exploredTiles.keys.toSet)

    System.err.println("unexplored: " + unexplored.size.toString)

    System.err.println("Deep Beige is Hungry")
    val my_hill_tiles = game.board.myHills.values.map { hill => hill.tile }.toList

    val food_orders = getTargetsForAnts(game,ants,game.board.food.keys,my_hill_tiles)
    System.err.println("food orders " + food_orders._1.toString)
    System.err.println("remainingAnts after food orders: " + food_orders._2.toString)
    val explore_orders = getTargetsForAnts(game, food_orders._2,unexplored,food_orders._3)
    System.err.println("explore orders " + explore_orders._1.toString)
    System.err.println("remainingAnts after explore orders " + explore_orders._2.toString)
    val left_overs = getLeftOvers(game,explore_orders._2,explore_orders._3)
    food_orders._1 ++ explore_orders._1 ++ left_overs
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

  private def getTargetsForAnts(game: Game, available_ants: Iterable[MyAnt], targets: Iterable[Tile], 
				blocked_locations: List[Tile]) : (Set[Order], Iterable[MyAnt], List[Tile]) = {

    val result = available_ants.foldLeft((Set[Order](),targets,blocked_locations,List[MyAnt]())) { (state,ant) => {
      if (state._2.size == 0) { state } else {
      val target = targets.reduceLeft[Tile]{(target1, target2) => 
	if(game.distanceFrom(ant.tile).to(target1) < game.distanceFrom(ant.tile).to(target2)) { target1 } else { target2 }
      }
      val directions = List(North,East,South,West)
      val possible_directions = directions.filter(d => game.directionFrom(ant.tile).to(target).contains(d))
      val order = getOrder(game,ant,possible_directions,blocked_locations)
      (state._1 ++ order._1,state._2.filterNot(x => x == target),order._2,ant +: state._4)
      }
    }}
    (result._1,available_ants.filterNot(x => result._4.contains(x)),result._3)
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
