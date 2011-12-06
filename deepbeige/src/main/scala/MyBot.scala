
object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  def ordersFrom(game: Game): Set[Order] = {
    val ants = game.board.myAnts.values
 
    System.err.println("Deep Beige is Hungry")
    val my_hill_tiles = game.board.myHills.values.map { hill => hill.tile }.toList
    val food_orders = getAntsForTargets(game,ants,game.board.food.keys,my_hill_tiles)
    val explore_orders = getTargetsForAnts(game, food_orders._2,unexplored(game),food_orders._3)
    val left_overs = getLeftOvers(game,explore_orders._2,explore_orders._3)
 
    food_orders._1 ++ explore_orders._1 ++ left_overs
  }

  private def unexplored(game:Game) = {
     {
      (0 until game.parameters.rows).foldLeft(Set[Tile]()) {(set, row) =>
	set ++ { 
	  (0 until game.parameters.columns).foldLeft(Set[Tile]()) { (subset, column) => 
	    subset + Tile(column,row)
	  }
	}
      }
    }.&~(game.board.exploredTiles.keys.toSet)
  }

  private def getLeftOvers(game: Game, available_ants: Iterable[MyAnt], blocked_locations: List[Tile] ) : Set[Order] = {
     available_ants.flatMap(  ant => { 
      getOrder(game,ant,List(North,East,South,West),blocked_locations)     }).toSet 
  }			 

  private def getAntsForTargets(game: Game, available_ants: Iterable[MyAnt], targets: Iterable[Tile],
				blocked_locations: List[Tile]) : (Set[Order], Iterable[MyAnt], List[Tile]) = {
    val result = targets.foldLeft((Set[Order](),available_ants,blocked_locations,List[MyAnt]())) { (state,target) => {
      if (state._2.size == 0) { state } 
      else {
	val ant = state._2.reduceLeft[MyAnt]{(ant1, ant2) => 
	  if(game.distanceFrom(ant1.tile).to(target) < game.distanceFrom(ant2.tile).to(target)) { ant1 } else { ant2 }
	}
        val possible_directions = List(North,East,South,West).filter(d => game.directionFrom(ant.tile).to(target).contains(d))
        getOrder(game,ant,possible_directions,state._3) match { 
	  case Some(order) => { 
	    val new_blocked_locations = game.tile(order.point).of(ant.tile) +: state._3
	    (state._1 + order, state._2.filterNot(x => x == ant),new_blocked_locations,ant +: state._4)}
	  case None => state
        }
      }
    }}
    (result._1,available_ants.filterNot(x => result._4.contains(x)),result._3)											       
  }			       
			 
  private def getTargetsForAnts(game: Game, available_ants: Iterable[MyAnt], targets: Iterable[Tile], 
				blocked_locations: List[Tile]) : (Set[Order], Iterable[MyAnt], List[Tile]) = {
    val result = available_ants.foldLeft((Set[Order](),targets,blocked_locations,List[MyAnt]())) { (state,ant) => {
      if (state._2.size == 0) { state } 
      else {
        val target = state._2.reduceLeft[Tile]{(target1, target2) => 
  	  if(game.distanceFrom(ant.tile).to(target1) < game.distanceFrom(ant.tile).to(target2)) { target1 } else { target2 }
        }
        val possible_directions = List(North,East,South,West).filter(d => game.directionFrom(ant.tile).to(target).contains(d))
        getOrder(game,ant,possible_directions,state._3) match {
	  case Some(order) => {
	    val new_blocked_locations = game.tile(order.point).of(ant.tile) +: state._3
	    (state._1 + order,state._2.filterNot(_ == target),new_blocked_locations,ant +: state._4)}
	  case None => state
        }
      }
    }}
    (result._1,available_ants.filterNot(x => result._4.contains(x)),result._3)
  }

  private def getOrder(game: Game, ant: MyAnt, possible_directions: List[CardinalPoint], blocked_locations: List[Tile]) : Option[Order] = {
    val direction: Option[CardinalPoint] = possible_directions.find { aim =>
      val targetTile = game.tile(aim).of(ant.tile)
      !game.board.water.contains(targetTile) && !blocked_locations.contains(targetTile) }
    direction match {
      case Some(aim) =>	Some(Order(ant.tile,aim))
      case None => None
    }
  }
}
