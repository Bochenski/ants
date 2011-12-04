
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

    val food_orders = getFoodOrders(game,ants,game.board.food.values,my_hill_tiles)
    System.err.print("remainingAnts after food orders: " + food_orders._2.toString)
    val explore_orders = getExplorerOrders(game, food_orders._2,unexplored,food_orders._3)
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

  private def getExplorerOrders(game: Game, available_ants: Iterable[MyAnt], unclaimed_targets: Iterable[Tile], 
				new_locations: List[Tile]) : (Set[Order], Iterable[MyAnt], List[Tile]) = {
    if ((available_ants.size == 0) || (unclaimed_targets.size == 0)) {
      (Set[Order](), available_ants,new_locations)
    } else { 
      val ant = available_ants.head
      val target = unclaimed_targets.reduceLeft[Tile]{(target1, target2) => 
	if(game.distanceFrom(ant.tile).to(target1) < game.distanceFrom(ant.tile).to(target2)) { target1 } else { target2 }
      }
    
      val directions = List(North,East,South,West)
      val possible_directions = directions.filter(d => game.directionFrom(ant.tile).to(target).contains(d))
      val order = getOrder(game,ant,possible_directions,new_locations)

      val remaining_ants = available_ants.filterNot(x => x  == ant)
      (order._1.toSet ++ getExplorerOrders(game,remaining_ants,unclaimed_targets.filterNot(x => x == target),order._2)._1,remaining_ants,order._2)						    }  
}

  private def getFoodOrders(game: Game, available_ants: Iterable[MyAnt], remaining_food: Iterable[Food], new_locations: List[Tile] ) : (Set[Order],Iterable[MyAnt],List[Tile]) = {
    if ((available_ants.size == 0) || (remaining_food.size == 0)) {
      (Set[Order](),available_ants,new_locations)
    } else { 
      val morsel = remaining_food.head
      val gatherer = available_ants.reduceLeft[MyAnt]{ (ant1 , ant2) =>
        if (game.distanceFrom(ant1.tile).to(morsel.tile) < game.distanceFrom(ant2.tile).to(morsel.tile) ) { ant1 } else { ant2 }						
      }
      val directions = List(North,East,South,West)
      val possible_directions = directions.filter( d => game.directionFrom(gatherer.tile).to(morsel.tile).contains(d))
      val order = getOrder(game,gatherer,possible_directions,new_locations)
      val remaining_ants = available_ants.filterNot( x => x == gatherer)
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
