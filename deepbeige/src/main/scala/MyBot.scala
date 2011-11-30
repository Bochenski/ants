
object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  def ordersFrom(game: Game): Set[Order] = {

    val directions = List(North, East, South, West)
    val ants = game.board.myAnts.values
    val food = game.board.food.values

    var new_locations = List[Tile]() 
    var busyAnts = List[MyAnt]()

    val food_orders = food.flatMap{ morsel =>
      val gatherer = ants.reduceLeft[MyAnt]{ (ant1 , ant2) =>
	val ant1_distance = game.distanceFrom(ant1.tile).to(morsel.tile)
	val ant2_distance = game.distanceFrom(ant2.tile).to(morsel.tile)
        if (ant1_distance < ant2_distance) { ant1 } else { ant2 }						
      }
				   
      val direction = directions.filter( d => game.directionFrom(gatherer.tile).to(morsel.tile).contains(d)).find { aim =>
	val targetTile = game.tile(aim).of(gatherer.tile)
          !game.board.water.contains(targetTile) && !new_locations.contains(targetTile)													         }
        new_locations = direction match {
	  case Some(aim) => { 
	    new_locations :+ game.tile(aim).of(gatherer.tile)
	  }
	  case None => new_locations
	}
      direction.map{ d => Order(gatherer.tile,d)}
    }.toSet

    System.err.println(food_orders.toString)
    food_orders
  //  ants.flatMap{ant =>
      // for this ant, find the first direction which is not water, if any
  //    val direction = directions.filter( d => game.directionFrom(onTILE).to(anotherTile).contains(d))find{aim =>
  //      val targetTile = game.tile(aim).of(ant.tile)
  //      !game.board.water.contains(targetTile) && !new_locations.contains(targetTile)
  // /   } 
  //    new_locations = direction match {
//	case Some(aim) => {
//	new_locations :+ game.tile(aim).of(ant.tile)
        // convert this (possible) direction into an order for this ant
//	}
//	case None => new_locations
//      }
//      direction.map{d => Order(ant.tile, d)}
//    }.toSet
  }
}
