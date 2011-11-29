
object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  def ordersFrom(game: Game): Set[Order] = {

    val directions = List(North, East, South, West)
    val ants = game.board.myAnts.values
    val food = game.board.food.values
    var new_locations = List[Tile]() 

    val food_routes = food.map{ morsel => morsel ->
      ants.map { ant => ant ->
	game.distanceFrom(ant.tile).to(morsel.tile) }
    }

    System.err.println(food_routes.toString)

//    val food_assignments: Map[MyAnt,Food] = food.map{ morsel => 

      //find ant closest to this morsel of food
//    }

    ants.flatMap{ant =>
      // for this ant, find the first direction which is not water, if any
      val direction = directions.find{aim =>
        val targetTile = game.tile(aim).of(ant.tile)
        !game.board.water.contains(targetTile) && !new_locations.contains(targetTile)
      }
      new_locations = direction match {
	case Some(aim) => {
	new_locations :+ game.tile(aim).of(ant.tile)
        // convert this (possible) direction into an order for this ant
	}
	case None => new_locations
      }
      direction.map{d => Order(ant.tile, d)}
    }.toSet
  }
}
