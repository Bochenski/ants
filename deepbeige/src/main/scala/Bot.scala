trait Bot {
  def ordersFrom(gameState: Game): (Set[Order],Map[Tile,Option[MyAnt]])
}
