package astrionic.adventofcode2020.solutions.day11

import astrionic.adventofcode2020.solutions.day11.SeatStatus.Seat

case class GridLocation(
    status: Seat,
    hasChanged: Boolean
)
