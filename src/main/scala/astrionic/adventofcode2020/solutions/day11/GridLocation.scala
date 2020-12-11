package astrionic.adventofcode2020.solutions.day11

import astrionic.adventofcode2020.solutions.day11.SeatStatus.SeatStatus

case class GridLocation(
    status: SeatStatus,
    hasChanged: Boolean
)
