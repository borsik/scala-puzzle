package com.nosto.fun.game1.scala

import com.nosto.fun.game1.{ArenaPosition, Piece}

import scala.util.Random

class RandomScalaOpponent(name: String) extends ScalaPlayer with Cloneable {
  private var myPiece: Piece = null

  override def getName(): String = name

  override def getSide(): Piece = myPiece

  override def setSide(piece: Piece): Unit = {
    myPiece = piece
  }

  override def move(board: Array[Array[Piece]], lastPosition: ArenaPosition): ArenaPosition = {
    val emptySlots = board.map(_.zipWithIndex.collect {
      case (null, index) => index
    }).zipWithIndex.flatMap {
      case (rows, col) => rows.map((_, col))
    }
    emptySlots(Random.nextInt(emptySlots.size)) match {
      case (row, col) => new ArenaPosition(col, row)
    }
  }
}
