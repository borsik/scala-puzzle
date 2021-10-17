package com.nosto.fun.olzhas

import com.nosto.fun.game1.{ArenaPosition, Piece}
import com.nosto.fun.game1.scala.ScalaPlayer

import scala.util.Try

class MinimaxScalaOpponent(name: String) extends ScalaPlayer with Cloneable {
  private var myPiece: Piece = null

  override def getName(): String = name

  override def getSide(): Piece = myPiece

  override def setSide(piece: Piece): Unit = {
    myPiece = piece
  }

  val otherPiece: Piece = Piece.values().filterNot(_ == myPiece).head

  def checkScore(board: Array[Array[Piece]], last: ArenaPosition): Option[Int] = {
    if (last == null) {
      None
    } else {
      val (i, j) = (last.row, last.column)
      val p = board(i)(j)
      val horizontal = Try(board(i)(j-2) == p && board(i)(j-1) == p && board(i)(j+1) == p && board(i)(j+2) == p).toOption
      val vertical = Try(board(i-2)(j) == p && board(i-1)(j) == p && board(i+1)(j) == p && board(i+2)(j) == p).toOption
      val leftTopDiag = Try(board(i-2)(j-2) == p && board(i-1)(j-1) == p && board(i+1)(j+1) == p && board(i+2)(j+2) == p).toOption
      val leftBottomDiag = Try(board(i-2)(j+2) == p && board(i-1)(j+1) == p && board(i+1)(j-1) == p && board(i+2)(j-2) == p && board(i+1)(j-1) == p).toOption
      val isFiveInRow = Seq(horizontal, vertical, leftTopDiag, leftBottomDiag).flatten.contains(true)
      if (isFiveInRow) {
        if (p == myPiece) Some(1)
        else Some(-1)
      } else if (!board.flatten.contains(null)) {
        Some(0)
      } else {
        None
      }
    }
  }

  def minimax(board: Array[Array[Piece]], last: ArenaPosition, depth: Int, isMaximizing: Boolean): Int = {
    checkScore(board, last) match {
      case Some(value) => value
      case None =>
        if (isMaximizing) {
          var bestScore = Int.MinValue
          for (i <- board.indices) {
            for (j <- board.indices) {
              if (board(i)(j) == null) {
                board(i)(j) = myPiece
                val newLast = new ArenaPosition(i, j)
                val score = minimax(board, newLast, depth + 1, false)
                board(i)(j) = null
                bestScore = Math.max(score, bestScore)
              }
            }
          }
          bestScore
        } else {
          var bestScore = Int.MaxValue
          for (i <- board.indices) {
            for (j <- board.indices) {
              if (board(i)(j) == null) {
                board(i)(j) = otherPiece
                val newLast = new ArenaPosition(i, j)
                val score = minimax(board, newLast, depth + 1, true)
                board(i)(j) = null
                bestScore = Math.min(score, bestScore)
              }
            }
          }
          bestScore
        }
    }
  }

  override def move(board: Array[Array[Piece]], last: ArenaPosition) = {
    var bestScore = Int.MinValue
    var bestMove: ArenaPosition = null
    for (i <- board.indices) {
      for (j <- board.indices) {
        if (board(i)(j) == null) {
          board(i)(j) = myPiece
          val newLast = new ArenaPosition(i, j)
          val score = minimax(board, newLast, 0, false)
          board(i)(j) = null
          if (score > bestScore) {
            bestScore = score
            bestMove = new ArenaPosition(i, j)
          }
        }
      }
    }
    bestMove
  }
}
