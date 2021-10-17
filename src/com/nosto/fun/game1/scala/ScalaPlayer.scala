package com.nosto.fun.game1.scala

import com.nosto.fun.game1._

/**
 * Trait for a computer player implemented with Scala.
 *
 * - Implement your algorithm to extend this trait.
 * - Add your algorithm to a separate package based on your name such as com.nosto.fun.add_your_name_here.
 * - Your target is to get 5 pieces in a row.
 * - To add your algorithm to the games dropdown menus add it to MainJFrame class similarly as existing
 *   RandomScalaOpponent.
 */
trait ScalaPlayer extends Player {
  /**
   * This method is called by the game framework to notify you of your side. Side (Round or
   * Cross) is randomly chosen by the game framework at the beginning of the match.
   *
   * @param piece Piece.ROUND or Piece.CROSS depending on your luck.
   */
  override def setSide(piece: Piece): Unit

  /**
   * This should always return the side that you've been informed with by the setSide-method
   *
   * @return Piece given with setSide-method
   */
  override def getSide(): Piece

  /**
   * Here is your main implementation point. This method is called to determine your next move.
   * The given parameters are informational and you can modify them if you want, your
   * modifications will be ignored. The result of your algorithm is announced with the return
   * value.
   *
   * @param board Current board status (first dimension is the board's rows, and the second is the
   *        board's columns). A single cell in the board table can be either: Piece.CROSS,
   *        Piece.ROUND or null. null means that the cell isn't taken yet.
   * @param last A helper parameter to inform you about the last move made by your opponent.
   * @return ArenaPosition instance that describes on which board cell you want to place your piece.
   */
  override def move(board: Array[Array[Piece]], last: ArenaPosition): ArenaPosition

  /**
   * @return your player's name
   */
  override def getName(): String

  /**
   * @return your player's name
   */
  override def toString(): String = getName()
}
