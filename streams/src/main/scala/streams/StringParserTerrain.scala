package streams

import common._

/**
 * This component implements a parser to define terrains from a
 * graphical ASCII representation.
 * 
 * When mixing in that component, a level can be defined by
 * defining the field `level` in the following form:
 * 
 *   val level =
 *     """------
 *       |--ST--
 *       |--oo--
 *       |--oo--
 *       |------""".stripMargin
 * 
 * - The `-` character denotes parts which are outside the terrain
 * - `o` denotes fields which are part of the terrain
 * - `S` denotes the start position of the block (which is also considered
     inside the terrain)
 * - `T` denotes the final position of the block (which is also considered
     inside the terrain)
 * 
 * In this example, the first and last lines could be omitted, and
 * also the columns that consist of `-` characters only.
 */
trait StringParserTerrain extends GameDef {

  /**
   * A ASCII representation of the terrain. This field should remain
   * abstract here.
   */
  val level: String

  /**
   * This method returns terrain function that represents the terrain
   * in `levelVector`. The vector contains parsed version of the `level`
   * string. For example, the following level
   * 
   *   val level =
   *     """ST
   *       |oo
   *       |oo""".stripMargin
   * 
   * is represented as
   * 
   *   Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
   *
   * The resulting function should return `true` if the position `pos` is
   * a valid position (not a '-' character) inside the terrain described
   * by `levelVector`.
   */

  def getRow(x_Pos: Int, levelVector: Vector[Vector[Char]]): Vector[Char] = {
    if (levelVector.isEmpty) Vector.empty
    else {
      if (x_Pos == 0) levelVector.head
      else getRow(x_Pos - 1, levelVector.tail)
    }
  }

  def getPoint(y_Pos: Int, curVector: Vector[Char]): Boolean = {
    if (curVector.isEmpty) false
    else {
      if (y_Pos == 0) (curVector.head != '-')
      else getPoint(y_Pos - 1, curVector.tail)
    }
  }

  //def terrainFunction_sub(x_Pos: Int, y_Pos: Int, levelVector: Vector[Vector[Char]]): Boolean = getPoint(y_Pos, getRow(x_Pos, levelVector))
    /*
    if (x_Pos >= levelVector.length) false
    else {
      val cur = getRow(x_Pos, levelVector)
      if (y_Pos >= cur.length) false else getPoint(y_Pos, cur)
    }
    */


  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = (given: Pos) => getPoint(given.y, getRow(given.x, levelVector))
    //terrainFunction_sub(given.x, given.y, levelVector)

  /**
   * This function should return the position of character `c` in the
   * terrain described by `levelVector`. You can assume that the `c`
   * appears exactly once in the terrain.
   *
   * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
   * `Vector` class
   */

  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    val x_Pos = levelVector.indexWhere( (x: Vector[Char]) => x.contains(c) )
    val y_Pos = getRow(x_Pos, levelVector).indexOf(c)
    Pos(x_Pos, y_Pos)
  }

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

}
