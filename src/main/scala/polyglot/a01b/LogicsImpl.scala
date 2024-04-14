package polyglot.a01b

import polyglot.{OptionToOptional, Pair}
import util.Optionals.Optional as ScalaOptional
import util.Sequences.*
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:

  private var minesSet: Sequence[Pair[Int, Int]] = Sequence.empty

  private var selected: Sequence[Pair[Int, Int]] = Sequence.empty

  val random = new Random()

  while(this.minesSet.size != this.mines)
    this.minesSet = this.minesSet.add(new Pair(random.nextInt(size), random.nextInt(size)))

  def neighbours(x: Int, y: Int): Int =
    var count: Int = 0
    for neighbourX <- x - 1 to x + 1 do
      for neighbourY <- y - 1 to y + 1 do
        if !(neighbourX == x && neighbourY == y) && this.minesSet.contains(new Pair(neighbourX, neighbourY))
          then count = count + 1
    count // number of neighbours containing a mine

  override def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if this.minesSet.contains(Pair(x, y)) then OptionToOptional(ScalaOptional.Empty())
    else
      this.selected = this.selected.add(new Pair(x, y))
      OptionToOptional(ScalaOptional.Just(neighbours(x, y)))

  override def won: Boolean = this.selected.size == (this.size * this.size) - this.minesSet.size