package exercises01


import java.lang.Math.pow

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(this.x + other.x, this.y + other.y)

  def -(other: Vector): Vector = new Vector(this.x - other.x, this.y - other.y)

  def *(scalar: Double): Vector = new Vector(this.x * scalar, this.y * scalar )

  def unary_- : Vector = new Vector(-this.x, -this.y)

  def euclideanLength: Double = pow( pow(this.x,2)+pow(this.y,2), 0.5)

  def normalized: Vector = if (pow(this.x,2)+pow(this.y,2) == 0) new Vector(0,0) else
                            new Vector(this.x/pow(pow(this.x,2)+pow(this.y,2),0.5),
                              this.y/pow(pow(this.x,2)+pow(this.y,2),0.5))

  override def equals(other: Any): Boolean = other match{
    case Vector(x, y) => (this.x == x & this.y == y)
    case _ => false
    }

  // Vector(x, y)
  override def toString: String = "Vector(" +String.valueOf(this.x)+", "+String.valueOf(this.y)+")"
}

object Vector {
  def fromAngle(angle: Double, length: Double): Vector = new Vector(
                                                                    java.lang.Math.cos(angle)*length,
                                                                    java.lang.Math.sin(angle)*length
                                                                   )
  def sum(list: List[Vector]): Vector = {
    var r = new Vector(0, 0)
    for (x <- list)
    {
      r = r + x
    }
    r
  }

  def unapply(arg: Vector): Option[(Double, Double)] =Option((arg.x, arg.y))
}
