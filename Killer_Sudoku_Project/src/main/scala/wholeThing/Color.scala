package wholeThing


class Color(val color: String, val hex: String):
  override def toString: String = color

object Red extends Color("red", "#FF6865")
object Blue extends Color("blue", "#ADD8E6")
object Yellow extends Color("yellow", "#E8E337")
object Green extends Color("green", "#86DC3D")


