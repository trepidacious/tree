package org.rebeam.tree.view

import io.circe.generic.JsonCodec

@JsonCodec
case class ColorHSLA(h: Float, s: Float, l: Float, a: Int)

@JsonCodec
case class Color(r: Int, g: Int, b: Int, a: Int) {

  def alphaUnitScale: Float = a/255.0f

  def toHSLA: ColorHSLA = {
    val rgb = Vector(r/255.0f, g/255.0f, b/255.0f)

    val min = rgb.min
    val max = rgb.max
    val maxIndex = rgb.indexOf(max)

    val range: Float = max - min
    val noRange: Boolean = range < 0.0001

    val h1: Float = if (noRange) {
      0
    } else if (maxIndex == 0) {
      (rgb(1) - rgb(2)) / range
    } else if (maxIndex == 1) {
      2 + (rgb(2) - rgb(0)) / range
    } else {
      4 + (rgb(0) - rgb(1)) / range
    } * 60

    val h = if (h1 < 0) {
      h1 + 360
    } else {
      h1
    }

    val l = (min + max) / 2

    val s = if (noRange) {
      0
    } else {
      if (l < 0.5) {
        range / (max + min)
      } else {
        range / (2 - max - min)
      }
    }
    ColorHSLA(h, s, l, a)
  }

  override def toString() = if (a == 255) s"rgb($r, $g, $b)" else s"rgba($r, $g, $b, $alphaUnitScale)"

//  def *(c: Color) = Color(r * c.r, g * c.g, b * c.b)
//
//  def +(c: Color) = Color(r + c.r, g + c.g, b + c.b)
}

object Color {

  private val d = "[0-9a-zA-Z]"
  private val RGB = "rgb\\((\\d+), (\\d+), (\\d+)\\)".r
  private val ShortHex = s"#($d)($d)($d)".r
  private val LongHex = s"#($d$d)($d$d)($d$d)".r
  private val RGBA = "rgba\\((\\d+), (\\d+), (\\d+), ([-+]?[0-9]*\\.?[0-9]+)\\)".r

  def apply(r: Int, g: Int, b: Int): Color = Color(r, g, b, 255)

  def apply(grey: Int): Color = Color(grey, grey, grey)

  def hex(x: String) = Integer.parseInt(x, 16)

  def unitStringToByte(s: String) = Math.max(Math.min(255, Math.round(s.toFloat * 255.0f)), 0)
  def stringToByte(s: String) = Math.max(Math.min(255, Math.round(s.toFloat)), 0)

  def lerp(a: Color, b: Color, l: Float): Color = {
    Color(
      (a.r + l * (b.r - a.r)).toInt,
      (a.g + l * (b.g - a.g)).toInt,
      (a.b + l * (b.b - a.b)).toInt,
      (a.a + l * (b.a - a.a)).toInt
    )
  }

  def apply(s: String): Color = {
    s match {
      //Numbers can be floating point, just round to byte.
      case RGB(r, g, b)         => Color(stringToByte(r), stringToByte(g), stringToByte(b))
      case ShortHex(r, g, b)    => Color(hex(r) * 16, hex(g) * 16, hex(b) * 16)
      case LongHex(r, g, b)     => Color(hex(r), hex(g), hex(b))
      //Numbers can be floating point, just round to byte. Note alpha is in unit scale
      case RGBA(r, g, b, alpha) => Color(stringToByte(r), stringToByte(g), stringToByte(b), unitStringToByte(alpha))
    }
  }

  val White = Color(255, 255, 255)
  val Red = Color(255, 0, 0)
  val Green = Color(0, 255, 0)
  val Blue = Color(0, 0, 255)
  val Cyan = Color(0, 255, 255)
  val Magenta = Color(255, 0, 255)
  val Yellow = Color(255, 255, 0)
  val Black = Color(0, 0, 0)

  val all = Seq(
    White,
    Red,
    Green,
    Blue,
    Cyan,
    Magenta,
    Yellow,
    Black
  )

}
