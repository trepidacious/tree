package org.rebeam.tree.view

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import View._
import MaterialColor._

object MaterialSwatch {

  val shades: Seq[Int] = Seq(50, 100, 200, 300, 400, 500, 600, 700, 800, 900)

  def swatches(name: String, colors: Seq[(String, Color)]) = {
    <.ul(
      colors.map(c => {
        val l = c._2.toHSLA.l
        <.li(
          name + " " + c._1,
          ^.color := (if (l < 0.6) Color.White else Color.Black).toString,
          ^.backgroundColor := c._2.toString()
        )
      })
    )
  }

  val SingleFamilyView = view[Family]("FamilyView") {
    f => swatches(f.name, Seq(("", f())))
  }

  val ShadedFamilyView = view[ShadedFamily]("FamilyView") {
    f => swatches(f.name, shades.map(shade => (shade.toString, f(shade))))
  }

  val AccentedFamilyView = view[AccentedFamily]("FamilyView") {
    f => swatches(
      f.name,
      Seq(
        shades.map(shade => (shade.toString, f(shade))),
        Seq(
          ("A100", f.a100),
          ("A200", f.a200),
          ("A400", f.a400),
          ("A700", f.a700)
        )
      ).flatten
    )
  }

  val AllFamiliesView = dynamicView("AllFamiliesView"){
    <.div(
      accented.map(f => AccentedFamilyView(f)),
      shaded.map(f => ShadedFamilyView(f)),
      single.map(f => SingleFamilyView(f))
    )
  }

}
