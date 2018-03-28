package org.rebeam.tree.view

object MaterialColor {

  private def applyShade(shade: Int, data: String*): Color = {
    //Exact shades
    if (shade == 50) {
      Color(data.head)
    } else if (shade % 100 == 0 && shade >= 100 && shade <= 900) {
      Color(data(shade / 100))

      //Shade <= 0 is white
    } else if (shade <= 0) {
      Color.White

      //Shade less than 50 lerps to white at 0
    } else if (shade <= 50) {
      Color.lerp(Color.White, Color(data.head), shade/50.0f)

      //Shade from 50 to 100 lerps from 50 to 100
    } else if (shade <= 100) {
      Color.lerp(Color(data.head), Color(data(1)), (shade - 50)/50.0f)

      //Shade from 100 to 900 lerps between values at each 100
    } else if (shade < 900) {
      val f = shade/100
      Color.lerp(Color(data(f)), Color(data(f + 1)), (shade % 100)/100.0f)

      //Shade 900 or over is just 900
    } else {
      Color(data.last)
    }
  }

  sealed trait Family {
    def apply(): Color
    def name: String
  }

  sealed trait FamilyWithShades extends Family {
    def apply(shade: Int): Color
  }

  sealed trait FamilyWithAccents extends FamilyWithShades {
    val a100: Color
    val a200: Color
    val a400: Color
    val a700: Color
  }

  case class SingleFamily(name: String, color: Color) extends Family {
    def apply(): Color = color
  }

  case class ShadedFamily(name: String, data: String*) extends FamilyWithShades {
    def apply(): Color = apply(500)
    def apply(shade: Int): Color = applyShade(shade, data: _*)
  }

  case class AccentedFamily(name: String, data: String*) extends FamilyWithAccents {
    lazy val a100: Color = Color(data(10))
    lazy val a200: Color = Color(data(11))
    lazy val a400: Color = Color(data(12))
    lazy val a700: Color = Color(data(13))
    def apply(): Color = apply(500)
    def apply(shade: Int): Color = applyShade(shade, data: _*)
  }

  object Red extends AccentedFamily(
    "Red",
    "#ffebee",
    "#ffcdd2",
    "#ef9a9a",
    "#e57373",
    "#ef5350",
    "#f44336",
    "#e53935",
    "#d32f2f",
    "#c62828",
    "#b71c1c",
    "#ff8a80",
    "#ff5252",
    "#ff1744",
    "#d50000"
  )

  object Pink extends AccentedFamily(
    "Pink",
    "#fce4ec",
    "#f8bbd0",
    "#f48fb1",
    "#f06292",
    "#ec407a",
    "#e91e63",
    "#d81b60",
    "#c2185b",
    "#ad1457",
    "#880e4f",
    "#ff80ab",
    "#ff4081",
    "#f50057",
    "#c51162"
  )

  object Purple extends AccentedFamily(
    "Purple",
    "#f3e5f5",
    "#e1bee7",
    "#ce93d8",
    "#ba68c8",
    "#ab47bc",
    "#9c27b0",
    "#8e24aa",
    "#7b1fa2",
    "#6a1b9a",
    "#4a148c",
    "#ea80fc",
    "#e040fb",
    "#d500f9",
    "#aa00ff"
  )
  object DeepPurple extends AccentedFamily(
    "Deep Purple",
    "#ede7f6",
    "#d1c4e9",
    "#b39ddb",
    "#9575cd",
    "#7e57c2",
    "#673ab7",
    "#5e35b1",
    "#512da8",
    "#4527a0",
    "#311b92",
    "#b388ff",
    "#7c4dff",
    "#651fff",
    "#6200ea"
  )
  object Indigo extends AccentedFamily(
    "Indigo",
    "#e8eaf6",
    "#c5cae9",
    "#9fa8da",
    "#7986cb",
    "#5c6bc0",
    "#3f51b5",
    "#3949ab",
    "#303f9f",
    "#283593",
    "#1a237e",
    "#8c9eff",
    "#536dfe",
    "#3d5afe",
    "#304ffe"
  )
  object Blue extends AccentedFamily(
    "Blue",
    "#e3f2fd",
    "#bbdefb",
    "#90caf9",
    "#64b5f6",
    "#42a5f5",
    "#2196f3",
    "#1e88e5",
    "#1976d2",
    "#1565c0",
    "#0d47a1",
    "#82b1ff",
    "#448aff",
    "#2979ff",
    "#2962ff"
  )
  object LightBlue extends AccentedFamily(
    "Light Blue",
    "#e1f5fe",
    "#b3e5fc",
    "#81d4fa",
    "#4fc3f7",
    "#29b6f6",
    "#03a9f4",
    "#039be5",
    "#0288d1",
    "#0277bd",
    "#01579b",
    "#80d8ff",
    "#40c4ff",
    "#00b0ff",
    "#0091ea"
  )
  object Cyan extends AccentedFamily(
    "Cyan",
    "#e0f7fa",
    "#b2ebf2",
    "#80deea",
    "#4dd0e1",
    "#26c6da",
    "#00bcd4",
    "#00acc1",
    "#0097a7",
    "#00838f",
    "#006064",
    "#84ffff",
    "#18ffff",
    "#00e5ff",
    "#00b8d4"
  )
  object Teal extends AccentedFamily(
    "Teal",
    "#e0f2f1",
    "#b2dfdb",
    "#80cbc4",
    "#4db6ac",
    "#26a69a",
    "#009688",
    "#00897b",
    "#00796b",
    "#00695c",
    "#004d40",
    "#a7ffeb",
    "#64ffda",
    "#1de9b6",
    "#00bfa5"
  )
  object Green extends AccentedFamily(
    "Green",
    "#e8f5e9",
    "#c8e6c9",
    "#a5d6a7",
    "#81c784",
    "#66bb6a",
    "#4caf50",
    "#43a047",
    "#388e3c",
    "#2e7d32",
    "#1b5e20",
    "#b9f6ca",
    "#69f0ae",
    "#00e676",
    "#00c853"
  )
  object LightGreen extends AccentedFamily(
    "Light Green",
    "#f1f8e9",
    "#dcedc8",
    "#c5e1a5",
    "#aed581",
    "#9ccc65",
    "#8bc34a",
    "#7cb342",
    "#689f38",
    "#558b2f",
    "#33691e",
    "#ccff90",
    "#b2ff59",
    "#76ff03",
    "#64dd17"
  )
  object Lime extends AccentedFamily(
    "Lime",
    "#f9fbe7",
    "#f0f4c3",
    "#e6ee9c",
    "#dce775",
    "#d4e157",
    "#cddc39",
    "#c0ca33",
    "#afb42b",
    "#9e9d24",
    "#827717",
    "#f4ff81",
    "#eeff41",
    "#c6ff00",
    "#aeea00"
  )
  object Yellow extends AccentedFamily(
    "Yellow",
    "#fffde7",
    "#fff9c4",
    "#fff59d",
    "#fff176",
    "#ffee58",
    "#ffeb3b",
    "#fdd835",
    "#fbc02d",
    "#f9a825",
    "#f57f17",
    "#ffff8d",
    "#ffff00",
    "#ffea00",
    "#ffd600"
  )
  object Amber extends AccentedFamily(
    "Amber",
    "#fff8e1",
    "#ffecb3",
    "#ffe082",
    "#ffd54f",
    "#ffca28",
    "#ffc107",
    "#ffb300",
    "#ffa000",
    "#ff8f00",
    "#ff6f00",
    "#ffe57f",
    "#ffd740",
    "#ffc400",
    "#ffab00"
  )
  object Orange extends AccentedFamily(
    "Orange",
    "#fff3e0",
    "#ffe0b2",
    "#ffcc80",
    "#ffb74d",
    "#ffa726",
    "#ff9800",
    "#fb8c00",
    "#f57c00",
    "#ef6c00",
    "#e65100",
    "#ffd180",
    "#ffab40",
    "#ff9100",
    "#ff6d00"
  )
  object DeepOrange extends AccentedFamily(
    "Deep Orange",
    "#fbe9e7",
    "#ffccbc",
    "#ffab91",
    "#ff8a65",
    "#ff7043",
    "#ff5722",
    "#f4511e",
    "#e64a19",
    "#d84315",
    "#bf360c",
    "#ff9e80",
    "#ff6e40",
    "#ff3d00",
    "#dd2c00"
  )
  object Brown extends ShadedFamily(
    "Brown",
    "#efebe9",
    "#d7ccc8",
    "#bcaaa4",
    "#a1887f",
    "#8d6e63",
    "#795548",
    "#6d4c41",
    "#5d4037",
    "#4e342e",
    "#3e2723"
  )
  object Grey extends ShadedFamily(
    "Grey",
    "#fafafa",
    "#f5f5f5",
    "#eeeeee",
    "#e0e0e0",
    "#bdbdbd",
    "#9e9e9e",
    "#757575",
    "#616161",
    "#424242",
    "#212121"
  )
  object BlueGrey extends ShadedFamily(
    "Blue Grey",
    "#eceff1",
    "#cfd8dc",
    "#b0bec5",
    "#90a4ae",
    "#78909c",
    "#607d8b",
    "#546e7a",
    "#455a64",
    "#37474f",
    "#263238"
  )
  object Black extends SingleFamily("Black", Color(0, 0, 0))
  object White extends SingleFamily("White", Color(255, 255, 255))

  val accented: Seq[AccentedFamily] = Seq(
    Red, Pink,
    Purple, DeepPurple,
    Indigo,
    Blue, LightBlue,
    Cyan,
    Teal,
    Green, LightGreen,
    Lime,
    Yellow,
    Amber,
    Orange, DeepOrange
  )

  val shaded: Seq[ShadedFamily] = Seq(
    Brown, Grey, BlueGrey
  )

  val single: Seq[SingleFamily] = Seq(
    Black, White
  )

  val all: Seq[Family] = Seq(accented, shaded, single).flatten

  // Backgrounds, using all the accented colors that have
  // shades suitable as a background for white text. In each case
  // we use the lightest shade suitable for use with white text.
  val backgrounds: Seq[Color] = Seq(
    MaterialColor.Red(600),
    MaterialColor.Pink(600),
    MaterialColor.Purple(600),
    MaterialColor.DeepPurple(600),
    MaterialColor.Indigo(600),
    MaterialColor.Blue(600),
    //    MaterialColor.LightBlue(600),
    MaterialColor.Cyan(800),
    //    MaterialColor.Teal(500),
    MaterialColor.Green(700),
    MaterialColor.LightGreen(700),
    //    MaterialColor.Lime(700),
    //    MaterialColor.Yellow(800),
    //    MaterialColor.Amber(500),
    MaterialColor.Orange(900)
    //    MaterialColor.DeepOrange(500)
  )

  def backgroundForIndex(i: Int) = backgrounds(i % backgrounds.size)
}
