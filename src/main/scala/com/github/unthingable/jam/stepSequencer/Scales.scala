package com.github.unthingable.jam.stepSequencer

object scales:
  opaque type Interval = Int

  case class Scale(name: String, intervals: Set[Interval])(root: Int): // notes in intervals from root, always 8 except chromatic
    lazy val fullScale: IndexedSeq[Int] = (0 until 128).filter(isInScale(_))
    lazy val fullScaleMap: Map[Int, Int] = fullScale.zipWithIndex.toMap

    inline def isInScale(inline note: Int): Boolean = 
      intervals.contains((root + note) % 12)

    // inline def nextInScale(inline note: Int): Option[Int] =
    //   (note until 128).view.find(isInScale(_))

    // inline def prevInScale(inline note: Int): Option[Int] =
    //   (note.to(0, -1)).view.find(isInScale(_))

  val scales: Vector[Int => Scale] = Vector(
    "Natural Minor"             -> "101101011010",
    "Major"                     -> "101011010101",
    "Dorian"                    -> "101101010110",
    "Phrygian"                  -> "110101011010",
    "Mixolydian"                -> "101011010110",
    "Melodic Minor (ascending)" -> "101101010101",
    "Harmonic Minor"            -> "101101011001",
    "Bebop Dorian"              -> "100111010110",
    "Blues"                     -> "100101110010",
    "Minor Pentatonic"          -> "100101010010",
    "Hungarian Minor"           -> "101100111001",
    "Ukranian Dorian"           -> "101100110110",
    "Marva"                     -> "110010110101",
    "Todi"                      -> "110100111001",
    "Whole Tone"                -> "101010101010",
    "Chromatic"                 -> "111111111111",
  ).map((name, seq) =>
    Scale(name, seq.toCharArray.zipWithIndex.filter(_._1 == '1').map(_._2).toSet)
  )
