package com.github.unthingable

import com.bitwig.extension.controller.api.{SettableEnumValue, Settings}

object JamSettings {
/*
  object ShowHide extends Enumeration {
    val Show, Hide = Value
  }

  object LimitLevels extends Enumeration {
    val None = Value
    val Zero = Value("0dB")
    val MinusTen = Value("-10dB")
    val Smart = Value
  }

  object DpadScroll extends Enumeration {
    val RegularOne = Value("single/page")
    val RegularPage = Value("page/single")
  }

  trait EnumSetting[A <: Enumeration] {
    def outerEnum: A
    def setting: SettableEnumValue
    def set(v: A#Value): Unit
    def get(): A#Value
    def addValueObserver(f: A#Value => Unit): Unit
  }

  def enumSetting[A <: Enumeration: ValueOf](p: Settings, s: String, category: String, init: A#Value) =
    new EnumSetting[A] {
      val outerEnum: A = valueOf[A]

      // side effect expected on creation
      val setting: SettableEnumValue = p.getEnumSetting(s, category, outerEnum.values.toArray.map(_.toString), init.toString)
      setting.markInterested()

      def set(v: A#Value): Unit = setting.set(v.toString)

      def get(): A#Value = outerEnum.withName(setting.get())

      def addValueObserver(f: A#Value => Unit): Unit = setting.addValueObserver(v => f(outerEnum.withName(v)))
    }
*/

  // import reflect.Selectable.reflectiveSelectable

  enum ShowHide:
    //  extends EnumT[ShowHide]:
    case Show, Hide

  enum LimitLevels:
    //  extends EnumT[LimitLevels]:
    case None, `0dB`, `-10dB`, Smart

  enum DpadScroll:
    //  extends EnumT[DpadScroll]:
    case `single/page`, `page/single`

  // trait EnumT[A] {
  //   def values: Array[A]
  //   def valueOf(name: String): A
  // }

  trait EnumSetting[E] {
    def setting: SettableEnumValue
    def set(v: E): Unit
    def get(): E
    def addValueObserver(f: E => Unit): Unit
  }

  import scala.compiletime.{summonAll, summonInline}
  import com.github.unthingable.util.MacroImpl

  object EnumSetting:
    inline def apply[E <: reflect.Enum](p: Settings, s: String, category: String, init: E): EnumSetting[E] =
      new EnumSetting[E]:
        val values: Array[E] = MacroImpl.enumValues[E]
        def valueOf(string: String): Option[E] = values.find(_.toString == string)
        
        // side effect expected on creation
        val setting: SettableEnumValue = p.getEnumSetting(s, category,
          values.map(_.toString),
          init.toString)

        setting.markInterested()

        def set(v: E): Unit = setting.set(v.toString)

        def get(): E = valueOf(setting.get()).get

        def addValueObserver(f: E => Unit): Unit = setting.addValueObserver(v => f(valueOf(v).get))
}