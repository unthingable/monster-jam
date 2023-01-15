package com.github.unthingable

import com.bitwig.extension.controller.api.{SettableEnumValue, Settings}

object JamSettings {

  enum ShowHide derives CanEqual:
    case Show, Hide

  enum LimitLevels derives CanEqual:
    case None, `0dB`, `-10dB`, Smart

  enum DpadScroll derives CanEqual:
    case `single/page`, `page/single`

  enum ShiftPlay derives CanEqual:
    case Restart, `Pause/Resume`

  trait EnumSetting[E] {
    def setting: SettableEnumValue
    def set(v: E): Unit
    def get(): E
    def addValueObserver(f: E => Unit): Unit
  }

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