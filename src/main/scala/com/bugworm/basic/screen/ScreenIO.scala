package com.bugworm.basic.screen

import com.bugworm.basic.BasicIO
import javafx.application.Application
import javafx.scene.input.KeyCode
import scalafx.beans.property.PropertyIncludes._
import scalafx.event.EventHandler
import scalafx.scene.input.KeyEvent
import com.bugworm.basic.BasicRuntime

class ScreenIO(val model : ScreenModel) extends BasicIO{

    val keys = Array(false, false, false, false)

    var loop : Option[Loop] = None

    def printStr(st : String, ln : Boolean){

        model.print(st)
        if(ln){
            model.newLine
        }
    }

    def input : String = {
        throw new UnsupportedOperationException
    }

    def color(f : BigDecimal){
        //TODO
    }

    def locate(x : BigDecimal, y : BigDecimal) {
        model.cursorX.value = x.intValue
        model.cursorY.value = y.intValue
    }

    def inkey = {
        throw new UnsupportedOperationException
    }

    def stick(n : BigDecimal) : BigDecimal = {
        //TODO nは本当はデバイスの指定に使いたい
        keys match {
            case Array(true, false, false, false) => BigDecimal(1)
            case Array(true, false, false, true) => BigDecimal(2)
            case Array(false, false, false, true) => BigDecimal(3)
            case Array(false, true, false, true) => BigDecimal(4)
            case Array(false, true, false, false) => BigDecimal(5)
            case Array(false, true, true, false) => BigDecimal(6)
            case Array(false, false, true, false) => BigDecimal(7)
            case Array(true, false, true, false) => BigDecimal(8)
            case _ => BigDecimal(0)
        }
    }

    def keyControl(event : KeyEvent) {

        val press = event.eventType == KeyEvent.KeyPressed
        event.code match{
	        case KeyCode.UP => keys(0) = press
	        case KeyCode.DOWN => keys(1) = press
	        case KeyCode.LEFT => keys(2) = press
	        case KeyCode.RIGHT => keys(3) = press
	        case _ =>
        }
    }

    def keyTyped(event : KeyEvent) {
        //TODO
    }

    def cycle(n : BigDecimal, runtime : BasicRuntime){

        loop.foreach(_.stop)
        loop = Option(new Loop(n.doubleValue(), runtime))
        loop.get.play()
    }
}