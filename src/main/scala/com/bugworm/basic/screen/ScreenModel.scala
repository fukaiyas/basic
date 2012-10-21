package com.bugworm.basic.screen

import scalafx.beans.property.IntegerProperty
import scalafx.beans.property.StringProperty

case class ScreenModel{

    val width = 40

    val height = 25

    val cursorX : IntegerProperty = IntegerProperty(0)

    val cursorY : IntegerProperty  = IntegerProperty(0)

    val text : Array[Array[StringProperty]] = Array.fill(width, height)(new StringProperty)

    def print(st : String){
        st.foreach{
            c =>
        	text(cursorX.value)(cursorY.value).value = c.toString
        	addX(1)
        }
    }

    def newLine(){
        //TODO ここでもしかして改行コードをいれとくのかも?
        addY(1)
        cursorX.value = 0
    }

    def addX(v : Int){
    	cursorX.value = cursorX.value + v
	    while(cursorX.value >= width){
    	    addY(1)
    	    cursorX.value = cursorX.value - width
    	}
    }

    def addY(v : Int){
        cursorY.value = cursorY.value + v
        if(cursorY.value >= height){
            val sh = cursorY.value - height + 1
    		for(x <- 0 until 40; y <- 0 until 25 - sh){
    		    text(x)(y).value = text(x)(y + sh).value
    		    text(x)(y + sh).value = ""
    		    //TODO 色が入ったら色情報も必要
    		}
            cursorY.value = height - 1
        }
    }
}