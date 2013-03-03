package com.bugworm.basic.screen

import java.io.FileInputStream
import java.io.InputStreamReader
import com.bugworm.basic.Basic
import com.bugworm.basic.BasicRuntime
import com.bugworm.basic.Line
import javafx.application.Application
import javafx.stage.Stage
import scalafx.Includes._
import scalafx.animation.KeyFrame
import scalafx.animation.Timeline
import scalafx.beans.property.IntegerProperty.sfxIntegerProperty2jfx
import scalafx.beans.property.DoubleProperty
import scalafx.scene.Scene.sfxScene2jfx
import scalafx.scene.control.Button
import scalafx.scene.control.Label
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.Pane
import scalafx.scene.layout.StackPane
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color.sfxColor2jfx
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Font.sfxFont2jfx
import scalafx.scene.text.Font
import scalafx.scene.Scene
import javafx.scene.text.FontWeight
import javafx.scene.input.KeyEvent
import javafx.event.ActionEvent
import com.bugworm.basic.BasicRuntime

object BasicView{

    def main(args : Array[String]){

        Application.launch(classOf[BasicView])
    }
}

class BasicView extends Application{

	val model = new ScreenModel
    val sio = new ScreenIO(model)

	val button = new Button("Start"){
        onAction = startButton
    }

    def start(stage : Stage) {

        val textscreen = new GridPane{
        	prefWidth = 640
        	prefHeight = 400
        	hgap = 0
        	vgap = 0
        }
        val scene = new Scene {
	        onKeyPressed = {
	            event : KeyEvent => sio.keyPressed(event)
	        }
	        onKeyReleased = {
	            event : KeyEvent => sio.keyReleased(event)
	        }
	        onKeyTyped = {
	            event : KeyEvent => sio.keyTyped(event)
	        }
            fill = Color.BLACK
            content = new VBox{
	            content = Seq(
	                new Pane{
	                    content = Seq(button)
	                },
		            new StackPane {
		                prefWidth = 640
		                prefHeight = 400
		                content = Seq(
		                    new Pane{
		                        content = Seq(new Rectangle{
	                        		x <== model.cursorX * 16
                    				y <== model.cursorY * 16
                    				width = 16
                    				height = 16
                    				fill = Color.WHITE
                    				visible = false
		                        })
		                    },
		                    textscreen
		                )
		            }
	            )
            }
        }

        for(x <- 0 until 40; y <- 0 until 25){
            textscreen.add(new Label{
                textFill = Color.WHITE
                font = Font.font("monospaced", FontWeight.BOLD, 16)
                prefWidth = 16
                prefHeight = 16
                maxWidth = 16
                maxHeight = 16
                minWidth = 16
                minHeight = 16
                text <== model.text(x)(y)
            }, x, y)
        }
        
        stage.setScene(scene)
        stage.show
    }

    def startButton {
        val reader = new InputStreamReader(new FileInputStream("Sample2.basic"), "UTF-8")
        val runtime = new BasicRuntime(Basic.parse(reader).get)
        runtime.io = sio
        reader.close()
        runtime.cycle(BigDecimal.valueOf(50))
        button.disable = true
    }
}

class Loop(val frame : Double, val runtime : BasicRuntime) extends Timeline {

    var lastnano : Long = System.nanoTime
    cycleCount = Timeline.INDEFINITE
    keyFrames = KeyFrame(frame ms, "main loop [" + frame + "ms]", execute)
    def execute {
        val t = System.nanoTime
        println((lastnano - t) / 1000);
        lastnano = t
        runtime.sync = false
        var i = 0
        while(!runtime.sync && i < 100){
	        runtime.lines.lift(runtime.currentLine).getOrElse(Line.end).execute(runtime)
	        runtime.next()
	        if(runtime.terminated){
	            runtime.sync = true
	            println("terminated")
	            stop
	        }
	        i += 1
        }
        runtime.io.flush()
    }
}
