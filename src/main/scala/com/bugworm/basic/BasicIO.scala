package com.bugworm.basic

trait BasicIO {
    def printStr(st : String, ln : Boolean) : Unit
    def input : String
    def color(f : BigDecimal, b : BigDecimal = BigDecimal.valueOf(0)) : Unit
    def locate(x : BigDecimal, y : BigDecimal) : Unit

    def inkey : String
    def stick(n : BigDecimal) : BigDecimal
}

class ConsoleIO extends BasicIO{

    val colors = List(
            Console.BLACK, Console.BLUE, Console.RED, Console.MAGENTA,
            Console.GREEN, Console.CYAN, Console.YELLOW, Console.WHITE)

    def printStr(st : String, ln : Boolean){
        if(ln) println(st) else print(st)
        Console.flush
    }

    def input : String = {
        readLine
    }

    def color(f : BigDecimal, b : BigDecimal = BigDecimal.valueOf(0)){
        Console.print(colors(f.intValue()))
    }

    def locate(x : BigDecimal, y : BigDecimal) {
        throw new UnsupportedOperationException
    }

    def inkey : String = {
        throw new UnsupportedOperationException
    }

    def stick(n : BigDecimal) : BigDecimal = {
        throw new UnsupportedOperationException
    }
}
