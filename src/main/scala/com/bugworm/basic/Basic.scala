package com.bugworm.basic

import java.io.{Reader, InputStreamReader, FileInputStream}
import scala.collection.immutable.List
import scala.collection.mutable.HashMap
import scala.util.parsing.combinator.RegexParsers
import com.bugworm.basic.screen.ScreenIO

object Basic extends RegexParsers{

//    def main(args : Array[String]){
//        val reader = new InputStreamReader(new FileInputStream("Sample.basic"), "UTF-8")
//        val lines = parse(reader).get
//        reader.close()
//        val runtime = new BasicRuntime(lines)
//        while(!runtime.terminated){
//            lines.lift(runtime.currentLine).getOrElse(Line.end).execute(runtime)
//            runtime.next()
//        }
//    }

    def parse(data : String) = {
    	parseAll(lines, data)
    }

    def parse(reader : Reader) = {
    	parseAll(lines, reader)
    }

    def lines : Parser[List[Line]] = rep(line)

    def line : Parser[Line] = operations ^^ (new Line(_)) |
            label~operations ^^ {case lab~ope => new Line(ope, lab)} |
            label ^^ (new Line(Operation.nop, _))

    def operations : Parser[Operation] = operation~rep(":"~>operation) ^^ {
        case ope~multi => new Operation{
            def execute(runtime : BasicRuntime){
                ope.execute(runtime)
                for(mope <- multi if runtime.nextLine == -1 && !runtime.terminated)mope.execute(runtime)
            }
        }
    }

    def operation : Parser[Operation] =
        numvar~"="~numexpr ^^ {
            case va~c~ex => new Operation{
                def execute(runtime : BasicRuntime) = runtime.decimalVars.put(va.name, ex(runtime))
            }
        } |
        strvar~"="~strexpr ^^ {
            case va~c~ex => new Operation{
                def execute(runtime : BasicRuntime) = runtime.strVars.put(va.name, ex(runtime))
            }
        } |
        "(?i)GO ?TO".r~>target ^^ (t => new Operation{
            def execute(runtime : BasicRuntime) = runtime.goto(t)
        }) |
        "(?i)PRINT".r~>(boolexpr | strexpr | numexpr)~";?".r ^^ {
            case ex~ln => new Operation{
            	def execute(runtime : BasicRuntime) = runtime.io.printStr(ex(runtime).toString, ln == "")
            }
        } |
        "(?i)INPUT".r~>strvar ^^ (ex => new Operation{
            def execute(runtime : BasicRuntime) = runtime.input(ex)
        }) |
        "(?i)COLOR".r~>numexpr ^^ (c => new Operation{
            def execute(runtime : BasicRuntime) = runtime.io.color(c(runtime))
        }) |
        "(?i)LOCATE".r~>numexpr~","~numexpr ^^ {
            case x~c~y => new Operation{
                def execute(runtime : BasicRuntime) = runtime.io.locate(x(runtime), y(runtime))
            }
        } |
        "(?i)END".r ^^^ new Operation{
            def execute(runtime : BasicRuntime) = runtime.terminated = true
        } |
        "(?i)IF".r~>boolexpr~"(?i)THEN".r~operations~opt("(?i)ELSE".r~operations) ^^ {
            case b~then~ope1~option => new Operation{
                def execute(runtime : BasicRuntime) = {
                    if(b(runtime)){
                        ope1.execute(runtime)
                    }else{
                        option.foreach{
                            case el~ope2 => ope2.execute(runtime)
                        }
                    }
                }
            }
        }

    def label : Parser[String] = "^\\*".r~>"[A-Za-z][A-Za-z0-9_]*".r | "^[1-9][0-9]{0,7}".r

    def target : Parser[String] = "[A-Za-z][A-Za-z0-9_]*".r | "[1-9][0-9]{0,7}".r

    //文字列系
    def strexpr : Parser[Value[String]] = strval~rep("+"~>strval) ^^ {
        case head~tail => new Value[String]{
		    def apply(runtime : BasicRuntime) : String = {
		        (head(runtime) /: tail.map(_(runtime)))(_ + _)
		    }
		}
    }

    def strval : Parser[Value[String]] = strfn | strvar | str

    def strfn : Parser[Function[String]] =
        "(?i)STR\\$\\(".r~>numexpr<~"\\)".r ^^ (n => new Function[String]{
            def apply(runtime : BasicRuntime) = n(runtime).toString
        })

    def strvar : Parser[Var[String]] = "[A-Za-z][A-Za-z0-9_]*\\$".r ^^ (new Var[String](_){
        def apply(runtime : BasicRuntime) = runtime.strVars.getOrElse(name, "");
    })

    def str : Parser[Const[String]] = """"[^\"]*"""".r ^^ (s => new Const[String](s.tail.init))

    //数値系
    def numexpr : Parser[Value[BigDecimal]] = numterm~rep("+"~numterm | "-"~numterm) ^^ {
        case n~nums => {
            new Value[BigDecimal]{
                def apply(runtime : BasicRuntime) = {
                    var v = n(runtime)
                    nums.foreach{
                        case "+"~t => v = v + t(runtime)
                        case "-"~t => v = v - t(runtime)
                    }
                    v
                }
            }
        }
    }

    def numterm : Parser[Value[BigDecimal]] = numval~rep("*"~numval | "/"~numval) ^^ {
        case n~nums => {
            new Value[BigDecimal]{
                def apply(runtime : BasicRuntime) = {
                    var v = n(runtime)
                    nums.foreach{
                        case "*"~t => v = v * t(runtime)
                        case "/"~t => v = v / t(runtime)
                    }
                    v
                }
            }
        }
    }

    def numval : Parser[Value[BigDecimal]] = numfn | numvar | num

    def numfn : Parser[Function[BigDecimal]] =
        "(?i)STICK\\(".r~>numexpr<~"\\)".r ^^ (n => new Function[BigDecimal]{
            def apply(runtime : BasicRuntime) = runtime.io.stick(n(runtime))
        })

    def numvar : Parser[Var[BigDecimal]] = "[A-Za-z][A-Za-z0-9_]*".r ^^ (new Var[BigDecimal](_){
        def apply(runtime : BasicRuntime) = runtime.decimalVars.getOrElse(name, BigDecimal(0));
    })

    def num : Parser[Const[BigDecimal]] = """(\d+(\.\d*)?|\d*\.\d+)""".r ^^ (n => new Const[BigDecimal](BigDecimal(n)))

    //boolean
    def boolexpr : Parser[Value[Boolean]] = boolterm~rep("OR"~>boolterm) ^^ {
        case b~bools => {
            new Value[Boolean]{
                def apply(runtime : BasicRuntime) = {
                	(b(runtime) /: bools.map(_(runtime)))(_ || _ )
                }
            }
        }
    }

    def boolterm : Parser[Value[Boolean]] = boolval~rep("AND"~>boolval) ^^ {
        case b~bools => {
            new Value[Boolean]{
                def apply(runtime : BasicRuntime) = {
                	(b(runtime) /: bools.map(_(runtime)))(_ && _ )
                }
            }
        }
    }

    def boolval : Parser[Value[Boolean]] = boolfn | boolvar | bool

    def boolfn : Parser[Function[Boolean]] =
        numexpr~"==|<>|><|!=|<=|=<|>=|=>|>|<|=".r~numexpr ^^ {
        	case num1~cp~num2 => new Function[Boolean]{
        		def apply(runtime : BasicRuntime) = {
        			cp match {
                    	case "=" | "==" => num1(runtime) == num2(runtime)
	                    case "<" => num1(runtime) < num2(runtime)
	                    case "<=" | "=<" => num1(runtime) <= num2(runtime)
	                    case ">" => num1(runtime) > num2(runtime)
	                    case ">=" | "=>" => num1(runtime) >= num2(runtime)
	                    case "<>" | "><" | "!=" => num1(runtime) != num2(runtime)
	                }
	            }
	        }
    	} |
    	strexpr~"=|==|<>|><|!=".r~strexpr ^^ {
    	    case str1~cp~str2 => new Function[Boolean]{
    	        def apply(runtime : BasicRuntime) = {
    	            cp match {
    	                case "=" | "==" => str1(runtime) == str2(runtime)
    	                case "<>" | "><" | "!=" => str1(runtime) != str2(runtime)
    	            }
    	        }
    	    }
    	}

    def boolvar : Parser[Var[Boolean]] = "[A-Za-z][A-Za-z0-9_]*\\?".r ^^ (new Var[Boolean](_){
        def apply(runtime : BasicRuntime) = runtime.booleanVars.getOrElse(name, false);
    })

    def bool : Parser[Const[Boolean]] = "(?i)true|false".r ^^ (n => new Const[Boolean](n.equalsIgnoreCase("true")))
}

object Operation{
    val nop = new Operation{
        def execute(runtime : BasicRuntime){}
    }
}

trait Operation{
    def execute(runtime : BasicRuntime) : Unit
}

object Line {
    val end = new Line(Operation.nop){
        override def execute(runtime : BasicRuntime) = {
            runtime.terminated = true
            runtime
        }
    }
}

class Line(val operation : Operation, val label : String = ""){
    def execute(runtime : BasicRuntime) : Unit = {
        operation.execute(runtime)
    }
}

trait Value[+T]{
    def apply(runtime : BasicRuntime) : T
}

trait Function[T] extends Value[T]{
    def apply(runtime : BasicRuntime) : T
}

abstract class Var[T](val name : String) extends Value[T]{
    def apply(runtime : BasicRuntime) : T
}

class Const[T](val v : T)extends Value[T]{
    def apply(runtime : BasicRuntime) = v
}

class BasicRuntime(lines : List[Line]){

    val strVars : HashMap[String, String] = new HashMap[String, String]
    val decimalVars : HashMap[String, BigDecimal] = new HashMap[String, BigDecimal]
    val booleanVars : HashMap[String, Boolean] = new HashMap[String, Boolean]
    val lineMap : HashMap[String, Int] = new HashMap[String, Int]
    var io : BasicIO = new ConsoleIO
    var terminated : Boolean = false
    var currentLine : Int = 0
    var nextLine : Int = -1

    def next() = {
        currentLine = if(nextLine == -1) currentLine + 1 else nextLine
        nextLine = -1
    }
    
    def goto(label : String) = {
        lineMap.get(label) match {
            case Some(index) => nextLine = index
            case None =>
                for(i <- 0 until lines.size if lines(i).label == label){
	                lineMap.put(label, i)
	                nextLine = i
                }
        }
    }

    def input(ex : Var[String]){
        strVars.put(ex.name, io.input)
    }
}
