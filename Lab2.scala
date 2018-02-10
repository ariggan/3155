package jsy.student

import javax.naming.NameNotFoundException

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * <Your Name>
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(b) => if (b==true) 1.0 else 0.0
      case S(str) => str.toDouble //if string is not a double value, then return undefined or something
      case Undefined => ??? //NaN
//      case _ => ???
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case S(str) => if(str=="") false else true
      case N(n)=> if (n==0) false else true
      case Undefined => false
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case N(n) => n.toString()
      case B(b) => if (b==true) b.toString else ""
      case _ => ???
//        undefined throws exception
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case N(n) => e
      case B(b) => e
      case S(str) => e
      case Var(x) => lookup(env, x)
      case Undefined => Undefined
      /* Inductive Cases */
      case Binary(bop, e1, e2) =>
        bop match {
          case Minus => N(toNumber(eval(env, e1))-toNumber(eval(env, e2)))
          case Plus => {
            if(e1.isInstanceOf[S] || e2.isInstanceOf[S]) S(toStr(eval(env, e1))+toStr(eval(env, e2)))
            else N(toNumber(eval(env, e1))+toNumber(eval(env, e2)))
          }
          case Times => N(toNumber(eval(env,e1))*toNumber(eval(env,e2)))
          case Div => {
            if(e2==0){
              if(toNumber(e1)>0) S("Infinity")
              else if(toNumber(e1)<0) S("-Infinity")
              else N(0)
            }
            else N(toNumber(eval(env,e1))/toNumber(eval(env,e2)))
          } //no divide by zero

          case And => {
            val eOne = eval(env,e1)
            val eTwo = eval(env, e2)
            val firstOrSecondVal = B(toBoolean(eval(env,e1))&&toBoolean(eval(env,e2)))
            if (firstOrSecondVal==B(true)) eTwo
            else if(eOne!=eTwo) if (eOne == B(true)) eTwo else eOne
            else eOne
//            if (eOne.isInstanceOf[N] || eTwo.isInstanceOf[N]) N(toNumber(B(toBoolean(eval(env,e1))&&toBoolean(eval(env,e2)))))
//            else if(eOne.isInstanceOf[S] || eTwo.isInstanceOf[S]) S(toStr(B(toBoolean(eval(env,e1))&&toBoolean(eval(env,e2)))))
//            else B(toBoolean(eval(env,e1))&&toBoolean(eval(env,e2)))
          }
          case Or => {
            val eOne = eval(env,e1)
            val eTwo = eval(env, e2)
            if (toBoolean(eOne) || toBoolean(eTwo)) {
              eOne match{
                case N(n) => if (n>0) eOne else eTwo
                case S(str) => if(str!="") eOne else eTwo
                case B(b) => if(b==true) eOne else eTwo
              }
            }
            else if(eOne==Undefined){
              if(eTwo==Undefined) Undefined
              else eTwo
            }
            else eTwo
          }

          case Eq => {
            val eOne = eval(env,e1)
            val eTwo = eval(env, e2)

            if(eOne == Undefined && eTwo == Undefined) B(true)
            else if(eOne == Undefined || eTwo==Undefined) B(false)
            else B(toNumber(eval(env,e1))==toNumber(eval(env,e2)))
          }
          case Ne => B(toNumber(eval(env,e1))!= toNumber(eval(env,e2)))
          case Lt => B(toNumber(eval(env,e1))<toNumber(eval(env,e2)))
          case Le => B(toNumber(eval(env,e1))<=toNumber(eval(env,e2)))
          case Gt => B(toNumber(eval(env,e1))>toNumber(eval(env,e2)))
          case Ge => B(toNumber(eval(env,e1))>=toNumber(eval(env,e2)))

          case Seq => eval(env,e1); eval(env,e2)

        }
      case Unary(uop,e1)=>
        uop match {
          case Neg => N(0-toNumber(eval(env, e1)))
          case Not => B(!(toBoolean(eval(env, e1))))
        }
      case ConstDecl(x, e1, e2) => {
        val tempEnv = extend(env, x, e1)
//        println(env)
        eval(tempEnv, e2)
      }
      case If(e1, e2, e3)=>{
        if (toBoolean(eval(env, e1))==true) eval(env, e2)
        else eval(env, e3)
      }
      case Print(e1) => println(pretty(eval(env, e1))); Undefined

      case _ => ???
    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}
