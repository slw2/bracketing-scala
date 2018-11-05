/** Import is for readLine so that we can write input directly to the program */

import scala.io.StdIn

object Brack {
  //Maximum length of word so we can define our arrays in dynamic programming
  val MAXWORD = 60

  //Operation to take 'A', 'B' and 'C' to corresponding Ints
  def LetterToInt(a: Char): Int = {
    if (a == 'A' || a == 'B' || a == 'C') {
      return (a.toInt - 'A'.toInt);
    } else {
      println("Please only Letters from A,B,C.")
      sys.exit
    }
  }

  //Defining the op array for everything to use
  val op = Array.ofDim[Int](3, 3)
  op(0)(0) = 1;
  op(0)(1) = 1;
  op(0)(2) = 0
  op(1)(0) = 2;
  op(1)(1) = 1;
  op(1)(2) = 0
  op(2)(0) = 0;
  op(2)(1) = 2;
  op(2)(2) = 2

  /** Read file into array (discarding the EOF character) */
  def readFile(fname: String): Array[Char] =
    scala.io.Source.fromFile(fname).toArray.init


  /* Functions below here need to be implemented */


  //TASK 1
  //PossibleRec checks whether bracketing to something is possible recursively
  //Checks whether w(i,j) can be bracketed to z

  def PossibleRec(w: Array[Int], i: Int, j: Int, z: Int): Boolean = {
    var poss = false
    if (j - i == 1) poss = (w(i) == z)
    else {
      for (k <- i + 1 until j)
        for (a <- 0 to 2)
          for (b <- 0 to 2)
            if (PossibleRec(w, i, k, a) && PossibleRec(w, k, j, b) && op(a)(b) == z) poss = true
    }
    poss
  }


  //TASK 2
  //NumberRec which checks the ways you get a result recursively
  //Computes number of ways w(i,j) can be bracketed to get z

  def NumberRec(w: Array[Int], i: Int, j: Int, z: Int): Int = {
    var ways = 0
    if (j - i == 1 && (w(i) == z)) ways = 1
    else {
      for (k <- i + 1 until j)
        for (a <- 0 to 2)
          for (b <- 0 to 2)
            if (op(a)(b) == z) ways = ways + (NumberRec(w, i, k, a) * NumberRec(w, k, j, b))
    }
    ways
  }


  //TASK 3
  //Runtime analysis of recursive solution along with tests

  // PossibleRec: T(n) = 6 x sum(k=1 to n-1)(T(k))
  //                   = T(n-1) + 6 x sum(k=1 to n-2)(T(k))
  //                   = 7 x T(n-1)
  //                   = Big O (7^n)

  // NumberRec: same


  //You may find the following class useful for Task 7
  // Binary tree class
  abstract class BinaryTree

  case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

  case class Leaf(value: Char) extends BinaryTree

  //Printing for a binary tree
  def print_tree(t: BinaryTree) {
    //TODO(optional)
  }

  //These arrays should hold the relevant data for dynamic programming
  var poss = Array.ofDim[Boolean](MAXWORD, MAXWORD, 3)
  var ways = Array.ofDim[Int](MAXWORD, MAXWORD, 3)
  var exp = Array.ofDim[BinaryTree](MAXWORD, MAXWORD, 3)


  //Task 4, 5, and 7(optional)
  //Fill out arrays with dynamic programming solution

  def Tabulate(w: Array[Int], n: Int): Unit = {
    for (i <- 0 to n - 1) {
      for (z <- 0 to 2) {
        //poss(i)(i+1)(z) = (w(i) == z)
        ways(i)(i + 1)(z) = if (w(i) == z) 1; else 0
      }
    }
    for (k <- 2 to n) {
      for (i <- 0 to n - k) {
        val j = i + k
        for (l <- 1 to k) {
          for (z <- 0 to 2) {
            for (a <- 0 to 2) {
              for (b <- 0 to 2) {
                //if (poss(i)(i+l)(a) && poss(i+l)(j)(b) && op(a)(b) == z) poss(i)(j)(z) = true
                //else poss(i)(j)(z) = false
                if (op(a)(b) == z) ways(i)(j)(z) += (ways(i)(i + l)(a) * ways(i + l)(j)(b))
              }
            }
          }
        }
      }
    }
  }


  //Task 6
  //Runtime analysis of dynamic programming version with tests

  //When you can only achieve a single letter from a string, and this is maximal (i.e. you 
  //can acheive it a maximal number of times), then the maximum length of this string is 20

  // Running time = O(n^3)
  // There are O(n^2) spaces in the table to fill, and it takes time O(n) to fill each one 
  // because you need to look at all the possible splittings of the subword.


  /** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {

    // string to print if error occurs
    val errString =
      "Usage: scala Brack -PossibleRec [file]\n" +
        "     | scala Brack -NumberRec [file]\n" +
        "     | scala Brack -Tabulate [file]\n"

    if (args.length > 2) {
      println(errString)
      sys.exit
    }

    //Get the plaintext, either from the file whose name appears in position
    //pos, or from standard input
    def getPlain(pos: Int) =
      if (args.length == pos + 1) readFile(args(pos)) else StdIn.readLine.toArray

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if (args.length < n) {
      println(errString); sys.exit
    }

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
    val plain = getPlain(1)
    val command = args(0)

    //Making sure the letters are of the right type
    val len = plain.length
    var plainInt = new Array[Int](len)
    if (len > MAXWORD) {
      println("Word Too Long! Change MAXWORD")
      sys.exit;
    } else {
      for (i <- 0 until len) {
        plainInt(i) = LetterToInt(plain(i))
      }
    }

    //Executing appropriate command
    if (command == "-PossibleRec") {
      println("Bracketing values for " + plain.mkString(""))
      for (i <- 0 to 2) {
        if (PossibleRec(plainInt, 0, len, i)) {
          println(('A'.toInt + i).toChar + " is Possible");
        }
        else {
          println(('A'.toInt + i).toChar + " is not Possible");
        }
      }
    }
    else if (command == "-NumberRec") {
      var z: Int = 0
      println("Bracketing values for " + plain.mkString(""))
      for (i <- 0 to 2) {
        z = NumberRec(plainInt, 0, len, i)
        if (z == 1) {
          printf(('A'.toInt + i).toChar + " can be achieved in %d way\n", z)
        }
        else {
          printf(('A'.toInt + i).toChar + " can be achieved in %d ways\n", z)
        }
      }
    }

    else if (command == "-Tabulate") {
      Tabulate(plainInt, len)
      println("Bracketing values for " + plain.mkString(""))
      for (v <- 0 to 2) {
        var z: Int = ways(0)(len)(v)
        print(z)
        if (z == 0) {
          println(('A'.toInt + v).toChar + " cannot be achieved")
        }
        else if (z == 1) {
          printf(('A'.toInt + v).toChar + " can be achieved %d way\n", z)
          printf("For example:")
          print_tree(exp(0)(len)(v))
          printf("\n")
        }
        else if (z > 1) {
          printf(('A'.toInt + v).toChar + " can be achieved %d ways\n", z)
          printf("For example:")
          print_tree(exp(0)(len)(v))
          printf("\n")
        }
      }
    }
    else println(errString)
  }
}


