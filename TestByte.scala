package zi

import java.math.BigInteger

object TestByte extends App {
  val rounds = 28;

  val str = "hello"
  val encode = new StringBuilder
  val decode = new StringBuilder
  var temp = 0

  for (i <- 0 to str.length() - 1 by 2) {
    try {
      val k = feist(Array(str(i), str(i + 1)), false)
      encode.append(k(0).toChar + "" + k(1).toChar)
    } catch {
      case t: StringIndexOutOfBoundsException => {
        val k = feist(Array(str(i), str(i)), false)
        encode.append(k(0).toChar)
        temp = k(1)
      }
    }
  }
  for (i <- 0 to encode.toString().length() - 1 by 2) {
    try {
      val k = feist(Array(encode(i), encode(i + 1)), true)
      decode.append(k(0).toChar + "" + k(1).toChar)
    } catch {
      case t: StringIndexOutOfBoundsException => {
        val k = feist(Array(encode(i), temp), true)
        decode.append(k(0).toChar)
      }
    }
  }

  println("Original: " + str)
  println("Encode: " + encode.toString())
  println("Decode: " + decode.toString())

  def feist(a: Array[Int], reverse: Boolean): Array[Int] = {
    var round = if (reverse) rounds else 1;
    var l = a(0);
    var r = a(1);
    for (i <- 0 to rounds - 1) {
      if (i < rounds - 1) {
        val t = l;
        l = r ^ f(l, round);
        r = t;
      } else {
        r = r ^ f(l, round);
      }
      if (reverse) round += -1 else round += 1;
    }
    Array(l, r);
  }

  def f(b: Int, k: Int) = b + k;
}