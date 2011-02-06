package jp.ddo.ttoshi.base64

import scala.annotation.tailrec

object Base64 {
  val encodeTable = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  def encode(fromBytes: Array[Byte]) : String = encode(fromBytes.toList)

  def encode(fromBytes: List[Byte]) :String = {
    val encoded = {
      get6BitStrList(fromBytes)
      .map(binaryToDecimal(_))
      .map(encodeChar(_))
      .mkString
    }

    encoded.length % 4 match {
      case 0 => encoded
      case x => encoded + "=" * (4 - x)
    }
  }

  def encodeChar(i: Int) :Char = encodeTable(i)

  def binaryToDecimal(src: String) :Int = Integer.parseInt(src, 2)

  def get6BitStrList(fromBytes: List[Byte]) :List[String] = {
    val BIT_LENGTH = 6
    val src = toBinaryString(fromBytes)
    trimList[Char](src.toList.sliding(BIT_LENGTH, BIT_LENGTH).toList, BIT_LENGTH, '0')
    .map(_.mkString)
  }

  def toBinaryString(fromBytes: List[Byte]) :String = {
    val BIT_LENGTH = 8
    val MASK = binaryToDecimal("11111111")

    fromBytes
    .map(x => (x & MASK).toBinaryString)
    .map(s => s.length match {
      case BIT_LENGTH => s
      case len if (len > BIT_LENGTH) => s.slice(len - BIT_LENGTH, len)
      case len if (len < BIT_LENGTH) => ("0" * (BIT_LENGTH - len)) + s
    })
    .mkString
  }

  def deleteEqual(src: String) :String = src.filter(_ != '=')

  def getEncodeTableIndexList(s: String): List[Int]= {
    deleteEqual(s)
    .map(x => encodeTable.indexOf(x))
    .toList
  }

  def convertIntTo6bitString(i: Int) :String = {
    val BIT_LENGTH = 6
    val result = i.toBinaryString
    result.length match {
      case BIT_LENGTH => result
      case len if (len < BIT_LENGTH) => ("0" * (BIT_LENGTH - len)) + result
    }
  }

  def decode(src: String) :String = {
    val BIT_LENGTH = 8

    val indexArray = {
      getEncodeTableIndexList(src)
      .map(x => convertIntTo6bitString(x))
    }
    val binaryStringArray: String = deleteExtraZero(indexArray.mkString)

    binaryStringArray.toList
    .sliding(BIT_LENGTH, BIT_LENGTH).toList.map(_.mkString)
    .map(x =>
      x.length match {
        case 6 => x
        case l => x + "0" * (6 - l)
      })
    .map(x => binaryToDecimal(x).toChar)
    .mkString
  }

  def deleteExtraZero(s: String): String = {
    val BIT_LENGTH = 8
    val len = s.length
    s.slice(0, (len / BIT_LENGTH)  * BIT_LENGTH)
  }

  def trim[A](xs: List[A], n: Int, c: A): List[A] = {
    xs.length match {
      case l if l == n => xs
      case l if l < n  => xs ::: List.fill(n - l)(c)
      case l if l > n  => xs.take(n)
    }
  }

  def trimList[A](xss: List[List[A]], n: Int, c: A) :List[List[A]] = xss.map(xs => trim[A](xs, n, c))
}

