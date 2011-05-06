package com.tototoshi.base64

import scala.annotation.tailrec

object Base64 {
  val encodeTable = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  def encode(fromBytes: Array[Byte]) : String = {
    val encoded = {
      group6Bits(fromBytes)
      .map(x => encodeChar(binaryToDecimal(x.toArray)))
      .mkString
    }
    encoded + "=" * ((4 - encoded.length % 4) % 4) grouped(76) mkString "\n"
  }

  def encodeChar(i: Int) :Char = encodeTable(i)

  def binaryStringToDecimal(src: String) :Int = Integer.parseInt(src, 2)

  def binaryToDecimal(ba: Array[Int]): Int = {
    val len = ba.length
    var sum = 0
    var i = 0
    while (i < len) {
      sum += ba(len - i - 1) * math.pow(2, i).toInt
      i += 1
    }
    sum
  }

  def group6Bits(fromBytes: Array[Byte]) :List[List[Int]] = {
    val BIT_LENGTH = 6
    val src = toBinaryArray(8)(fromBytes)
    trimList[Int](src.toList.grouped(BIT_LENGTH).toList, BIT_LENGTH, 0)
  }

  def toBinaryArray(bitLength: Int)(from: Array[Byte]): Array[Int] = {
    val ba = new Array[Int](bitLength * from.length)
    var i = 0
    while (i < bitLength * from.length) {
      ba((i / bitLength) * bitLength + bitLength - (i % 8) - 1) = from(i / bitLength) >> (i % bitLength) & 1
      i += 1
    }
    ba
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
    "0" * (BIT_LENGTH - result.length) + result
  }

  def decode(src: String) :String = {
    val BIT_LENGTH = 8

    val indexArray = {
      getEncodeTableIndexList(src)
      .map(x => convertIntTo6bitString(x))
    }
    val binaryStringArray: String = deleteExtraZero(indexArray.mkString)

    binaryStringArray
    .grouped(BIT_LENGTH)
    .map(x => x + "0" * (6 - x.length))
    .map(binaryStringToDecimal(_).toChar)
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
