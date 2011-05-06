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
    val ba = Array.fill(bitLength * from.length)(0)
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

  def decode(src: String) :Array[Byte] = {
    val BIT_LENGTH = 8

    val indexArray = {
      getEncodeTableIndexList(src.filterNot(_ == '\n'))
      .map(x => toBinaryArray(6)(Array.fill(1)(x.toByte)))
    }
    val binaryArray: Array[Int] = deleteExtraZero(indexArray.flatMap(s => s).toArray)

    binaryArray
    .grouped(BIT_LENGTH)
    .map(binaryToDecimal(_).toByte).toArray
  }

  def deleteExtraZero(s: Array[Int]): Array[Int] = {
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
