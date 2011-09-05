package com.github.tototoshi.test.base64

import com.github.tototoshi.base64.Base64
import org.scalatest.FunSuite
import org.apache.commons.io.FileUtils
import java.io.File

class testBase64 extends FunSuite{
  test("EncodeTable"){
    assert(Base64.encodeChar(0) == 'A')
    assert(Base64.encodeChar(53) == '1')
  }

  test("toBinarySeq") {
    assert(Base64.toBinarySeq(8)("ABCDEFG" getBytes).toList.mkString == "01000001010000100100001101000100010001010100011001000111")
  }

  test("binaryToDecimal") {
    val binary = List(1, 1, 1, 1).toArray
    val decimal = 15
    assert(Base64.binaryToDecimal(binary) === decimal)
  }

  test("Encode"){
    assert(Base64.encode("ABCDEFG".getBytes) == "QUJDREVGRw==")
    assert(Base64.encode("hogepiyofoobar".getBytes) == "aG9nZXBpeW9mb29iYXI=")
    assert(Base64.encode("ABCDEFG".getBytes) == "QUJDREVGRw==")
    assert(Base64.encode("hogepiyofoobar".getBytes) == "aG9nZXBpeW9mb29iYXI=")
    assert(Base64.encode("homuho".getBytes) == "aG9tdWhv")

    val pixel: Array[Byte] = FileUtils.readFileToByteArray(new File(getClass.getResource("/pixel.gif").getPath))
    assert(Base64.encode(pixel) == "R0lGODlhAQABAIAAAAAAAAAAACH5BAEAAAAALAAAAAABAAEAAAICRAEAOw==")

    val scalachan: Array[Byte] = FileUtils.readFileToByteArray(new File(getClass.getResource("/scala-chan.jpg").getPath))
    val scalachanExpected: String = FileUtils.readFileToString(new File(getClass.getResource("/scala-chan.expected").getPath))
    assert(Base64.encode(scalachan) == scalachanExpected.trim)
  }

  test("deleteEqual"){
    val src = "abcdefg==="
    val expected = "abcdefg"
    assert(Base64.deleteEqual(src) == expected)
  }

  test("getEncodeTableIndexList"){
    val src = "QUJD"
    val srcWithEqual = "QUJD=="
    val expected = 16 :: 20 :: 9 :: 3 :: Nil
    assert(Base64.getEncodeTableIndexList(src) == expected)
    assert(Base64.getEncodeTableIndexList(srcWithEqual) == expected)
  }

  test("decode"){
    val src = "QUJDREVGRw=="
    val src2 = "aG9nZXBpeW9mb29iYXI="
    val expected = "ABCDEFG".getBytes
    val expected2 = "hogepiyofoobar".getBytes
    assert(Base64.decode(src2).toList == expected2.toList)
    assert(Base64.decode(src).toList == expected.toList)

    val scalachan: Array[Byte] = FileUtils.readFileToByteArray(new File(getClass.getResource("/scala-chan.jpg").getPath))
    val scalachan64: String = FileUtils.readFileToString(new File(getClass.getResource("/scala-chan.expected").getPath))
    assert(Base64.decode(scalachan64.trim).toList == scalachan.toList)
  }

  test("trimList") {
    val a = List(List(1, 2, 3), List(1, 2, 3, 4), List(1, 2))
    assert(List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 0)) == Base64.trimList[Int](a, 3, 0))
  }

  test("trim") {
    val a = List(1, 2, 3)
    val b = List(1, 2, 3, 4)
    val c = List(1, 2)
    assert(List(1, 2, 3) == Base64.trim[Int](a, 3, 0))
    assert(List(1, 2, 3) == Base64.trim[Int](b, 3, 0))
    assert(List(1, 2, 0) == Base64.trim[Int](c, 3, 0))
  }

}
