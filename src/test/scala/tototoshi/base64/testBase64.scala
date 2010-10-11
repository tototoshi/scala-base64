package tototoshi.test.base64

import tototoshi.base64._
import org.scalatest.FunSuite

class testBase64 extends FunSuite{
  test("toBinaryString"){
    val expected = {
      "01000001" +
      "01000010" +
      "01000011" +
      "01000100" +
      "01000101" +
      "01000110" +
      "01000111"
    }

    assert(Base64.toBinaryString("ABCDEFG".getBytes().toList) == expected)
  }

  test("EncodeTable"){
    assert(Base64.encodeChar(0) == 'A')
    assert(Base64.encodeChar(53) == '1')
  }

  test("binaryToDecimal"){
    assert(Base64.binaryToDecimal("110101") == 53)
  }

  test("get6BitstrList"){
    val expected = List("010000", "010100", "001001", "000011", "010001",
			"000100", "010101", "000110", "010001", "110000")
    assert(Base64.get6BitStrList("ABCDEFG".getBytes().toList) == expected)
  }

  test("concatAll"){
    val src = "a" :: "b" :: "c" :: Nil
    val expected = "abc"
    assert(Base64.concatAll(src) == expected)
  }

  test("Encode"){
    assert(Base64.encode("ABCDEFG".getBytes) == "QUJDREVGRw==")
    assert(Base64.encode("hogepiyofoobar".getBytes) == "aG9nZXBpeW9mb29iYXI=")
    assert(Base64.encode("ABCDEFG".getBytes.toList) == "QUJDREVGRw==")
    assert(Base64.encode("hogepiyofoobar".getBytes.toList) == "aG9nZXBpeW9mb29iYXI=")
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

  test("convertIntTo6bitString"){
    val src = 43
    val expected = "101011"
    assert(Base64.convertIntTo6bitString(src) == expected)
  }

  test("decode"){
    val src = "QUJDREVGRw=="
    val src2 = "aG9nZXBpeW9mb29iYXI="
    val expected = "ABCDEFG"
    val expected2 = "hogepiyofoobar"
    assert(Base64.decode(src) == expected)
    assert(Base64.decode(src2) == expected2)
  }


}
