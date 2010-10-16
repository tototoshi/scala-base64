//http://ja.wikipedia.org/wiki/Base64

package tototoshi.base64

object Base64 {
  val encodeTable = List('A', 'B', 'C', 'D', 'E', 'F', 'G',
			 'H', 'I', 'J', 'K', 'L', 'M', 'N',
			 'O', 'P', 'Q', 'R', 'S', 'T', 'U',
			 'V', 'W', 'X', 'Y', 'Z',
			 'a', 'b', 'c', 'd', 'e', 'f', 'g',
			 'h', 'i', 'j', 'k', 'l', 'm', 'n',
			 'o', 'p', 'q', 'r', 's', 't', 'u',
			 'v', 'w', 'x', 'y', 'z',
			 '0', '1', '2', '3', '4', '5', '6',
			 '7', '8', '9',
			 '+', '/');

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
    takeEach(BIT_LENGTH, src)
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
    .toList
    .map(x => encodeTable.indexOf(x))
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

    takeEach(BIT_LENGTH, binaryStringArray)
    .map(x => binaryToDecimal(x).toChar)
    .mkString
  }

  def deleteExtraZero(s: String): String = {
    val BIT_LENGTH = 8
    val len = s.length
    s.slice(0, (len / BIT_LENGTH)  * BIT_LENGTH)
  }

  def takeEach(i: Int, src: String): List[String] = {
    val BIT_LENGTH = i
    var store :List[String] = List()
    var offset = 0
    def limit = offset + BIT_LENGTH

    while(limit < src.size){
      store = src.slice(offset, limit) :: store
      offset += BIT_LENGTH
    }

    var tail = src.slice(offset, limit)
    tail = tail.length match {
      case BIT_LENGTH => tail
      case len if (len < BIT_LENGTH) => tail + "0" * (BIT_LENGTH - tail.size)
    }

    store = tail :: store
    store.reverse
  }

}
