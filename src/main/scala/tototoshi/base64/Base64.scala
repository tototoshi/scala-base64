package tototoshi.base64

object Base64 {
  val encodeTable = List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/');

  def encode(fromBytes: List[Byte]) :String = {
    val encoded = {
      get6BitStrList(fromBytes)
      .map(nishin(_))
      .map(encodeChar(_))
      .foldLeft(""){(x,y) => x + y}
    }
    encoded.length % 4 match {
      case 0 => encoded
      case x => encoded + "=" * (4 - x)
    }
  }

  def encodeChar(i: Int) :Char = encodeTable(i)

  def nishin(src: String) :Int = Integer.parseInt(src, 2)

  def get6BitStrList(fromBytes: List[Byte]) :List[String] = {
    val src = toBinaryString(fromBytes)
    var store :List[String] = List()
    var offset = 0
    def limit = offset + 6

    while(limit < src.length()){
      store = src.slice(offset, limit) :: store
      offset += 6
    }

    var tail = src.slice(offset, limit)
    tail = tail.length match {
      case 6 => tail
      case len if (len < 6) => tail + "0" * (6- tail.length())
    }

    store = tail :: store
    store.reverse
  }

  def toBinaryString(fromBytes: List[Byte]) :String= {
    get8bitStrList(fromBytes)
    .foldLeft(""){(x,y) => x+y}
  }

  def get8bitStrList(fromBytes: List[Byte]) :List[String]= {
    fromBytes
    .map(x => (x & 255).toBinaryString)
    .map(s => s.length match {
      case 8 => s
      case len if (len > 8) => s.slice(len - 8, len)
      case len if (len < 8) => ("0" * (8 - len)) + s
    })
  }

}
