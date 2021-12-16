// using scala 2.13
// using lib org.typelevel::cats-effect:3.3.0
// using lib com.lihaoyi::os-lib:0.7.8
// using lib commons-codec:commons-codec:1.15

package adventofcode

package object utils {
  implicit class ListRotator[A](list: List[List[A]]) {
    def rotate90(): List[List[A]] = list.transpose.map(_.reverse)
    def rotate180(): List[List[A]] = rotate90().rotate90()
    def rotate270(): List[List[A]] = list.map(_.reverse).transpose
  }

  implicit class StringListRotator(list: List[String]) {
    def rotate90(): List[String] = list.map(_.toList).rotate90().map(_.mkString)
    def rotate180(): List[String] = list.map(_.toList).rotate180().map(_.mkString)
    def rotate270(): List[String] = list.map(_.toList).rotate270().map(_.mkString)
  }

  implicit class BinaryStringToInt(string: String) {
    def parseBinaryToInt(): Int = Integer.parseInt(string, 2)
  }

  implicit class BinaryStringToLong(string: String) {
    def parseBinaryToLong(): Long = java.lang.Long.parseLong(string, 2)
  }

  implicit class StringToMD5Hex(string: String) {
    def toMd5Hex: String = org.apache.commons.codec.digest.DigestUtils
      .md5Hex(string)
      .toLowerCase()
  }

  implicit class SeqFrequency[A](seq: Seq[A]) {
    def frequency(): Map[A, Long] =
      seq
        .groupBy(identity)
        .view
        .mapValues(_.size.toLong)
        .toMap
  }
}
