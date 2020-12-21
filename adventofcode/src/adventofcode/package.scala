package adventofcode

package object utils {
  implicit class ListRotator[A](list: List[List[A]]) {
    def rotate90(): List[List[A]] = list.transpose.map(_.reverse)
    def rotate180(): List[List[A]] = rotate90().rotate90()
    def rotate270(): List[List[A]] = list.map(_.reverse).transpose
  }
}