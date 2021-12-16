package adventofcode.year2021

import cats.effect._
import adventofcode._
import adventofcode.utils._

object Day16 extends IOApp {

  val hexMap = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  case class PacketHeader(version: Int, typeId: Int)

  trait Packet {
    def header: PacketHeader
  }

  case class LiteralPacket(header: PacketHeader, number: Long) extends Packet

  case class OperatorPacket(header: PacketHeader, subPackets: List[Packet]) extends Packet

  case object Packet {
    def decodeHex(hex: String): Packet =
      decodeBinary(hex.map(hexMap).mkString)

    def decodeBinary(binary: String): Packet = {
      val arr = binary.toCharArray()
      var pos = 0

      def read(n: Int): String = {
        val result = arr.slice(pos, pos + n).mkString
        pos += n
        result
      }

      def readPacket(): Packet = {
        val version = read(3).parseBinaryToInt
        val typeId = read(3).parseBinaryToInt
        val header = PacketHeader(version, typeId)
        typeId match {
          case 4 =>
            val start = pos
            var buf = ""
            while (arr(pos) == '1') {
              buf += read(5).takeRight(4)
            }
            buf += read(5).takeRight(4)

            LiteralPacket(header, buf.parseBinaryToLong)
          case _ =>
            val lengthTypeId = read(1).parseBinaryToInt
            lengthTypeId match {
              case 0 =>
                val lengthInBits = read(15).parseBinaryToInt
                val targetPos = pos + lengthInBits
                var subPackets = scala.collection.mutable.ArrayBuffer.empty[Packet]
                while (pos < targetPos) {
                  subPackets.addOne(readPacket())
                }
                OperatorPacket(header, subPackets.toList)
              case _ =>
                val numSubPackets = read(11).parseBinaryToInt
                val subPackets = (1 to numSubPackets).map(_ => readPacket()).toList
                OperatorPacket(header, subPackets)
            }
        }
      }

      readPacket()
    }

    def sumVersion(packet: Packet): Long = {
      def sumVersions(packets: List[Packet]): Long =
        packets.map { p =>
          p match {
            case LiteralPacket(_, _)           => p.header.version
            case OperatorPacket(_, subPackets) => p.header.version + sumVersions(subPackets)
          }
        }.sum

      sumVersions(List(packet))
    }

    def evaluate(packet: Packet): Long = {
      packet match {
        case LiteralPacket(_, value) =>
          value
        case OperatorPacket(PacketHeader(_, typeId), subPackets) =>
          val scores = subPackets.map(evaluate)
          typeId match {
            case 0 => scores.sum
            case 1 => scores.product
            case 2 => scores.min
            case 3 => scores.max
            case 5 => if (scores(0) > scores(1)) 1 else 0
            case 6 => if (scores(0) < scores(1)) 1 else 0
            case 7 => if (scores(0) == scores(1)) 1 else 0
          }
      }
    }
  }

  case class Input(hexPacket: String)

  case object Input {
    def from(input: List[String]): Input = {
      Input(input.head)
    }
  }

  case class Context(aggregate: Packet => Long)

  object Runner extends Day[Input, Context, Long](2021, 16) {
    override def transformInput(lines: List[String]): Input =
      Input.from(lines)

    override def partOneContext(): Option[Context] =
      Some(Context(Packet.sumVersion))

    override def partTwoContext(): Option[Context] =
      Some(Context(Packet.evaluate))

    override def process(input: Input, context: Option[Context]): Option[Long] =
      context.map(_.aggregate(Packet.decodeHex(input.hexPacket)))
  }

  override def run(args: List[String]): IO[ExitCode] = Runner.run(args)
}
