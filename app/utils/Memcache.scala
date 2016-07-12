package utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip.{DeflaterOutputStream, InflaterInputStream}
import javax.inject.{Inject, Singleton}

import play.api.Configuration
import play.api.inject.ApplicationLifecycle
import play.api.libs.json.Json
import shade.memcached.{AuthConfiguration, Codec, Memcached}

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.Try
import scala.xml.{Elem, XML}

@Singleton
class Memcache @Inject() (configuration: Configuration, lifecycle: ApplicationLifecycle) (implicit ec: ExecutionContext) {

  lazy val instance = {
    val maybeAuthConfig = for {
      username <- configuration.getString("memcached.username")
      password <- configuration.getString("memcached.password")
    } yield AuthConfiguration(username, password)

    Memcached(shade.memcached.Configuration(configuration.getString("memcached.servers").get, maybeAuthConfig))
  }

  lifecycle.addStopHook(() => Future.fromTry(Try(instance.close())))

}


object Memcache {

  // from: http://stackoverflow.com/questions/15079332/round-tripping-through-deflater-in-scala-fails

  def compress(bytes: Array[Byte]): Array[Byte] = {
    val deflater = new java.util.zip.Deflater
    val baos = new ByteArrayOutputStream
    val dos = new DeflaterOutputStream(baos, deflater)
    dos.write(bytes)
    dos.finish()
    dos.close()
    baos.close()
    deflater.end()
    baos.toByteArray
  }

  def decompress(bytes: Array[Byte]): Array[Byte] = {
    val inflater = new java.util.zip.Inflater()
    val bytesIn = new ByteArrayInputStream(bytes)
    val in = new InflaterInputStream(bytesIn, inflater)
    val out = Source.fromInputStream(in).map(_.toByte).toArray
    in.close()
    bytesIn.close()
    inflater.end()
    out
  }

  implicit object StringsCodec extends Codec[List[String]] {
    def serialize(fileList: List[String]): Array[Byte] = compress(Json.toJson(fileList).toString().getBytes)
    def deserialize(data: Array[Byte]): List[String] = Json.parse(decompress(data)).as[List[String]]
  }

  implicit object ElemCode extends Codec[Elem] {
    def serialize(elem: Elem): Array[Byte] = compress(elem.toString().getBytes)
    def deserialize(data: Array[Byte]): Elem = XML.loadString(new String(decompress(data)))
  }

}
