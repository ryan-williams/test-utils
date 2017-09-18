package org.hammerlab.test.serde

import java.io._

import org.hammerlab.paths.Path

/**
 * Helpers for Java serde in tests.
 */
object JavaSerialization {
  def javaRead[T](bytes: Array[Byte]): T = {
    javaRead(new ByteArrayInputStream(bytes))
  }

  def javaRead[T](is: InputStream): T = {
    val ois = new ObjectInputStream(is)
    try {
      ois.readObject().asInstanceOf[T]
    } finally {
      ois.close()
    }
  }

  def javaRead[T](path: Path): T = {
    javaRead(path.inputStream)
  }

  def javaWrite(o: Object, path: Path): Unit = {
    javaWrite(o, path.outputStream)
  }

  def javaWrite(o: Object, os: OutputStream): Unit = {
    val oos = new ObjectOutputStream(os)
    oos.writeObject(o)
    oos.close()
  }

  def javaBytes(o: Object): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    javaWrite(o, baos)
    baos.toByteArray
  }
}
