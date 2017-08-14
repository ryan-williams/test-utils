package org.hammerlab.test.serde

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }

import org.hammerlab.test.Suite
import JavaSerialization._

class JavaSerializationTest
  extends Suite {
  test("round trips") {
    javaRead[String](javaBytes("abc")) should be("abc")

    val baos = new ByteArrayOutputStream()
    javaWrite("abc", baos)
    val bais = new ByteArrayInputStream(baos.toByteArray)
    javaRead[String](bais) should be("abc")

    val path = tmpPath()
    javaWrite("abc", path)
    javaRead[String](path) should be("abc")
  }
}
