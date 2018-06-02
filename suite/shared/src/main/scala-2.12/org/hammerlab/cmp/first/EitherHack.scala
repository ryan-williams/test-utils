package org.hammerlab.cmp.first

/**
 * This is used in Scala 2.11 to work around [[Either]] not extending [[Product]]/[[Serializable]], cf.
 *
 * See [[CaseClass]] or the Scala-2.11 version for more info.
 */
trait EitherHack
