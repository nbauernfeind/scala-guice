/*
 *  Copyright 2010-2014 Benjamin Lings
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.codingwell

import java.lang.annotation.Annotation
import java.lang.reflect.{Type => JType}

import com.google.inject.internal.Annotations
import com.google.inject.util.Types
import com.google.inject.{Key, TypeLiteral}

import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

/**
 * Note: Scala 2.10's reflection is not thread safe. See https://issues.scala-lang.org/browse/SI-6240.
 * All access to reflection via typeOf and the runtime mirror are synchronized.
 */
package object scalaguice {
  private[this] lazy val mirror = synchronized {
    runtimeMirror(getClass.getClassLoader)
  }

  /**
   * Create a [[com.google.inject.TypeLiteral]] from a [[scala.reflect.api.TypeTags#TypeTag]].
   * Subtypes of [[scala.AnyVal]] will be converted to their corresponding
   * Java wrapper classes.
   */
  def typeLiteral[T: TypeTag]: TypeLiteral[T] = synchronized {
    TypeLiteral.get(javaTypeOf[T]).asInstanceOf[TypeLiteral[T]]
  }

  def typeArgs(typ: Type): List[Type] = typ.asInstanceOf[TypeRefApi].args

  def cls[T: TypeTag]: Class[T] = synchronized {
    cls(typeOf[T])
  }

  def cls[T](typ: Type): Class[T] = synchronized {
    val c: Class[_] = if (isArray(typ)) {
      java.lang.reflect.Array.newInstance(cls(typeArgs(typ).head), 0).getClass
    } else {
      mirror.runtimeClass(typ.typeSymbol.asClass)
    }

    c.asInstanceOf[Class[T]]
  }

  private def isArray[T: TypeTag]: Boolean = synchronized {
    isArray(typeOf[T])
  }

  private def isArray(typ: Type): Boolean = synchronized {
    typ <:< typeOf[Array[_]]
  }

  private[scalaguice] def javaTypeOf[T: TypeTag]: JType = synchronized {
    javaTypeOf(typeOf[T])
  }

  private[scalaguice] def javaTypeOf(typ: Type): JType = synchronized {
    def toWrapper(c: JType) = c match {
      case java.lang.Byte.TYPE => classOf[java.lang.Byte]
      case java.lang.Short.TYPE => classOf[java.lang.Short]
      case java.lang.Character.TYPE => classOf[java.lang.Character]
      case java.lang.Integer.TYPE => classOf[java.lang.Integer]
      case java.lang.Long.TYPE => classOf[java.lang.Long]
      case java.lang.Float.TYPE => classOf[java.lang.Float]
      case java.lang.Double.TYPE => classOf[java.lang.Double]
      case java.lang.Boolean.TYPE => classOf[java.lang.Boolean]
      case java.lang.Void.TYPE => classOf[java.lang.Void]
      case cls => cls
    }

    val args = typ.asInstanceOf[TypeRefApi].args

    if (isArray(typ)) return cls(typ)

    import com.google.inject.util.Types
    args match {
      case Nil => toWrapper(cls(typ))
      case _ => cls(typ) match {
        case c: Class[_] if c.getEnclosingClass == null => Types.newParameterizedType(c, args.map(javaTypeOf(_)): _*)
        case c: Class[_] => Types.newParameterizedTypeWithOwner(c.getEnclosingClass, c, args.map(javaTypeOf(_)): _*)
      }
    }
  }

  /**
   * Returns the name the set should use.  This is based on the annotation.
   * If the annotation has an instance and is not a marker annotation,
   * we ask the annotation for its toString.  If it was a marker annotation
   * or just an annotation type, we use the annotation's name. Otherwise,
   * the name is the empty string.
   */
  private[scalaguice] def nameOf[T](key: Key[T]): String = {
    val annotation: Annotation = key.getAnnotation
    val annotationType: Class[_ <: Annotation] = key.getAnnotationType
    if (annotation != null && !Annotations.isMarker(annotationType)) {
      key.getAnnotation.toString
    } else if (key.getAnnotationType != null) {
      "@" + key.getAnnotationType.getName
    } else {
      ""
    }
  }

  /** Helpers to wrap T into some enclosing type that takes a single type parameter. */
  private[scalaguice] type HKClassTag[CC2[_]] = ClassTag[CC2[_]]

  private[scalaguice] class WrapHelper[WType[_] : HKClassTag] {
    def around[T](typ: TypeLiteral[T]): TypeLiteral[WType[T]] = {
      val wType = Types.newParameterizedType(implicitly[HKClassTag[WType]].runtimeClass, typ.getType)
      TypeLiteral.get(wType).asInstanceOf[TypeLiteral[WType[T]]]
    }
  }

  private[scalaguice] def wrap[WType[_] : HKClassTag] = new WrapHelper[WType]

  /** Helpers to wrap K, V into some enclosing type that takes two type parameters. */
  private[scalaguice] type HKClassTag2[CC2[_, _]] = ClassTag[CC2[_, _]]

  private[scalaguice] class WrapHelper2[WType[_, _] : HKClassTag2] {
    def around[K, V](kTyp: TypeLiteral[K], vTyp: TypeLiteral[V]): TypeLiteral[WType[K, V]] = {
      val wType = Types.newParameterizedType(
        implicitly[HKClassTag2[WType]].runtimeClass,
        kTyp.getType,
        vTyp.getType
      )

      TypeLiteral.get(wType).asInstanceOf[TypeLiteral[WType[K, V]]]
    }
  }

  private[scalaguice] def wrap2[WType[_, _] : HKClassTag2] = new WrapHelper2[WType]
}
