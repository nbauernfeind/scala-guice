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
package net.codingwell.scalaguice

import com.google.inject.Binder
import com.google.inject.binder._
import com.google.inject.name.Names
import java.lang.annotation.{Annotation => JAnnotation}
import javax.inject.Provider
import scala.reflect.runtime.universe._

/**
 * Extensions for Guice's binding DSL.
 * These allow using a type parameter instead of <code>classOf[Foo]}</code>
 * or <code>new TypeLiteral[Bar[Foo]] {}</code>. The extra methods are
 * named as those in the normal binding DSL suffixed with <code>Type</code>.
 *
 * For example, instead of
 * {{{
 * binder.bind(new TypeLiteral[Bar[Foo]]{}).to(classOf[FooBarImpl])
 * }}}
 * use
 * {{{
 * import BindingExtensions._
 * binder.bindType[Bar[Foo]].toType[FooImpl]
 * }}}
 *
 * '''Note''' This syntax allows binding to and from generic types.
 * It doesn't currently allow bindings between wildcard types because the
 * TypeTags for wildcard types don't provide access to type bounds.
 */
object BindingExtensions {

  implicit class ScalaBinder(b: Binder) {
    def bindType[T: TypeTag] = b bind typeLiteral[T]
  }

  implicit class ScalaScopedBindingBuilder(b: ScopedBindingBuilder) {
    def inType[TAnn <: JAnnotation : TypeTag]() = b in cls[TAnn]
  }

  implicit class ScalaLinkedBindingBuilder[T](b: LinkedBindingBuilder[T]) {
    def toType[TImpl <: T : TypeTag] = b to typeLiteral[TImpl]

    def toProviderType[TProvider <: Provider[_ <: T] : TypeTag] = b toProvider cls[TProvider]
  }

  implicit class ScalaAnnotatedBindingBuilder[T](b: AnnotatedBindingBuilder[T]) {
    def annotatedWithType[TAnn <: JAnnotation : TypeTag] = b annotatedWith cls[TAnn]
  }

  implicit class ScalaAnnotatedConstantBindingBuilder(b: AnnotatedConstantBindingBuilder) {
    def annotatedWithType[TAnn <: JAnnotation : TypeTag] = b annotatedWith cls[TAnn]
    def annotatedWithName(name: String) = b annotatedWith Names.named(name)
  }

  implicit class ScalaConstantBindingBuilder(b: ConstantBindingBuilder) {
    def to[T: TypeTag]() = b to cls[T]
  }
}

