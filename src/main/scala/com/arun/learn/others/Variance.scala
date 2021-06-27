package com.arun.learn.others

/* >: lower bound <: upper bound
 3 types of variance
 1. invariance: normal usage, means dogshelter will only be used where dog shelter is required
 2. covariance:
  functions have two positions: 1 input position, 2 output positions
  input position is always contra variant
  output position is always covariant
  see some Function2: trait Function2[-T1,-T2, +R] extends AnyRef
 */

abstract class Animal
final case class Dog(name: String) extends Animal

abstract class AnimalShelter[+A <: Animal]:
  def adopt(name: String): Animal

final class DogShelter extends AnimalShelter[Dog]:
  override def adopt(name: String): Dog = Dog(name)

/*
abstract class Vet:
  def diagnose(dog: Dog): String

this will throw error as it doesnt able to override the def
final class ExperiencedVet extends Vet:
  override def diagnose(animal: Animal): String =
    s"$animal will be fine"

to fix this there is  a hack as below

abstract class Vet:
  def diagnose: Dog => String

final class ExperiencedVet extends Vet:
  override val diagnose: Animal => String = animal => s"$animal will be fine"
this works because internally above val is converted to this Function2: trait Function2[-T1,-T2, +R] extends AnyRef
and the input is contravariant which makes it accpet any super type of the type


to work actually we can do this way
don't need to use bound as we are not concerned with that for now
 */
abstract class Vet[-A >: Dog]:
  def diagnose(a: A): String

final class ExperiencedVet extends Vet[Animal]:
  override def diagnose(animal: Animal): String =
    s"$animal will be fine"

object Variance: // extends scala.App:
  println("_" * 100)

  val shelter: AnimalShelter[Dog] = new DogShelter
  val animal = shelter.adopt("snoopy")
  println(animal)

  val vet: Vet[Animal] = new ExperiencedVet
  println(vet.diagnose(animal))
  println("_" * 100)
