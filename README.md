# Testing

Voici le sujet, créer un programme qui calcule le score d'une partie de bowling.

http://butunclebob.com/ArticleS.UncleBob.TheBowlingGameKata


Après le checkout de ce projet, vous pouvez déjà lancer le projet avec la commande suivante:
```
sbt test
```

Vous devrier avoir un rapport comme celui-ci:
```
sbt:kata_bowling_scala> test
[info] Run completed in 17 milliseconds.
[info] Total number of tests run: 0
[info] Suites: completed 0, aborted 0
[info] Tests: succeeded 0, failed 0, canceled 0, ignored 0, pending 0
[info] No tests were executed.
```

Si tout fonctionne correctement, vous pouvez demander à `sbt` de lancer continuellement les tests unitaires après chaque modification de fichier:
```
sbt ~test
```

Vous utiliserez Scala comme langage et le framework [ScalaTest](http://www.scalatest.org/).

Un fichier `BownlingSpec.scala` dans le projet. Il permet d'avoir un squelette de tests avec la convention [FunSpec](http://www.scalatest.org/user_guide/selecting_a_style), comme l'exemple:
```
import org.scalatest.FunSpec

class SetSpec extends FunSpec {

  describe("A Set") {
    describe("when empty") {
      it("should have size 0") {
        assert(Set.empty.size == 0)
      }

      it("should produce NoSuchElementException when head is invoked") {
        assertThrows[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}
```
Vous êtes cependant libre de changer de style si vous le souhaitez.


# Contexte

The **game** consists of 10 **frames**.  In each frame the player has two opportunities to knock down 10 **pins**.
The **score** for the frame is the total number of pins knocked down, plus *bonuses* for **strikes** and **spares**.

A spare is when the player knocks down all 10 pins in two tries.
The bonus for that frame is the number of pins knocked down by the next **roll**.

A strike is when the player knocks down all 10 pins on his first try.
The bonus for that frame is the value of the next two balls rolled.

In the *tenth frame* a player who rolls a spare or strike is allowed to roll the extra balls to complete the frame.
*However no more than three balls can be rolled in tenth frame.*


# Exercices

## Exercice1: Tests et implémentations

Implémentez les tests et le code pour passer les règles données ci-dessus.
=> **pensez à faire des tests lisibles et qui réutilisent les termes en bold ci-dessus!**
=> **pensez à avoir des assertions avec des messages propres.**
=> **je vous conseille fortement d'écrire les tests du plus simple au plus compliqué et d'essayer Test Driven Development.**

Votre première implémentation doit être simple et ne pas vérifier à la compilation de contraintes sur les *rolls*.


Quelques tests à implémenter:
```
All roll with 0 pin down give a score of 0
All roll with strikes give 300.
All roll with 1 pin down give 20.
```

## Exercice2: Property based testing

Implémentez au moins deux tests par propriété avec ScalaTest + [ScalaCheck](http://www.scalatest.org/user_guide/generator_driven_property_checks).
(*les deux premiers tests donnés dans l'énoncé de l'exercice 1 sont un bon départ !*)

Pour cela, il vous faudra créer un générateur aléatoire de **roll** avec les conditions suivantes:
* `roll` entre 0 et 10 pour le premier *roll* d'une frame.
* `roll` entre n et 10 pour le second *roll* d'une frame si pas strike.

À partir de ce `Gen[Roll]`, vous devriez pouvoir avoir un `Gen[Frame]` et donc ensuite un `Gen[Game]` (*les types ici sont donnés à titre indicatif*).


## Exercice3: Type Driven Development (bonus)

Pour améliorer la qualité du projet, on peut vérifier à la compilation des combinaisons impossibles.

=> **Rendre impossible la création de game ne peut pas avoir plus de 10 frames**.
=> **Type votre classe frame pour quelle puisse être**:
* Regular
* Spare
* Strike

(Pensez ADT [Algrebic Data Type in Scala](http://tpolecat.github.io/presentations/algebraic_types.html#11))!