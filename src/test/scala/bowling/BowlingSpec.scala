package bowling
import scala.annotation.tailrec
import org.scalatest.{FunSpec, Matchers}

case class Frame(roll1 : Int, roll2 : Int){}
 
case class gameState(score : Int, frames : List[Frame]){
    

    override def toString: String = {
        "Score : " + score +  " \n==========="
    }

    def scoreOf = score

      def roll(pinDown : Int) : gameState = {
          var bonus = 0
        if(gameState.strike(this.frames)){
            bonus = pinDown
        }
        
         copy(score = score + pinDown + bonus)
        
     }

     def frame(roll1 : Int, roll2 : Int) : gameState = {
         var bonus = 0;
        
       val firstroll = roll(roll1)
       var newGameState =  firstroll
      if(gameState.spare(this.frames)){
            bonus =  roll1;
      }
       
       if(roll1 == 10) {
           if(gameState.strikeBonusLeft(this.frames)){
                bonus = roll2
            }
            newGameState = firstroll.copy(score = firstroll.score + bonus, frames = Frame(10, 0) :: firstroll.frames)
       } else {
           val secondroll = firstroll.roll(roll2)
          newGameState =  secondroll.copy(score = secondroll.score + bonus,frames = Frame(roll1, roll2) :: secondroll.frames)
       }

       
       newGameState
       
     }


    
}
object gameState {
   
   @tailrec
      def time(game : gameState, pinDown : Int, nb : Int) :  gameState ={
         if(nb == 0){
             game
         } else {
            gameState.time(game.roll(pinDown), pinDown, nb - 1)
         }
     }

    def spare(frames : List[Frame]) : Boolean = { 
        if(frames.isEmpty){
            false
        } else {
        (frames.head.roll1 + frames.head.roll2) == 10 && !gameState.strike(frames)
         }
    }

    def strike(frames : List[Frame]) : Boolean = { 
        if(frames.isEmpty){
            false
        } else {
        (frames.head.roll1) == 10
         }
    }
    def strikeBonusLeft(frames : List[Frame]) : Boolean= {
        if(frames.isEmpty){
            false
        } else {
            if(frames.tail.isEmpty) {
                false 
            } else {
            gameState.strike(frames.tail)
          }
        }
    }          
}

class BowlingSpec extends FunSpec with Matchers {
    

  describe("Bowling score") {

    val frames = Nil
    val gameAtInit = gameState(0, frames) 

      it("should be 0 when 0 pin has been knock down for the first roll"){
          assert(gameAtInit.roll(0).score == 0)
      }

     it("should be 0 when 0 pin has been knock down for the 20 rolls"){
          assert(gameState.time(gameAtInit,0,20).score == 0)
      }

       val gameOnePinDown = gameState(0, frames) 
      it("should be 20 when 1 pin has been knock down for the 20 rolls"){
          assert(gameState.time(gameOnePinDown,1,20).score == 20)
      }

      it("should be 10 when 5 pins has been knock down in 2 rolls"){
          assert(gameState.time(gameAtInit,5,2).score == 10)
      }

      val gameAfterTwoRolls = gameAtInit.roll(4).roll(6)
     it("should be 10 when 4 pins has been knock down in 1 roll and 6 pins has been knock down in 2 rolls "){
          assert(gameAfterTwoRolls.score == 10)
      }
      

    val gameAfterOneFrame = gameAtInit.frame(4,4)
        it("should be 8 after one frame(4,4) "){
            assert(gameAfterOneFrame.score == 8)
        }

       val gameAfterTwoFrame = gameAtInit.frame(4,4).frame(0,4)
    it("should be 12 afer one frame(4,4) and one frame (0,4) "){
        assert(gameAfterTwoFrame.score == 12)
     }

    val gameAfterOneStrikeWithoutBonus = gameAtInit.frame(10,6)
    it("should be (STRIKE) 10 when a frame consists of one first roll of 10 "){
        assert(gameAfterOneStrikeWithoutBonus.score == 10)
     }
  

  }
   describe("Frames list") {
      val frames = Nil
    val gameAtInit = gameState(0, frames) 

    it("should be empty at the beginning "){
        assert(gameAtInit.frames.isEmpty)
     }

      val gameAfterOneFrame = gameAtInit.frame(4,4)
    it("should not be empty after one frame "){
        assert(!gameAfterOneFrame.frames.isEmpty)
     }
     
     val gameHaveListOfFrames = gameAtInit.frame(4,4).frame(6,0)
    it("should contains Frame(4,4) and Frame(6,0) after one frame of (4,4) and one frame of (6,0) "){
        assert(gameHaveListOfFrames.frames == List(Frame(6,0), Frame(4,4)))
     }

      it("should have a head as Frame(6,0) after one frame of (4,4) and one frame of (6,0) "){
        assert(gameHaveListOfFrames.frames.head == Frame(6,0))
     }
     
       val gameFullGame = gameAtInit.frame(4,4).frame(6,0).frame(6,0).frame(6,0).frame(6,0).frame(6,0).frame(6,0).frame(6,0).frame(6,0).frame(6,0).frame(4,4).frame(6,0).frame(6,0).frame(6,0).frame(6,0).frame(6,0).frame(6,0).frame(6,0).frame(6,0).frame(6,0)
      it("should have a 20 element after a one player full game "){
        assert(gameFullGame.frames.length == 20)
     }
     
     describe("Score with spare (4,6)") {

        val frames = Nil
        val gameAtInit = gameState(0, frames) 
         val gameAtSpare = gameAtInit.frame(4,6)
         it("should be 10 if it is the current frame "){
        assert(gameAtSpare.score == 10)
       }
        val gameAfterSpare = gameAtInit.frame(4,6).frame(4,0)
         it("should be 18 after the second frame (4,0) has been done (=bonus spare active)"){
        assert(gameAfterSpare.score == 18)
       }

     }

     describe("Score with strike (10,0)") {

        val frames = Nil
        val gameAtInit = gameState(0, frames) 
         val gameAtStrike = gameAtInit.frame(10,0)
         it("should be 10 if it is the current frame "){
        assert(gameAtStrike.score == 10)
       }
        val gameAfterStrike = gameAtInit.frame(10,0).frame(4,1)
         it("should be 20 after the second frame (4,1) has been done (=bonus strike active)"){
        assert(gameAfterStrike.score == 20)
       }

        val gameAfter2Strikes = gameAtInit.frame(10,0).frame(10,0).frame(5,0)
         it("should be 45 after 2 strikes and one frame of (5,0)"){
        assert(gameAfter2Strikes.score == 45)
       }

       val gameWithStrikeNineFrames =  gameAtInit.frame(10,0).frame(10,0).frame(10,0).frame(10,0).frame(10,0).frame(10,0).frame(10,0).frame(10,0).frame(10,0)
         it("should be 270 after the nineth frame of strike"){
        assert(gameWithStrikeNineFrames.score == 270)
         }
     }     
      /** it("should be 0 when all roll into gutter"){
          aGameWith(
              rollingInto(gutter).time(20)
          ) should have(scoreOf(20))
      } **/
  }

}
