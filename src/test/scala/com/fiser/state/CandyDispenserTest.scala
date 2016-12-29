package com.fiser.state

import org.scalatest.{Matchers, WordSpec}

class CandyDispenserTest extends WordSpec with Matchers {
  val coins = 10
  val candies = 5
  "A CandyDispenser" should {
    "have 14 coins and 1 candy" when {
      "4 consecutive candies a bought" in {
        val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
        val state: State[Machine, (Int, Int)] = CandyDispenser.simulateMachine(inputs)
        val result: ((Int, Int), Machine) = state.run(Machine(locked = true, candies, coins))
        result shouldBe((14, 1), Machine(locked = true, 1, 14))
      }
    }
  }


  it should {
    "do nothing" when {
      "machine is unlocked and a coin is inserted" in {
        val state: State[Machine, (Int, Int)] = CandyDispenser.simulateMachine(List(Coin))
        val machine = Machine(locked = false, candies, coins)
        val result: ((Int, Int), Machine) = state.run(machine)
        result shouldBe((coins, candies), machine)
      }

      "machine is locked and the knob is turned" in {
        val state: State[Machine, (Int, Int)] = CandyDispenser.simulateMachine(List(Turn))
        val machine = Machine(locked = true, candies, coins)
        val result: ((Int, Int), Machine) = state.run(machine)
        result shouldBe((coins, candies), machine)
      }
    }
  }
}
