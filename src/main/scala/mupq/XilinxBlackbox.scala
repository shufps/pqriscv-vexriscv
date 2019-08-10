package mupq

import spinal.core._

case class XilinxPLLSpec(
  pfdMin : HertzNumber = 19 MHz,
  pfdMax : HertzNumber = 450 MHz,
  vcoMin : HertzNumber = 800 MHz,
  vcoMax : HertzNumber = 1600 MHz
) {

  /**
    * Calculates the parameters for a PLL/MMCM
    *
    * @param clkIn Input frequency of the PLL
    * @param clkOut Output ferquencies (up to 6) of the PLL
    * @
    *
    */
  def calcPLLParameters(clkIn : HertzNumber, clkOut : Seq[HertzNumber]) : Tuple3[Int, Int, Array[Int]] = {
    require(clkOut.length > 0 && clkOut.length <= 6, "Number of output clocks must be 1-6")
    // def gcd(a : Long, b : Long) : Long = if(b == 0) a else gcd(b, a % b)
    // def lcm(a : Long, b : Long) : Long = (a * b).abs / gcd(a, b)
    // val commonFreq = clkOut.map(_.toLong).reduceLeft((a, b) => lcm(a, b))
    // println(f"Finding D/M for common frequency ${commonFreq}")

    val d_min : Int = (clkIn.toDouble / pfdMax.toDouble).ceil.toInt
    val d_max : Int = (clkIn.toDouble / pfdMin.toDouble).floor.toInt
    val m_min : Int = ((vcoMin.toDouble / clkIn.toDouble) * d_min).ceil.toInt
    val m_max : Int = ((vcoMax.toDouble / clkIn.toDouble) * d_max).floor.toInt

    // println(f"d_min: $d_min d_max: $d_max m_min: $m_min m_max: $m_max m_ideal: ${d_min * vcoMax.toLong / clkIn.toLong}")

    val cand = (for {
                  d <- d_min to d_max; m <- m_min to m_max
                  val vco = clkIn.toLong * m / d
                  if (vco >= vcoMin.toLong && // Don't exeed VCOMIN
                        vco <= vcoMax.toLong && // Don't exeed VCOMAX
                        clkOut.forall( // VCO should be a multiple of all output frequencies (but at most 128)
                          f => vco % f.toLong == 0 && vco / f.toLong < 128
                        ))
                    } yield (d, m, vco)
    ).sortBy((x) => (-x._3, x._2, x._1)) // Priority sort: highest VCO with lowest m and d

    assert(cand.length != 0, "Unable to find valid configuration!")

    // println(f"Found candidates: ${cand}")

    val best = cand.head

    // println(f"Best is: ${best}")

    val (d, m, vco) = best

    return (d, m, clkOut.map(c => (vco / c.toLong).toInt).toArray)
  }
}

object XilinxPLLCalc {
  def main(args: Array[String]) : Unit = {
    val spec = XilinxPLLSpec()
    println(spec.calcPLLParameters(33 MHz, Array(528 MHz, 264 MHz, 176 MHz, 132 MHz, 66 MHz, 33 MHz)))
    println(spec.calcPLLParameters(100 MHz, Array(50 MHz)))
  }
}

class XilinxPLLBase(clkIn : HertzNumber, clkOut : Seq[HertzNumber],
                    specs : XilinxPLLSpec = XilinxPLLSpec()) extends BlackBox {
  val (d, m, o) = specs.calcPLLParameters(clkIn, clkOut)
  val outdiv = o ++ Array.fill(6 - o.length) {1}
  val generic = new Generic {
    val BANDWIDTH = "OPTIMIZED"
    val CLKFBOUT_MULT = m
    val CLKFBOUT_PHASE = 0.0
    val CLKIN1_PERIOD = 1e9 / clkIn.toDouble
    val CLKOUT0_DIVIDE = outdiv(0)
    val CLKOUT1_DIVIDE = outdiv(1)
    val CLKOUT2_DIVIDE = outdiv(2)
    val CLKOUT3_DIVIDE = outdiv(3)
    val CLKOUT4_DIVIDE = outdiv(4)
    val CLKOUT5_DIVIDE = outdiv(5)
    val CLKOUT0_DUTY_CYCLE = 0.5
    val CLKOUT1_DUTY_CYCLE = 0.5
    val CLKOUT2_DUTY_CYCLE = 0.5
    val CLKOUT3_DUTY_CYCLE = 0.5
    val CLKOUT4_DUTY_CYCLE = 0.5
    val CLKOUT5_DUTY_CYCLE = 0.5
    val CLKOUT0_PHASE = 0.0
    val CLKOUT1_PHASE = 0.0
    val CLKOUT2_PHASE = 0.0
    val CLKOUT3_PHASE = 0.0
    val CLKOUT4_PHASE = 0.0
    val CLKOUT5_PHASE = 0.0
    val DIVCLK_DIVIDE = d
    val REF_JITTER1 = 0.01
    val STARTUP_WAIT = "TRUE"
  }

  val io = new Bundle {
    val CLKFBIN = in Bool
    val CLKFBOUT = out Bool
    val CLKIN1 = in Bool
    val CLKOUT0 = out Bool
    val CLKOUT1 = out Bool
    val CLKOUT2 = out Bool
    val CLKOUT3 = out Bool
    val CLKOUT4 = out Bool
    val CLKOUT5 = out Bool
    val LOCKED = out Bool
    val PWRDWN = in Bool
    val RST = in Bool
  }
  noIoPrefix()
  setBlackBoxName("PLLE2_BASE")
}

class XilinxGlobalBuffer extends BlackBox {
  val io = new Bundle() {
    val I = in Bool
    val O = out Bool
  }
  noIoPrefix()
  setBlackBoxName("BUFG")
}
