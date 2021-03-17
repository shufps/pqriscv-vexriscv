package mupq

import scopt.OptionParser

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.simple._
import spinal.lib.io._
import spinal.lib.misc.{HexTools}
import spinal.lib.com.jtag.Jtag

import vexriscv._
import vexriscv.plugin._

class PipelinedMemoryBusSPRAM(busConfig: PipelinedMemoryBusConfig) extends Component {
  require(busConfig.dataWidth == 32, "Only 32 bit busses")
  val io = new Bundle {
    val bus = slave(PipelinedMemoryBus(busConfig))
  }

  /* Tie together two RAMS to get 32-bit width */
  val rams: Array[Ice40SPRAM] = (0 to 1).map(_ => new Ice40SPRAM).toArray
  val enable                  = io.bus.cmd.valid
  val mask                    = io.bus.cmd.mask
  /* Fan out the simple byte mask of the bus to bit masks */
  val maskLow  = mask(1) ## mask(1) ## mask(0) ## mask(0)
  val maskHigh = mask(3) ## mask(3) ## mask(2) ## mask(2)
  for ((ram, i) <- rams.zipWithIndex) {
    /* Don't ever sleep */
    ram.io.POWEROFF := True
    ram.io.STANDBY := False
    ram.io.SLEEP := False
    // Bus assignments
    ram.io.CHIPSELECT := enable
    ram.io.ADDRESS := io.bus.cmd.address(15 downto 2).asBits
    ram.io.WREN := io.bus.cmd.write
    if (i % 2 == 0) {
      ram.io.MASKWREN := maskLow
      ram.io.DATAIN := io.bus.cmd.data(15 downto 0)
    } else {
      ram.io.MASKWREN := maskHigh
      ram.io.DATAIN := io.bus.cmd.data(31 downto 16)
    }
  }
  /* Always ready */
  io.bus.cmd.ready := True

  /* Memory is synchronous, so response is ready one cycle later */
  io.bus.rsp.valid := Delay(io.bus.cmd.fire && !io.bus.cmd.write, 2, init = False)
  io.bus.rsp.data := Delay(rams(1).io.DATAOUT ## rams(0).io.DATAOUT, 1, init = B(0))
}

class PipelinedMemoryBusEBRAM(onChipRamSize : BigInt, onChipRamHexFile : String, pipelinedMemoryBusConfig : PipelinedMemoryBusConfig) extends Component{
  val io = new Bundle{
    val bus = slave(PipelinedMemoryBus(pipelinedMemoryBusConfig))
  }

  val ram = Mem(Bits(32 bits), onChipRamSize / 4)
  io.bus.rsp.valid := RegNext(io.bus.cmd.fire && !io.bus.cmd.write) init(False)
  io.bus.rsp.data := ram.readWriteSync(
    address = (io.bus.cmd.address >> 2).resized,
    data  = io.bus.cmd.data,
    enable  = io.bus.cmd.valid,
    write  = io.bus.cmd.write,
    mask  = io.bus.cmd.mask
  )
  io.bus.cmd.ready := True

  if(onChipRamHexFile != null){
    HexTools.initRam(ram, onChipRamHexFile, 0x00000000l)
  }
}

class PQVexRiscvUP5K(
  val coreFrequency: HertzNumber = 24 MHz,
  
  cpuPlugins: () => Seq[Plugin[VexRiscv]] = PQVexRiscv.withDSPMultiplier()
)
extends PQVexRiscv(
  cpuPlugins = cpuPlugins,
  gpioWidth = 4,
  ibusRange = SizeMapping(0x00000000L, 256 KiB) // spans RAM and ROM
) {
  val io = new Bundle {
    val clk = in Bool
    /* UART */
    val io_uart_txd = out Bool // TXD
    val io_uart_rxd = in Bool // RXD
    /* JTAG */
    
    val jtag_tdo = out Bool // TDO
    val jtag_tck = in Bool // TCK
    val jtag_tdi = in Bool // TDI
    val jtag_tms = in Bool // TMS
  
    val reset = in Bool
    
    val spi_miso_i = in Bool
    val spi_miso_o = out Bool
    val spi_miso_oe = out Bool

    val spi_mosi_i = in Bool
    val spi_mosi_o = out Bool
    val spi_mosi_oe = out Bool

    val spi_sck_i = in Bool
    val spi_sck_o = out Bool
    val spi_sck_oe = out Bool

    val spi_ss_i = in Bool
    val spi_ss_o = out Bool
    val spi_ss_oe = out Bool

    val gpio_0_i = in Bool
    val gpio_0_o = out Bool
    val gpio_0_oe = out Bool

    val gpio_1_i = in Bool
    val gpio_1_o = out Bool
    val gpio_1_oe = out Bool

    val gpio_2_i = in Bool
    val gpio_2_o = out Bool
    val gpio_2_oe = out Bool

    val gpio_3_i = in Bool
    val gpio_3_o = out Bool
    val gpio_3_oe = out Bool

    val i2c_sda_0_i = in Bool
    val i2c_sda_0_o = out Bool
    val i2c_sda_0_oe = out Bool
    val i2c_scl_0_i = in Bool
    val i2c_scl_0_o = out Bool
    val i2c_scl_0_oe = out Bool

    val i2c_sda_1_i = in Bool
    val i2c_sda_1_o = out Bool
    val i2c_sda_1_oe = out Bool
    val i2c_scl_1_i = in Bool
    val i2c_scl_1_o = out Bool
    val i2c_scl_1_oe = out Bool
  }
  
  asyncReset := !io.reset
  
  ice40i2c0.io.scl_i := io.i2c_scl_0_i
  io.i2c_scl_0_o := ice40i2c0.io.scl_o
  io.i2c_scl_0_oe := ice40i2c0.io.scl_oe

  ice40i2c0.io.sda_i := io.i2c_sda_0_i
  io.i2c_sda_0_o := ice40i2c0.io.sda_o
  io.i2c_sda_0_oe := ice40i2c0.io.sda_oe

  ice40i2c1.io.scl_i := io.i2c_scl_1_i
  io.i2c_scl_1_o := ice40i2c1.io.scl_o
  io.i2c_scl_1_oe := ice40i2c1.io.scl_oe

  ice40i2c1.io.sda_i := io.i2c_sda_1_i
  io.i2c_sda_1_o := ice40i2c1.io.sda_o
  io.i2c_sda_1_oe := ice40i2c1.io.sda_oe


  gpio(0).read := io.gpio_0_i
  io.gpio_0_o := gpio(0).write
  io.gpio_0_oe := gpio(0).writeEnable

  gpio(1).read := io.gpio_1_i
  io.gpio_1_o := gpio(1).write
  io.gpio_1_oe := gpio(1).writeEnable

  gpio(2).read := io.gpio_2_i
  io.gpio_2_o := gpio(2).write
  io.gpio_2_oe := gpio(2).writeEnable

  gpio(3).read := io.gpio_3_i
  io.gpio_3_o := gpio(3).write
  io.gpio_3_oe := gpio(3).writeEnable

  ice40spi.io.mosi_i := io.spi_mosi_i
  io.spi_mosi_o := ice40spi.io.mosi_o
  io.spi_mosi_oe := ice40spi.io.mosi_oe

  ice40spi.io.miso_i := io.spi_miso_i
  io.spi_miso_o := ice40spi.io.miso_o
  io.spi_miso_oe := ice40spi.io.miso_oe

  ice40spi.io.sck_i := io.spi_sck_i
  io.spi_sck_o := ice40spi.io.sck_o
  io.spi_sck_oe := ice40spi.io.sck_oe

  ice40spi.io.ss_i := io.spi_ss_i
  io.spi_ss_o := ice40spi.io.ss_o
  io.spi_ss_oe := ice40spi.io.ss_oe
  
  mainClock := io.clk
  
  /* Remove io_ prefix from generated Verilog */
  noIoPrefix()
  /* PLL */
  // val pll = new Ice40PLLPad(
  //   divR = 0,
  //   divF = 52,
  //   divQ = 5)

  //  pll.io.PACKAGEPIN := io.ice_clk
  //  pll.io.BYPASS := False
  //  pll.io.RESETB := True
  /* Plugins */
  io.jtag_tdo := jtag.tdo
  jtag.tck := io.jtag_tck
  jtag.tdi := io.jtag_tdi
  jtag.tms := io.jtag_tms

  uart.rxd := io.io_uart_rxd
  io.io_uart_txd := uart.txd

  val memory = new ClockingArea(systemClockDomain) {
    val rom1 = new PipelinedMemoryBusEBRAM(12288, null, busConfig)
    busSlaves += rom1.io.bus -> SizeMapping(0x00000000l, 128 KiB)
    val ram1 = new PipelinedMemoryBusSPRAM(busConfig)
    busSlaves += ram1.io.bus -> SizeMapping(0x00020000L, 64 KiB)
    val ram2 = new PipelinedMemoryBusSPRAM(busConfig)
    busSlaves += ram2.io.bus -> SizeMapping(0x00020000L + (64 KiB).toLong, 64 KiB)
  }
}

object PQVexRiscvUP5K {
  def main(args: Array[String]): Unit = {
    case class PQVexRiscvUP5KConfig(
      cpuPlugins: () => Seq[Plugin[VexRiscv]] = PQVexRiscv.baseConfig()
    )
    val optParser = new OptionParser[PQVexRiscvUP5KConfig]("PQVexRiscvUP5K") {
      head("PQVexRiscvUP5K board")
      help("help") text ("print usage text")
      opt[Unit]("mul") action ((_, c) =>
        c.copy(cpuPlugins = PQVexRiscv.withDSPMultiplier(c.cpuPlugins)))
    }
    val config = optParser.parse(args, PQVexRiscvUP5KConfig()) match {
      case Some(config) => config
      case None         => ???
    }
    val report = SpinalConfig(
      mode = Verilog,
      targetDirectory = "rtl/gen"
    ).generate(
      new PQVexRiscvUP5K(
        cpuPlugins = config.cpuPlugins
      )
    )
    report.mergeRTLSource(s"rtl/gen/${report.toplevelName}.aux")
  }
}
