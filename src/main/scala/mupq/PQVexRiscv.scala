package mupq

import scala.collection.mutable.ArrayBuffer

import java.nio.{ByteBuffer, ByteOrder}


import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc._
import spinal.lib.bus.simple._
import spinal.lib.io._
import spinal.lib.com.jtag._
import spinal.lib.com.uart._

import vexriscv._
import vexriscv.demo.MuraxApb3Timer
import vexriscv.plugin._

class Apb3Key extends Component{
  val apbConfig = Apb3Config(
    addressWidth  = 12,
    dataWidth     = 32,
    selWidth      = 1,
    useSlaveError = false
  )

  val io = new Bundle {
    val apb = slave(Apb3(apbConfig))
  }
  
  val data = List(U"had143d95", U"h10335acb", U"h7ca844fb", U"hb814c3ae", U"he66581ea", U"h4ab51e23", U"h505fb803", U"h1fb2f2fb").read(io.apb.PADDR(4 downto 2))
  
  io.apb.PRDATA := RegNext(data.asBits).resize(32)
  io.apb.PREADY := True
}

/*
class Apb3Key(onChipRamBinFile : String) extends Component{
  import java.nio.file.{Files, Paths}
  val byteArray = Files.readAllBytes(Paths.get(onChipRamBinFile))
  val wordCount = (byteArray.length+3)/4
  val buffer = ByteBuffer.wrap(Files.readAllBytes(Paths.get(onChipRamBinFile))).order(ByteOrder.LITTLE_ENDIAN);
  val wordArray = (0 until wordCount).map(i => {
    val v = buffer.getInt
    if(v < 0)  BigInt(v.toLong & 0xFFFFFFFFl) else  BigInt(v)
  })

  val io = new Bundle{
    val apb = slave(Apb3(log2Up(wordCount*4),32))
  }

  val rom = Mem(Bits(32 bits), wordCount) initBigInt(wordArray)
//  io.apb.PRDATA := rom.readSync(io.apb.PADDR >> 2)
  io.apb.PRDATA := rom.readAsync(RegNext(io.apb.PADDR >> 2))
  io.apb.PREADY := True
}
*/
class ICE40I2CMaster(i2cnum: Int = 0) extends Component {
  val apbConfig = Apb3Config(
    addressWidth  = 12,
    dataWidth     = 32,
    selWidth      = 1,
    useSlaveError = false
  )

  val io = new Bundle {
    val apb = slave(Apb3(apbConfig))
    
    val scl_i = in Bool
    val scl_o = out Bool
    val scl_oe = out Bool

    val sda_i = in Bool
    val sda_o = out Bool
    val sda_oe = out Bool
  }

  val i2c = new Ice40I2C
  
  i2c.addGeneric("BUS_ADDR74", if (i2cnum == 0) { "0b0001"} else { "0b0011" })

  i2c.io.SBSTBI := io.apb.PENABLE && io.apb.PSEL(0)
  i2c.io.SBRWI := io.apb.PWRITE
  
  i2c.io.SBADRI0 := io.apb.PADDR(2) // address with 32bit address
  i2c.io.SBADRI1 := io.apb.PADDR(3)
  i2c.io.SBADRI2 := io.apb.PADDR(4)
  i2c.io.SBADRI3 := io.apb.PADDR(5)
  i2c.io.SBADRI4 := io.apb.PADDR(6)
  i2c.io.SBADRI5 := io.apb.PADDR(7)
  i2c.io.SBADRI6 := io.apb.PADDR(8)
  i2c.io.SBADRI7 := io.apb.PADDR(9)
  
  i2c.io.SBDATI0 := io.apb.PWDATA(0)
  i2c.io.SBDATI1 := io.apb.PWDATA(1)
  i2c.io.SBDATI2 := io.apb.PWDATA(2)
  i2c.io.SBDATI3 := io.apb.PWDATA(3)
  i2c.io.SBDATI4 := io.apb.PWDATA(4)
  i2c.io.SBDATI5 := io.apb.PWDATA(5)
  i2c.io.SBDATI6 := io.apb.PWDATA(6)
  i2c.io.SBDATI7 := io.apb.PWDATA(7)

  io.scl_oe := i2c.io.SCLOE
  io.scl_o := i2c.io.SCLO
  i2c.io.SCLI := io.scl_i

  io.sda_oe := i2c.io.SDAOE
  io.sda_o := i2c.io.SDAO
  i2c.io.SDAI := io.sda_i
  
  val data = i2c.io.SBDATO7 ## i2c.io.SBDATO6 ## i2c.io.SBDATO5 ## i2c.io.SBDATO4 ##
             i2c.io.SBDATO3 ## i2c.io.SBDATO2 ## i2c.io.SBDATO1 ## i2c.io.SBDATO0
             
  io.apb.PRDATA := RegNext(data).resize(32)
  io.apb.PREADY := RegNext(i2c.io.SBACKO)
}

class ICE40SPIMaster extends Component {
  val apbConfig = Apb3Config(
    addressWidth  = 12,
    dataWidth     = 32,
    selWidth      = 1,
    useSlaveError = false
  )

  val io = new Bundle {
    val apb = slave(Apb3(apbConfig))
    
    val miso_i = in Bool
    val miso_o = out Bool
    val miso_oe = out Bool

    val mosi_i = in Bool
    val mosi_o = out Bool
    val mosi_oe = out Bool

    val sck_i = in Bool
    val sck_o = out Bool
    val sck_oe = out Bool
    
    val ss_i = in Bool
    val ss_o = out Bool
    val ss_oe = out Bool
  }

  val spi = new Ice40SPI
  spi.addGeneric("BUS_ADDR74", "0b0000")

  spi.io.SBSTBI := io.apb.PENABLE && io.apb.PSEL(0)
  spi.io.SBRWI := io.apb.PWRITE
  
  spi.io.SBADRI0 := io.apb.PADDR(2) // address with 32bit address
  spi.io.SBADRI1 := io.apb.PADDR(3)
  spi.io.SBADRI2 := io.apb.PADDR(4)
  spi.io.SBADRI3 := io.apb.PADDR(5)
  spi.io.SBADRI4 := io.apb.PADDR(6)
  spi.io.SBADRI5 := io.apb.PADDR(7)
  spi.io.SBADRI6 := io.apb.PADDR(8)
  spi.io.SBADRI7 := io.apb.PADDR(9)
  
  spi.io.SBDATI0 := io.apb.PWDATA(0)
  spi.io.SBDATI1 := io.apb.PWDATA(1)
  spi.io.SBDATI2 := io.apb.PWDATA(2)
  spi.io.SBDATI3 := io.apb.PWDATA(3)
  spi.io.SBDATI4 := io.apb.PWDATA(4)
  spi.io.SBDATI5 := io.apb.PWDATA(5)
  spi.io.SBDATI6 := io.apb.PWDATA(6)
  spi.io.SBDATI7 := io.apb.PWDATA(7)

  io.mosi_oe := spi.io.MOE
  io.mosi_o := spi.io.MO
  spi.io.SI := io.mosi_i

  io.miso_oe := spi.io.SOE
  io.miso_o := spi.io.SO
  spi.io.MI := io.miso_i

  io.sck_oe := spi.io.SCKOE
  io.sck_o := spi.io.SCKO
  spi.io.SCKI := io.sck_i
  
  io.ss_oe := spi.io.MCSNOE0
  io.ss_o := spi.io.MCSNO0
  spi.io.SCSNI := io.ss_i
  
  val data = spi.io.SBDATO7 ## spi.io.SBDATO6 ## spi.io.SBDATO5 ## spi.io.SBDATO4 ##
             spi.io.SBDATO3 ## spi.io.SBDATO2 ## spi.io.SBDATO1 ## spi.io.SBDATO0
             
  io.apb.PRDATA := RegNext(data).resize(32)
  io.apb.PREADY := RegNext(spi.io.SBACKO)
}


abstract class PQVexRiscv(
  cpuPlugins: () => Seq[Plugin[VexRiscv]],
  ibusRange: SizeMapping,
  genUART: Boolean = true,
  gpioWidth: Int = 0,
  genTimer: Boolean = false
)
extends Component {
  val coreFrequency: HertzNumber

  /* Clock and resets */

  val asyncReset: Bool = Bool

  val mainClock: Bool = Bool

  val resetCtrlClockDomain: ClockDomain =
    ClockDomain(clock = mainClock, config = ClockDomainConfig(resetKind = BOOT))

  val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
    val bufferedReset = BufferCC(asyncReset)

    val mainClockReset   = RegNext(bufferedReset)
    val systemClockReset = RegNext(bufferedReset)
  }

  val systemClockDomain: ClockDomain = ClockDomain(
    clock = mainClock,
    reset = resetCtrl.systemClockReset,
    frequency = FixedFrequency(coreFrequency)
  )

  val debugClockDomain: ClockDomain = ClockDomain(
    clock = mainClock,
    reset = resetCtrl.mainClockReset,
    frequency = FixedFrequency(coreFrequency)
  )

  /* Bus interconnect */
  val busConfig = PipelinedMemoryBusConfig(
    addressWidth = 32,
    dataWidth = 32
  )

  val busSlaves  = ArrayBuffer[(PipelinedMemoryBus, SizeMapping)]()
  val busMasters = ArrayBuffer[(PipelinedMemoryBus, SizeMapping)]()

  /* VexRiscv Core */
  var jtag: Jtag = null

  val core = new ClockingArea(systemClockDomain) {
    val timerInterrupt    = False
    val externalInterrupt = False

    val config = VexRiscvConfig(plugins = cpuPlugins() ++ Seq(new DebugPlugin(debugClockDomain, 3)))

    val cpu = new VexRiscv(config)
    /* Wire the Busses / Lines to the plugins */
    var ibus: PipelinedMemoryBus = PipelinedMemoryBus(busConfig)
    var dbus: PipelinedMemoryBus = PipelinedMemoryBus(busConfig)
    for (plugin <- cpu.plugins) plugin match {
      case plugin: IBusSimplePlugin =>
        val cpuibus = plugin.iBus.toPipelinedMemoryBus()
        ibus.cmd <-/< cpuibus.cmd
        ibus.rsp >> cpuibus.rsp
      case plugin: DBusSimplePlugin =>
        val cpudbus = plugin.dBus.toPipelinedMemoryBus()
        dbus.cmd <-/< cpudbus.cmd
        dbus.rsp >> cpudbus.rsp
        plugin.dBus.rsp.error := False
      case plugin: CsrPlugin =>
        plugin.externalInterrupt := externalInterrupt
        plugin.timerInterrupt := timerInterrupt
      case plugin: DebugPlugin =>
        plugin.debugClockDomain {
          resetCtrl.systemClockReset setWhen (RegNext(plugin.io.resetOut))
          jtag = plugin.io.bus.fromJtag()
        }
      case _ =>
    }

    busMasters += dbus -> SizeMapping(0L, (1L << 32L))
    busMasters += ibus -> ibusRange
  }

  /* Peripherals */

  var gpio: TriStateArray = null

  var uart : Uart = null
  
  var ice40spi : ICE40SPIMaster = null
  var ice40i2c0 : ICE40I2CMaster = null
  var ice40i2c1 : ICE40I2CMaster = null
  var key : Apb3Key = null

  val peripherals = new ClockingArea(systemClockDomain) {
    if (gpioWidth > 0) {
      gpio = TriStateArray(gpioWidth bits)
    }

    if (genUART) {
      uart = Uart()
    }

    if (genUART || gpioWidth > 0 || genTimer) {
      val apbBridge = new PipelinedMemoryBusToApbBridge(
        apb3Config = Apb3Config(
          addressWidth = 20,
          dataWidth = 32
        ),
        pipelineBridge = false,
        pipelinedMemoryBusConfig = busConfig
      )

      busSlaves += apbBridge.io.pipelinedMemoryBus -> SizeMapping(0xf0000000L, 1 MiB)

      val apbMapping = ArrayBuffer[(Apb3, SizeMapping)]()

      if (gpioWidth > 0) {
        val gpioACtrl = Apb3Gpio(gpioWidth = gpioWidth, withReadSync = true)
        gpio <> gpioACtrl.io.gpio
        apbMapping += gpioACtrl.io.apb -> (0x00000, 4 KiB)
      }

      if (genUART) {
        val uartCtrlConfig = UartCtrlMemoryMappedConfig(
          uartCtrlConfig = UartCtrlGenerics(
            dataWidthMax = 8,
            clockDividerWidth = 20,
            preSamplingSize = 1,
            samplingSize = 3,
            postSamplingSize = 1
          ),
          initConfig = UartCtrlInitConfig(
            baudrate = 115200,
            dataLength = 7, //7 => 8 bits
            parity = UartParityType.NONE,
            stop = UartStopType.ONE
          ),
          busCanWriteClockDividerConfig = false,
          busCanWriteFrameConfig = false,
          txFifoDepth = 16,
          rxFifoDepth = 16
        )

        val uartCtrl = Apb3UartCtrl(uartCtrlConfig)
        uart <> uartCtrl.io.uart
        core.externalInterrupt setWhen (uartCtrl.io.interrupt)
        apbMapping += uartCtrl.io.apb -> (0x10000, 4 KiB)
      }

      if (genTimer) {
        val timer = new MuraxApb3Timer()
        core.timerInterrupt setWhen (timer.io.interrupt)
        apbMapping += timer.io.apb -> (0x20000, 4 KiB)
      }

      
      ice40spi = new ICE40SPIMaster
      apbMapping += ice40spi.io.apb -> (0x30000, 4 KiB)
      
      ice40i2c0 = new ICE40I2CMaster(0)
      apbMapping += ice40i2c0.io.apb -> (0x40000, 4 KiB)
      
      ice40i2c1 = new ICE40I2CMaster(1)
      apbMapping += ice40i2c1.io.apb -> (0x50000, 4 KiB)
      
      key = new Apb3Key(/*"src/main/scala/mupq/secret.key"*/)
      apbMapping += key.io.apb -> (0x60000, 4 KiB)
      
      val apbDecoder = Apb3Decoder(
        master = apbBridge.io.apb,
        slaves = apbMapping
      )
    }
  }

  def buildInterconnect(): Unit = {
    assert(!SizeMapping.verifyOverlapping(busSlaves.map(_._2)))
    val crossbar = new ClockingArea(systemClockDomain) {
      val interconnect = new PipelinedMemoryBusInterconnect()
      interconnect.perfConfig()
      /* Setup the interconnect */
      interconnect.addSlaves(busSlaves: _*)
      /* Check which masters overlap with which slaves */
      def overlaps(a: SizeMapping, b: SizeMapping): Boolean =
        if (a.base < b.base) a.end >= b.base else b.end >= a.base
      interconnect.addMasters(
        busMasters.map(m =>
          m._1 -> busSlaves.filter(s => overlaps(m._2, s._2)).map(s => s._1).toSeq): _*
      )
    }
  }

  Component.current.addPrePopTask(() => buildInterconnect())
}

object PQVexRiscv {
  type PluginSeq = Seq[Plugin[VexRiscv]]
  type PluginGen = () => PluginSeq

  /** Basic set of Plugins (conforms mostly to rv32i) */
  def baseConfig(base: PluginGen = () => Seq()) = () =>
    base() ++ Seq(
      new IBusSimplePlugin(
        resetVector = 0x00000000L,
        cmdForkOnSecondStage = true,
        cmdForkPersistence = false,
        prediction = NONE,
        catchAccessFault = false,
        compressedGen = true
      ),
      new DBusSimplePlugin(
        catchAddressMisaligned = false,
        catchAccessFault = false,
        earlyInjection = false
      ),
      new CsrPlugin(
        CsrPluginConfig
          .smallest(0x00000000L)
          .copy(
            mtvecAccess = CsrAccess.READ_WRITE,
            mcycleAccess = CsrAccess.READ_ONLY,
            minstretAccess = CsrAccess.READ_ONLY 
          )
      ),
      new DecoderSimplePlugin(
        catchIllegalInstruction = false
      ),
      new RegFilePlugin(
        regFileReadyKind = plugin.SYNC,
        zeroBoot = false
      ),
      new IntAluPlugin,
      new SrcPlugin(
        separatedAddSub = false,
        executeInsertion = false
      ),
      new FullBarrelShifterPlugin,
      new HazardSimplePlugin(
        bypassExecute = true,
        bypassMemory = true,
        bypassWriteBack = true,
        bypassWriteBackBuffer = true,
        pessimisticUseSrc = false,
        pessimisticWriteRegFile = false,
        pessimisticAddressMatch = false
      ),
      new BranchPlugin(
        earlyBranch = false,
        catchAddressMisaligned = false
      ),
      new YamlPlugin("cpu0.yaml")
    )

  /** Plugins for a small multiplier */
  def smallMultiplier = Seq(
    new MulDivIterativePlugin(
      genMul = true,
      genDiv = true,
      mulUnrollFactor = 1,
      divUnrollFactor = 1
    )
  )

  /** Config with a small multiplier */
  def withSmallMultiplier(base: PluginGen = baseConfig()) = () => base() ++ smallMultiplier

  /** Plugins for a multiplier for FPGAs */
  def dspMultiplier = Seq(
    new Mul16Plugin,
    new MulDivIterativePlugin(
      genMul = false,
      genDiv = true,
      divUnrollFactor = 1
    )
  )

  /** Config with a multiplier for FPGAs */
  def withDSPMultiplier(base: PluginGen = baseConfig()) = () => base() ++ dspMultiplier
}
