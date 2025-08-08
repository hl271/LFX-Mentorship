package stack

import chisel3.stage.ChiselStage
import java.nio.file.Paths

// Your code starts here
import chisel3._
import chisel3.util._

/* StackModule.scala
 * This module implements a stack with push, pop, and peek operations.
 * It includes overflow and underflow checks, and provides status flags.
 *
 * Parameters:
 * - dataWidth: Bit width of a single stack element
 * - len: Length of the stack (number of elements it can hold)
 */
class StackModule(dataWidth: Int, len: Int) extends Module {
  /* Assertion checks */
  require(len >= 1, "Stack length must be at least 1")
  require(dataWidth >= 1, "Data width must be at least 1 bit")

  /* Module I/O declaration */
  val io = IO(new Bundle {
    // Input
    val in = Input(UInt(32.W)) // 32-bit input instruction
    // Output
    val out = Output(UInt(dataWidth.W)) // output values from the stack
    // Output status flags
    val underflow = Output(Bool()) // underflow flag for the "pop" & "peek" instructions
    val overflow = Output(Bool()) // overflow flag for the "push" instruction
    val isEmpty = Output(Bool()) // check if the stack is empty
    val isFull = Output(Bool()) // check if the stack is full
    val popped = Output(Bool()) // indicate successful pop operation
    val peeked = Output(Bool()) // indicate successful peek operation
  })

  /* Define instruction opcodes */
  object StackOpcodes {
    val PUSH = "b0100111".U
    val POP = "b1000011".U
    val PEEK = "b1000000".U
  }
  /* State Elements */
  // Create a register file with `len` regs, each initialized to dataWidth-bit 0
  val stackMem = RegInit(VecInit(Seq.fill(len)(0.U(dataWidth.W))))
  // Stack pointer register that points to the top of the stack
  val stackPtr = RegInit(0.U(chisel3.util.log2Ceil(len+1).W)) // width of ptr = log of stack's len
  // Output data register
  val regOut = RegInit(0.U(dataWidth.W))
  // Status registers
  val regPopped = RegInit(false.B)
  val regPeeked = RegInit(false.B)
  val regOverflow = RegInit(false.B)
  val regUnderflow = RegInit(false.B)
  // Set default values for registers in each clock cycle
  regOut := 0.U
  regPopped := false.B
  regPeeked := false.B
  regOverflow := false.B
  regUnderflow := false.B

  /* I/O assignments */
  // Combinational
  io.isFull := stackPtr === len.U
  io.isEmpty := stackPtr === 0.U

  // Sequential
  io.out := regOut
  io.popped := regPopped
  io.peeked := regPeeked
  io.overflow := regOverflow
  io.underflow := regUnderflow

  /* Instruction decoding */
  val instOp = io.in(6, 0)
  // Extract the data part of the instruction based on dataWidth
  val instData = if (dataWidth > 25) io.in(31, 7).pad(dataWidth) else io.in(7 + dataWidth - 1, 7)

  /* Sequential logic for stack operations */
  when (instOp === StackOpcodes.PUSH) {
    when (!io.isFull) {
      stackMem(stackPtr) := instData
      stackPtr := stackPtr + 1.U
    }.otherwise {
      // Stack overflow
      regOverflow := true.B
    }
  }.elsewhen (instOp === StackOpcodes.POP) {
    when (!io.isEmpty) {
      stackPtr := stackPtr - 1.U
      regPopped := true.B
      regOut := stackMem(stackPtr - 1.U) // Return pop value at the top of the stack
    }.otherwise {
      // Stack underflow
      regUnderflow := true.B
    }
  }.elsewhen (instOp === StackOpcodes.PEEK) {
    when (!io.isEmpty) {
      regPeeked := true.B
      regOut := stackMem(stackPtr - 1.U) // Return peeking value at the top element of the stack
    }.otherwise {
      // Stack underflow
      regUnderflow := true.B
    }
  }
}


// Your code ends here

object SVGen extends App {
  val out = Paths.get(
    "out",
    this.getClass
      .getName
      .stripSuffix("$")
  ).toString
  new ChiselStage().emitSystemVerilog(
    new StackModule(args(0).toInt, args(1).toInt),
    Array("--target-dir", out),
  )
}
