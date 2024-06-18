package arithmetic

import chisel3._
import chisel3.util._

class Divider(bitWidth: Int) extends Module {
    val io = IO(new Bundle {
        val start = Input(Bool())
        val done = Output(Bool())
        val dividend = Input(UInt(bitWidth.W))
        val divisor = Input(UInt(bitWidth.W))
        val quotient = Output(UInt(bitWidth.W))
        val remainder = Output(UInt(bitWidth.W))
    })

    val remainder = RegInit(0.U(bitWidth.W))       //current remainder
    val quotient = RegInit(VecInit(Seq.fill(bitWidth)(0.U(1.W))))   //= {dividend[i:0], quotient[N−1:i+1]}, where dividend is the input dividend and quotient is the final output quotient, and i is the current cycle
    val divisor = RegInit(0.U(bitWidth.W))         //divisor
    val counter = RegInit(0.U(log2Ceil(bitWidth).W))    //when getting a warning about the size of the bits in the vector, chat gpt showed me to do this log func
    val running = RegInit(false.B)
    val progcount = RegInit(0.U(bitWidth.W))
    io.quotient := 0.U
    io.remainder := 0.U
    io.done := false.B


        when(io.start){
            counter := (bitWidth-1).U
            divisor := io.divisor
            remainder := 0.U
            running := true.B
            progcount := bitWidth.U
            for (i <- 0 until bitWidth) {quotient(i) := io.dividend(i)}
        }
        .elsewhen(running){
            val nextRemainder = Wire(UInt(bitWidth.W))
            nextRemainder :=  (remainder << 1) | quotient(counter)
            when(nextRemainder >= divisor){
                quotient(counter) := 1.U
                remainder := nextRemainder - divisor
                printf(p"$nextRemainder")
            }.otherwise{
                quotient(counter) := 0.U
                remainder := nextRemainder
            }
            printf(p"$quotient $remainder $divisor $counter\n")
            when(progcount === 0.U){     //IMPLEMENT CLOCK, EVERYTHING ELSE WORKS

                io.quotient := quotient.asUInt
                io.remainder := remainder
                io.done := true.B
            }
            counter := counter - 1.U
            progcount := progcount - 1.U
        }
    }
