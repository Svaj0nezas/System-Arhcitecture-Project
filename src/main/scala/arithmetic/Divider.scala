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
    val clock = RegInit(0.U(bitWidth.W))
    
    when(io.start){
        clock := bitWidth.U - 1.U
        remainder := 0.U
        divisor := io.divisor
        quotient(clock) := dividend(clock)
    }.otherwise{
        clock := clock - 1.U
        val r := (remainder << 1) + dividend(clock)
        when(r < divisor){
            quotient(clock) := 0
            remainder := r
        }.otherwise{
            quotient(clock) := 1
            remainder := r - divisor
        }
        when(clock === 0){
            io.quotient := quotient
            io.remainder := remainder
            io.done := true.B
        }
    }
}