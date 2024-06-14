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
    val counter = RegInit(0.U(bitWidth.W))
    val r = RegInit(0.U(bitWidth.W))
    
    when(io.start){
        io.done := false.B
        counter := bitWidth.U - 1.U
        divisor := io.divisor
        remainder := 0.U
        quotient := io.dividend
    }.otherwise{
        counter := counter - 1.U
        remainder(0.U) := quotient(counter)
        when(remainder < divisor){
            quotient(counter) := 0.U
            remainder := remainder << 1 
        }.otherwise{
            quotient(counter) := 1.U
            remainder := remainder - divisor
        }
        when(counter === 0.U){
            io.quotient := Cat(quotient.reverse)
            io.remainder := r(bitWidth-1, 0)
            io.done := true.B
        }
    }
}