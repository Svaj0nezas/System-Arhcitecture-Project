package RISCV.implementation.RV32M

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._


class DivisionUnit extends AbstractExecutionUnit {
    val stalled = WireInit(STALL_REASON.NO_STALL)
    val was_stalled = RegInit(STALL_REASON.NO_STALL)
    when(~io_reset.rst_n){
        was_stalled := STALL_REASON.NO_STALL
    }.otherwise{
        was_stalled := stalled
    }

    
    io.misa := "b01__0000__0_00000_00000_00100_00000_00000".U

    io.valid := false.B    
    io.stall := stalled
    val oper1 = WireInit(0.U(3.W))
    val oper2 = WireInit(0.U(7.W))
    val opcode = RegInit(0.U(7.W))

    io_data <> DontCare
    io_reg <> DontCare
    io_pc <> DontCare
    io_reset <> DontCare

    opcode := io.instr(6,0) //RISCV_TYPE.getOP(RISCV_TYPE.UNKNOWN).asUInt
    oper2 := io.instr(31,25) //RISCV_TYPE.getFunct7(RISCV_TYPE.UNKNOWN).asUInt
    oper1 := io.instr(14,12) //RISCV_TYPE.getFunct3(RISCV_TYPE.UNKNOWN).asUInt
    

    when(was_stalled === STALL_REASON.EXECUTION_UNIT){
        when(io_data.data_gnt){
            stalled := STALL_REASON.NO_STALL
        }
    }.elsewhen(opcode === RISCV_OP.OP.asUInt && oper2 === RISCV_FUNCT7.MULDIV.asUInt){
        switch(oper1){
            is(100.U){
                io.valid := true.B
                when(io_reg.reg_read_data2 === 0.U){
                    io_reg.reg_rd := (-1).S.asUInt
                }.otherwise{
                    io_reg.reg_rd := io_reg.reg_read_data1 / io_reg.reg_read_data2
                }
            }
            is(101.U){
                io.valid := true.B
                when(io_reg.reg_read_data2 === 0.U){
                    io_reg.reg_rd := (2^32-1).U
                }.otherwise{
                    io_reg.reg_rd := io_reg.reg_read_data1 / io_reg.reg_read_data2
                }
            }
            is(110.U){
                io.valid := true.B
                when(io_reg.reg_read_data2 === 0.U){
                    io_reg.reg_rd := io_reg.reg_read_data1
                }.otherwise{
                    io_reg.reg_rd := io_reg.reg_read_data1 % io_reg.reg_read_data2
                }
            }
            is(111.U){
                io.valid := true.B
                when(io_reg.reg_read_data2 === 0.U){
                    io_reg.reg_rd := io_reg.reg_read_data1
                }.otherwise{
                    io_reg.reg_rd := io_reg.reg_read_data1 % io_reg.reg_read_data2
                }
            }
        }
    }
}