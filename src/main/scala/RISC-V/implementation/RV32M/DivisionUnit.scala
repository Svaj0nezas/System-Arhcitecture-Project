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
    val opcode = WireInit(0.U(7.W))
    val regValue = WireInit(0.U(32.W))

    val sReg1 = WireInit(0.S(32.W))
    val sReg2 = WireInit(0.S(32.W))
    val regSValue = WireInit(0.S(32.W))

    io_data <> DontCare
    io_reg <> DontCare
    io_pc <> DontCare
    io_reset <> DontCare

    opcode := io.instr(6,0) //RISCV_TYPE.getOP(RISCV_TYPE.UNKNOWN).asUInt
    oper2 := io.instr(31,25) //RISCV_TYPE.getFunct7(RISCV_TYPE.UNKNOWN).asUInt
    oper1 := io.instr(14,12) //RISCV_TYPE.getFunct3(RISCV_TYPE.UNKNOWN).asUInt
    io_reg.reg_write_en := false.B
    io_reg.reg_write_data := 0.U
    io_reg.reg_rs1 := io.instr(19,15)
    io_reg.reg_rs2 := io.instr(24,20)
    io_reg.reg_rd := io.instr(11,7)

   when(was_stalled === STALL_REASON.EXECUTION_UNIT){
       when(io_data.data_gnt){
           stalled := STALL_REASON.NO_STALL
       }
    }.elsewhen(opcode === 51.U && oper2 === 1.U){
    switch(io.instr(14,12)){
        is("b100".U){
            sReg1 := io_reg.reg_read_data1.asSInt
            sReg2 := io_reg.reg_read_data2.asSInt
            when(io_reg.reg_read_data2 === 0.U){
                regValue := (-1).S.asUInt
            }.otherwise{
                regValue := (sReg1 / sReg2).asUInt
            }   
            printf(p"$regValue")
            io.valid := true.B
        }
        is("b101".U){
            when(io_reg.reg_read_data2 === 0.U){
                regValue := (2^32-1).U
            }.otherwise{
                regValue := io_reg.reg_read_data1 / io_reg.reg_read_data2
            }
            io.valid := true.B
        }
        is("b110".U){
            sReg1 := io_reg.reg_read_data1.asSInt
            sReg2 := io_reg.reg_read_data2.asSInt
            when(io_reg.reg_read_data2 === 0.U){
                regValue := io_reg.reg_read_data1
            }.otherwise{
                regValue := (sReg1 % sReg2).asUInt
            }
            io.valid := true.B
        }
        is("b111".U){
            
            when(io_reg.reg_read_data2 === 0.U){
                regValue := io_reg.reg_read_data1
            }.otherwise{
                regValue := io_reg.reg_read_data1 % io_reg.reg_read_data2
            }
            io.valid := true.B
        }
        }
    }
    io_reg.reg_write_en := true.B
    io_reg.reg_write_data := regValue
    io_pc.pc_we := true.B
    io_pc.pc_wdata := io_pc.pc + 4.U
}