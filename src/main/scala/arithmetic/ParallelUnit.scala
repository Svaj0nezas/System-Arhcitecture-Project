package arithmetic

import chisel3._
import scala.reflect.ClassTag
import chisel3.util._
import chisel3.experimental.hierarchy.{Definition, Instance, instantiable, public}

abstract class ComputationalUnit(width: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))
        val c = Output(UInt(width.W))
    })  
}

class ParallelUnit(vectorSize: Int, arraySize: Int, unitWidth: Int, comp : (Int) => ComputationalUnit) extends Module {
    require(vectorSize % arraySize == 0)
    val io = IO(new Bundle {
        val a = Input(Vec(vectorSize, UInt(unitWidth.W)))
        val b = Input(Vec(vectorSize, UInt(unitWidth.W)))
        val start = Input(Bool())
        val done = Output(Bool())
        val c = Output(Vec(vectorSize, UInt(unitWidth.W)))
    })

    val units = Seq.fill(arraySize)(Module(comp(unitWidth)))
    val a = RegInit(VecInit(Seq.fill(vectorSize) (0.U(unitWidth.W))))
    val b = RegInit(VecInit(Seq.fill(vectorSize) (0.U(unitWidth.W))))
    val counter = RegInit(0.U(arraySize.W))
    val running = RegInit(false.B)
    val acc = RegInit(0.U(4.W))
    val mem = RegInit(VecInit(Seq.fill(vectorSize) (0.U(unitWidth.W))))
    io.done := false.B
    io.c := DontCare
    for(i<-0 until arraySize){
        units(i).io.a := DontCare
        units(i).io.b := DontCare
    }
    
    when(io.start){
        a := io.a
        b := io.b
        io.done := false.B
        acc := 0.U
        running := true.B
            counter := (vectorSize / arraySize).U
        for(i<-0 until arraySize){
            units(i).io.a := 0.U
            units(i).io.b := 0.U
        }
    }.elsewhen(running){
        for(i<-0 until arraySize){           
            val j = Wire(UInt(4.W))
            j := (i.U + acc * arraySize.U)
            units(i).io.a := a(j)
            units(i).io.b := b(j)
            mem(j) := units(i).io.c
        }
        acc := acc + 1.U
        counter := counter - 1.U
        when(counter === 0.U){
            io.c := mem
            io.done := true.B
            running := false.B
        }
        val r = io.c
        printf(p"$r $acc this is io.c \n")
    }
}