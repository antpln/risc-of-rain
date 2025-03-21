mod cpu;
use cpu::{Assembler, Instruction}; // bring the Instruction enum into scope

fn main() {
    let mut cpu = cpu::CPU::new();

    // Multiplication program using ADDI and ADD instructions
    // r1 = multiplicand, r2 = multiplier, r3 = result
    cpu.set_register(2, 17);
    cpu.set_register(1, 3);
    let program = "00000193
001181B3
FFF10113
00010663
FF3FF06F
00000013
";
    let mut ins = Assembler::program_from_string(program);
    //Check the instructions in the program
    for i in 0..ins.len() {
        println!("Instruction {}: {:?}", i, ins[i]);
    }

    cpu.load_program(ins);
    println!("Starting RISC-V Simulator...");
    cpu.run();
    println!("Simulation completed!");
}
