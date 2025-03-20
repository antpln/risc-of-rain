mod cpu;
use cpu::Instruction; // bring the Instruction enum into scope

fn main() {


    let mut cpu = cpu::CPU::new();


    // Multiplication program using ADDI and ADD instructions
    // r1 = multiplicand, r2 = multiplier, r3 = result
    cpu.set_register(2, 5);
    cpu.set_register(1, 5);
    let program = vec![
        // 0: Initialize r3 = 0.
        Instruction::ADDI { rd: 3, rs1: 0, imm: 0 },
        
        // 1: Accumulate: r3 = r3 + r1.
        Instruction::ADD  { rd: 3, rs1: 3, rs2: 1 },
        
        // 2: Decrement multiplier: r2 = r2 - 1.
        Instruction::ADDI { rd: 2, rs1: 2, imm: -1 },
        
        // 3: Branch: if r2 == 0, branch to PC = 3 + 3 = 6 (exit).
        Instruction::BEQ  { rs1: 2, rs2: 0, imm: 3 },
        
        // 4: Jump back to accumulation (PC = 4 + (-4) = 0, but we want to jump to 1)
        // Adjust the offset so that 4 + offset = 1.
        Instruction::JAL  { rd: 0, imm: -3 },
        
        // 5: Exit.
        Instruction::NOP,
    ];
    
    
    

    cpu.load_program(program);
    println!("Starting RISC-V Simulator...");
    cpu.run();
    println!("Simulation completed!");
}