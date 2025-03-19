mod cpu;

fn main() {
    println!("Starting RISC-V Simulator...");

    let mut cpu = cpu::CPU::new();

    // Initialize registers:
    // x2 = 5, x3 = 10 so that ADD produces x1 = 15
    // x5 = 20, x6 = 3 so that SUB produces x4 = 17
    cpu.set_register(2, 5);
    cpu.set_register(3, 10);
    cpu.set_register(5, 20);
    cpu.set_register(6, 3);

    // Test program: Simple ADD, SUB, and NOP sequence
    let program = vec![
        cpu::Instruction::ADD { rd: 1, rs1: 2, rs2: 3 },  // x1 = x2 + x3 => 15
        cpu::Instruction::SUB { rd: 4, rs1: 5, rs2: 6 },  // x4 = x5 - x6 => 17
        cpu::Instruction::NOP,  // Stop execution
    ];

    cpu.load_program(program); // Load program into memory
    cpu.run(); // Execute the program

    println!("Simulation completed!");
}
