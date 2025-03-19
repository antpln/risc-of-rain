mod cpu;

fn main() {
    println!("Starting RISC-V Cycle-Accurate Simulator...");
    
    let mut cpu = cpu::CPU::new();  // Initialize CPU
    cpu.run();  // Run the simulation
    
    println!("Simulation completed!");
}