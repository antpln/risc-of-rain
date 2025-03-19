#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    ADD { rd: usize, rs1: usize, rs2: usize },
    SUB { rd: usize, rs1: usize, rs2: usize },
    LOAD { rd: usize, rs1: usize, imm: i32 },
    STORE { rs1: usize, rs2: usize, imm: i32 },
    BEQ { rs1: usize, rs2: usize, imm: i32 },
    NOP,
}

#[derive(Debug)]
struct Pipeline {
    if_id: Option<Instruction>,
    id_ex: Option<Instruction>,
    ex_mem: Option<Instruction>,
    mem_wb: Option<Instruction>,
}

#[derive(Debug)]
pub struct CPU {
    registers: [i32; 32],
    pc: usize,
    memory: [i32; 256],
    cycle_count: u64,
    pipeline: Pipeline,
    instruction_memory: Vec<Instruction>, // Empty program memory
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            registers: [0; 32],
            pc: 0,
            memory: [0; 256],
            cycle_count: 0,
            pipeline: Pipeline {
                if_id: None,
                id_ex: None,
                ex_mem: None,
                mem_wb: None,
            },
            instruction_memory: Vec::new(), // Empty program memory
        }
    }

    pub fn load_program(&mut self, program: Vec<Instruction>) {
        self.instruction_memory = program; // Load program into instruction memory
    }

    pub fn set_register(&mut self, index: usize, value: i32) {
        if index < self.registers.len() {
            self.registers[index] = value;
        }
    }

    fn fetch(&mut self) {
        if self.pc < self.instruction_memory.len() {
            self.pipeline.if_id = Some(self.instruction_memory[self.pc]); // Read instruction from memory
        } else {
            self.pipeline.if_id = Some(Instruction::NOP);  // If out of bounds, return NOP
        }
    }

    fn fetch_inst(&self) -> Instruction {
        // Simple instruction fetch simulation
        match self.pc {
            0 => Instruction::ADD { rd: 1, rs1: 2, rs2: 3 },
            1 => Instruction::SUB { rd: 4, rs1: 5, rs2: 6 },
            2 => Instruction::NOP, // Stops execution
            _ => Instruction::NOP,
        }
    }

    fn decode(&mut self) {
        self.pipeline.id_ex = self.pipeline.if_id.take();
    }

    fn execute(&mut self) {
        if let Some(inst) = self.pipeline.id_ex.take() {
            self.pipeline.ex_mem = Some(inst);
        }
    }

    fn memory_access(&mut self) {
        self.pipeline.mem_wb = self.pipeline.ex_mem.take();
    }

    fn write_back(&mut self) {
        if let Some(inst) = self.pipeline.mem_wb.take() {
            match inst {
                Instruction::ADD { rd, rs1, rs2 } => {
                    self.registers[rd] = self.registers[rs1] + self.registers[rs2];
                }
                Instruction::SUB { rd, rs1, rs2 } => {
                    self.registers[rd] = self.registers[rs1] - self.registers[rs2];
                }
                _ => {}
            }
        }
    }

    pub fn run(&mut self) {
        loop {
            self.cycle_count += 1;

            self.write_back();
            self.memory_access();
            self.execute();
            self.decode();
            self.fetch();

            println!(
                "Cycle {}: PC={} Pipeline {:?}",
                self.cycle_count, self.pc, self.pipeline
            );

            self.pc += 1;
            if self.cycle_count > 10 {
                break;
            }
        }
        println!("Final Register State: {:?}", self.registers);
    }
}
