#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    // Arithmetic instructions
    ADD  { rd: usize, rs1: usize, rs2: usize },
    ADDI { rd: usize, rs1: usize, imm: i32 },
    // Control flow instructions
    BEQ  { rs1: usize, rs2: usize, imm: i32 },
    JAL  { rd: usize, imm: i32 },
    // System instruction
    NOP,
}

#[derive(Debug, Clone, Copy)]
pub struct PipelineRegister {
    pub instruction: Instruction,
    pub pc: usize,             // The PC at which the instruction was fetched.
    pub dest: Option<usize>,   // Destination register (if the instruction writes one).
    pub result: Option<i32>,   // Computed result, computed in EX.
}

#[derive(Debug)]
pub struct Pipeline {
    pub if_id: Option<PipelineRegister>,
    pub id_ex: Option<PipelineRegister>,
    pub ex_mem: Option<PipelineRegister>,
    pub mem_wb: Option<PipelineRegister>,
}

#[derive(Debug)]
pub struct CPU {
    registers: [i32; 32],
    pc: usize,
    cycle_count: u64,
    pipeline: Pipeline,
    instruction_memory: Vec<Instruction>,
    branch_taken: bool,
}

impl CPU {
    pub fn new() -> Self {
        let mut regs = [0; 32];
        regs[0] = 0; // x0 is hardwired to 0.
        CPU {
            registers: regs,
            pc: 0,
            cycle_count: 0,
            pipeline: Pipeline {
                if_id: None,
                id_ex: None,
                ex_mem: None,
                mem_wb: None,
            },
            instruction_memory: Vec::new(),
            branch_taken: false,
        }
    }

    // Prevent accidental writes to x0.
    pub fn set_register(&mut self, index: usize, value: i32) {
        if index != 0 && index < self.registers.len() {
            self.registers[index] = value;
        }
    }

    pub fn load_program(&mut self, program: Vec<Instruction>) {
        self.instruction_memory = program;
    }

    // A helper function for forwarding: reads register value,
    // first checking later pipeline stages.
    fn read_reg(&self, reg: usize) -> i32 {
        if let Some(mem_wb) = self.pipeline.mem_wb {
            if let Some(dest) = mem_wb.dest {
                if dest == reg {
                    if let Some(result) = mem_wb.result {
                        return result;
                    }
                }
            }
        }
        if let Some(ex_mem) = self.pipeline.ex_mem {
            if let Some(dest) = ex_mem.dest {
                if dest == reg {
                    if let Some(result) = ex_mem.result {
                        return result;
                    }
                }
            }
        }
        self.registers[reg]
    }

    // Fetch creates a PipelineRegister with the current PC.
    fn fetch(&mut self) {
        let instr = if self.pc < self.instruction_memory.len() {
            self.instruction_memory[self.pc]
        } else {
            Instruction::NOP
        };
        self.pipeline.if_id = Some(PipelineRegister {
            instruction: instr,
            pc: self.pc,
            dest: None,
            result: None,
        });
    }

    // Hazard detection: if the instruction in IF/ID is a branch and the
    // instruction in ID/EX is writing to one of its source registers,
    // then we have a hazard.
    fn hazard_detect(&self) -> bool {
        if let Some(if_id) = &self.pipeline.if_id {
            if let Instruction::BEQ { rs1, rs2, .. } = if_id.instruction {
                if let Some(id_ex) = &self.pipeline.id_ex {
                    if let Some(dest) = id_ex.dest {
                        if dest == rs1 || dest == rs2 {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    // Decode stage: if a hazard is detected for a branch, stall by inserting a bubble.
    fn decode(&mut self) {
        if self.hazard_detect() {
            // Stall: insert a bubble (NOP) into the ID/EX stage.
            self.pipeline.id_ex = Some(PipelineRegister {
                instruction: Instruction::NOP,
                pc: self.pc, // Current PC (doesn't matter much)
                dest: None,
                result: None,
            });
        } else {
            self.pipeline.id_ex = self.pipeline.if_id.take();
        }
    }

    // Execute stage: compute results for arithmetic instructions and
    // evaluate branch conditions using forwarded register values.
    fn execute(&mut self) {
        if let Some(pr) = self.pipeline.id_ex.take() {
            match pr.instruction {
                Instruction::ADDI { rd, rs1, imm } => {
                    let val = self.read_reg(rs1);
                    let res = val + imm;
                    self.pipeline.ex_mem = Some(PipelineRegister {
                        instruction: pr.instruction,
                        pc: pr.pc,
                        dest: Some(rd),
                        result: Some(res),
                    });
                }
                Instruction::ADD { rd, rs1, rs2 } => {
                    let v1 = self.read_reg(rs1);
                    let v2 = self.read_reg(rs2);
                    let res = v1 + v2;
                    self.pipeline.ex_mem = Some(PipelineRegister {
                        instruction: pr.instruction,
                        pc: pr.pc,
                        dest: Some(rd),
                        result: Some(res),
                    });
                }
                Instruction::BEQ { rs1, rs2, imm } => {
                    let v1 = self.read_reg(rs1);
                    let v2 = self.read_reg(rs2);
                    if v1 == v2 {
                        // Compute branch target using the PC from the pipeline register.
                        self.pc = (pr.pc as i32 + imm) as usize;
                        self.branch_taken = true;
                        self.flush_pipeline();
                        self.pipeline.ex_mem = Some(PipelineRegister {
                            instruction: Instruction::NOP,
                            pc: pr.pc,
                            dest: None,
                            result: None,
                        });
                    } else {
                        self.pipeline.ex_mem = Some(pr);
                    }
                }
                Instruction::JAL { rd, imm } => {
                    if rd != 0 {
                        self.registers[rd] = pr.pc as i32 + 4;
                    }
                    self.pc = (pr.pc as i32 + imm) as usize;
                    self.branch_taken = true;
                    self.flush_pipeline();
                    self.pipeline.ex_mem = Some(PipelineRegister {
                        instruction: Instruction::NOP,
                        pc: pr.pc,
                        dest: None,
                        result: None,
                    });
                }
                // For NOP and any other instructions we simply forward.
                _ => {
                    self.pipeline.ex_mem = Some(pr);
                }
            }
        }
    }

    // Memory access: simply forward the pipeline register.
    fn memory_access(&mut self) {
        self.pipeline.mem_wb = self.pipeline.ex_mem.take();
    }

    // Write-back stage: update registers for arithmetic instructions.
    fn write_back(&mut self) {
        if let Some(pr) = self.pipeline.mem_wb.take() {
            match pr.instruction {
                Instruction::ADDI { rd, .. }
                | Instruction::ADD { rd, .. } => {
                    if let Some(result) = pr.result {
                        if rd != 0 {
                            self.registers[rd] = result;
                        }
                    }
                }
                _ => {}
            }
        }
        // Enforce that register 0 remains 0.
        self.registers[0] = 0;
    }

    fn flush_pipeline(&mut self) {
        self.pipeline.if_id = None;
        self.pipeline.id_ex = None;
        self.pipeline.ex_mem = None;
        self.pipeline.mem_wb = None;
    }

    // Run loop: note that forwarding plus hazard detection now removes the need for manual bubbles.
    pub fn run(&mut self) {
        self.fetch();
        loop {
            self.cycle_count += 1;

            self.write_back();
            self.memory_access();
            self.execute();
            self.decode();

            if !self.branch_taken {
                self.pc += 1;
            } else {
                self.branch_taken = false;
            }
            self.fetch();

            println!("Cycle {}: PC={} Registers={:?} Pipeline={:?}",
                     self.cycle_count, self.pc, self.registers, self.pipeline);
            if self.pc >= self.instruction_memory.len() || self.cycle_count > 100 {
                break;
            }
        }
        println!("Final Register State: {:?}", self.registers);
    }
}
