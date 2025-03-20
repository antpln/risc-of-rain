#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    // Arithmetic instructions
    ADD   { rd: usize, rs1: usize, rs2: usize },
    ADDI  { rd: usize, rs1: usize, imm: i32 },
    SUB   { rd: usize, rs1: usize, rs2: usize },
    SLL   { rd: usize, rs1: usize, rs2: usize },
    SLLI  { rd: usize, rs1: usize, imm: i32 },
    SLT   { rd: usize, rs1: usize, rs2: usize },
    SLTI  { rd: usize, rs1: usize, imm: i32 },
    SLTU  { rd: usize, rs1: usize, rs2: usize },
    SLTIU { rd: usize, rs1: usize, imm: i32 },
    XOR   { rd: usize, rs1: usize, rs2: usize },
    XORI  { rd: usize, rs1: usize, imm: i32 },
    SRL   { rd: usize, rs1: usize, rs2: usize },
    SRLI  { rd: usize, rs1: usize, imm: i32 },
    SRA   { rd: usize, rs1: usize, rs2: usize },
    SRAI  { rd: usize, rs1: usize, imm: i32 },
    OR    { rd: usize, rs1: usize, rs2: usize },
    ORI   { rd: usize, rs1: usize, imm: i32 },
    AND   { rd: usize, rs1: usize, rs2: usize },
    ANDI  { rd: usize, rs1: usize, imm: i32 },
    LUI   { rd: usize, imm: i32 },
    AUIPC { rd: usize, imm: i32 },

    // Control flow instructions
    JAL   { rd: usize, imm: i32 },
    JALR  { rd: usize, rs1: usize, imm: i32 },
    BEQ   { rs1: usize, rs2: usize, imm: i32 },
    BNE   { rs1: usize, rs2: usize, imm: i32 },
    BLT   { rs1: usize, rs2: usize, imm: i32 },
    BGE   { rs1: usize, rs2: usize, imm: i32 },
    BLTU  { rs1: usize, rs2: usize, imm: i32 },
    BGEU  { rs1: usize, rs2: usize, imm: i32 },

    // Load instructions
    LB    { rd: usize, rs1: usize, imm: i32 },
    LH    { rd: usize, rs1: usize, imm: i32 },
    LW    { rd: usize, rs1: usize, imm: i32 },
    LBU   { rd: usize, rs1: usize, imm: i32 },
    LHU   { rd: usize, rs1: usize, imm: i32 },

    // Store instructions
    SB    { rs1: usize, rs2: usize, imm: i32 },
    SH    { rs1: usize, rs2: usize, imm: i32 },
    SW    { rs1: usize, rs2: usize, imm: i32 },

    // Memory ordering instructions
    FENCE   { pred: i32, succ: i32 },
    FENCE_I { pred: i32, succ: i32 },

    // Environment instructions
    ECALL,
    EBREAK,

    // No operation
    NOP,
}

#[derive(Debug, Clone, Copy)]
pub struct PipelineRegister {
    pub instruction: Instruction,
    pub pc: usize,             // The PC where the instruction was fetched.
    pub dest: Option<usize>,   // Destination register (if any).
    pub result: Option<i32>,   // Computed result (or loaded value, etc.)
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
    memory: Vec<i32>, // simple word-addressable memory
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
            memory: vec![0; 1024], // simple memory model
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

    // Read a register value with basic forwarding.
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

    // Fetch stage: package instruction and current PC.
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

    // Hazard detection: for example, detect hazards for branches.
    fn hazard_detect(&self) -> bool {
        if let Some(if_id) = &self.pipeline.if_id {
            match if_id.instruction {
                Instruction::BEQ { rs1, rs2, .. }
                | Instruction::BNE { rs1, rs2, .. }
                | Instruction::BLT { rs1, rs2, .. }
                | Instruction::BGE { rs1, rs2, .. }
                | Instruction::BLTU { rs1, rs2, .. }
                | Instruction::BGEU { rs1, rs2, .. } => {
                    if let Some(id_ex) = &self.pipeline.id_ex {
                        if let Some(dest) = id_ex.dest {
                            if dest == rs1 || dest == rs2 {
                                return true;
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    // Decode stage: forward IF/ID to ID/EX (or insert bubble on hazard).
    fn decode(&mut self) {
        if self.hazard_detect() {
            self.pipeline.id_ex = Some(PipelineRegister {
                instruction: Instruction::NOP,
                pc: self.pc,
                dest: None,
                result: None,
            });
        } else {
            self.pipeline.id_ex = self.pipeline.if_id.take();
        }
    }

    // Execute stage: compute arithmetic, evaluate branches, etc.
    fn execute(&mut self) {
        if let Some(pr) = self.pipeline.id_ex.take() {
            let next_reg = |dest: Option<usize>, result: Option<i32>| PipelineRegister {
                instruction: pr.instruction,
                pc: pr.pc,
                dest,
                result,
            };
            match pr.instruction {
                // Arithmetic instructions:
                Instruction::ADD { rd, rs1, rs2 } => {
                    let res = self.read_reg(rs1) + self.read_reg(rs2);
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::ADDI { rd, rs1, imm } => {
                    let res = self.read_reg(rs1) + imm;
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::SUB { rd, rs1, rs2 } => {
                    let res = self.read_reg(rs1) - self.read_reg(rs2);
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::SLL { rd, rs1, rs2 } => {
                    let res = self.read_reg(rs1) << (self.read_reg(rs2) & 0x1F);
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::SLLI { rd, rs1, imm } => {
                    let res = self.read_reg(rs1) << (imm & 0x1F);
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::SLT { rd, rs1, rs2 } => {
                    let res = if self.read_reg(rs1) < self.read_reg(rs2) { 1 } else { 0 };
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::SLTI { rd, rs1, imm } => {
                    let res = if self.read_reg(rs1) < imm { 1 } else { 0 };
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::SLTU { rd, rs1, rs2 } => {
                    let res = if (self.read_reg(rs1) as u32) < (self.read_reg(rs2) as u32) { 1 } else { 0 };
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::SLTIU { rd, rs1, imm } => {
                    let res = if (self.read_reg(rs1) as u32) < (imm as u32) { 1 } else { 0 };
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::XOR { rd, rs1, rs2 } => {
                    let res = self.read_reg(rs1) ^ self.read_reg(rs2);
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::XORI { rd, rs1, imm } => {
                    let res = self.read_reg(rs1) ^ imm;
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::SRL { rd, rs1, rs2 } => {
                    let res = (self.read_reg(rs1) as u32 >> (self.read_reg(rs2) & 0x1F)) as i32;
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::SRLI { rd, rs1, imm } => {
                    let res = (self.read_reg(rs1) as u32 >> (imm & 0x1F)) as i32;
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::SRA { rd, rs1, rs2 } => {
                    let res = self.read_reg(rs1) >> (self.read_reg(rs2) & 0x1F);
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::SRAI { rd, rs1, imm } => {
                    let res = self.read_reg(rs1) >> (imm & 0x1F);
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::OR { rd, rs1, rs2 } => {
                    let res = self.read_reg(rs1) | self.read_reg(rs2);
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::ORI { rd, rs1, imm } => {
                    let res = self.read_reg(rs1) | imm;
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::AND { rd, rs1, rs2 } => {
                    let res = self.read_reg(rs1) & self.read_reg(rs2);
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::ANDI { rd, rs1, imm } => {
                    let res = self.read_reg(rs1) & imm;
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::LUI { rd, imm } => {
                    // LUI loads upper 20 bits
                    let res = imm << 12;
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::AUIPC { rd, imm } => {
                    let res = pr.pc as i32 + (imm << 12);
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }

                // Control flow instructions:
                Instruction::JAL { rd, imm } => {
                    let ret = pr.pc as i32 + 4;
                    if rd != 0 {
                        self.registers[rd] = ret;
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
                Instruction::JALR { rd, rs1, imm } => {
                    let ret = pr.pc as i32 + 4;
                    if rd != 0 {
                        self.registers[rd] = ret;
                    }
                    self.pc = ((self.read_reg(rs1) + imm) & !1) as usize;
                    self.branch_taken = true;
                    self.flush_pipeline();
                    self.pipeline.ex_mem = Some(PipelineRegister {
                        instruction: Instruction::NOP,
                        pc: pr.pc,
                        dest: None,
                        result: None,
                    });
                }
                Instruction::BEQ { rs1, rs2, imm } => {
                    if self.read_reg(rs1) == self.read_reg(rs2) {
                        self.pc = (pr.pc as i32 + imm) as usize;
                        self.branch_taken = true;
                        self.flush_pipeline();
                    }
                    self.pipeline.ex_mem = Some(pr);
                }
                Instruction::BNE { rs1, rs2, imm } => {
                    if self.read_reg(rs1) != self.read_reg(rs2) {
                        self.pc = (pr.pc as i32 + imm) as usize;
                        self.branch_taken = true;
                        self.flush_pipeline();
                    }
                    self.pipeline.ex_mem = Some(pr);
                }
                Instruction::BLT { rs1, rs2, imm } => {
                    if self.read_reg(rs1) < self.read_reg(rs2) {
                        self.pc = (pr.pc as i32 + imm) as usize;
                        self.branch_taken = true;
                        self.flush_pipeline();
                    }
                    self.pipeline.ex_mem = Some(pr);
                }
                Instruction::BGE { rs1, rs2, imm } => {
                    if self.read_reg(rs1) >= self.read_reg(rs2) {
                        self.pc = (pr.pc as i32 + imm) as usize;
                        self.branch_taken = true;
                        self.flush_pipeline();
                    }
                    self.pipeline.ex_mem = Some(pr);
                }
                Instruction::BLTU { rs1, rs2, imm } => {
                    if (self.read_reg(rs1) as u32) < (self.read_reg(rs2) as u32) {
                        self.pc = (pr.pc as i32 + imm) as usize;
                        self.branch_taken = true;
                        self.flush_pipeline();
                    }
                    self.pipeline.ex_mem = Some(pr);
                }
                Instruction::BGEU { rs1, rs2, imm } => {
                    if (self.read_reg(rs1) as u32) >= (self.read_reg(rs2) as u32) {
                        self.pc = (pr.pc as i32 + imm) as usize;
                        self.branch_taken = true;
                        self.flush_pipeline();
                    }
                    self.pipeline.ex_mem = Some(pr);
                }

                // Load instructions: compute effective address.
                Instruction::LB { rd, rs1, imm } => {
                    let addr = (self.read_reg(rs1) + imm) as usize;
                    let byte = self.memory[addr] & 0xFF;
                    // Sign-extend 8 bits.
                    let res = ((byte as i8) as i32) as i32;
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::LH { rd, rs1, imm } => {
                    let addr = (self.read_reg(rs1) + imm) as usize;
                    let half = self.memory[addr] & 0xFFFF;
                    let res = ((half as i16) as i32) as i32;
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::LW { rd, rs1, imm } => {
                    let addr = (self.read_reg(rs1) + imm) as usize;
                    let res = self.memory[addr];
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(res)));
                }
                Instruction::LBU { rd, rs1, imm } => {
                    let addr = (self.read_reg(rs1) + imm) as usize;
                    let byte = self.memory[addr] & 0xFF;
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(byte)));
                }
                Instruction::LHU { rd, rs1, imm } => {
                    let addr = (self.read_reg(rs1) + imm) as usize;
                    let half = self.memory[addr] & 0xFFFF;
                    self.pipeline.ex_mem = Some(next_reg(Some(rd), Some(half)));
                }

                // Store instructions: write to memory.
                Instruction::SB { rs1, rs2, imm } => {
                    let addr = (self.read_reg(rs1) + imm) as usize;
                    let val = self.read_reg(rs2) & 0xFF;
                    self.memory[addr] = val;
                    self.pipeline.ex_mem = Some(pr);
                }
                Instruction::SH { rs1, rs2, imm } => {
                    let addr = (self.read_reg(rs1) + imm) as usize;
                    let val = self.read_reg(rs2) & 0xFFFF;
                    self.memory[addr] = val;
                    self.pipeline.ex_mem = Some(pr);
                }
                Instruction::SW { rs1, rs2, imm } => {
                    let addr = (self.read_reg(rs1) + imm) as usize;
                    let val = self.read_reg(rs2);
                    self.memory[addr] = val;
                    self.pipeline.ex_mem = Some(pr);
                }

                // Memory fence instructions (no-op in this simple model)
                Instruction::FENCE { .. } => {
                    self.pipeline.ex_mem = Some(pr);
                }
                Instruction::FENCE_I { .. } => {
                    self.pipeline.ex_mem = Some(pr);
                }

                // Environment instructions: for now, simply halt simulation.
                Instruction::ECALL => {
                    println!("ECALL at PC {} on cycle {}. Halting.", pr.pc, self.cycle_count);
                    self.pc = self.instruction_memory.len();
                    self.pipeline.ex_mem = Some(PipelineRegister {
                        instruction: Instruction::NOP,
                        pc: pr.pc,
                        dest: None,
                        result: None,
                    });
                }
                Instruction::EBREAK => {
                    println!("EBREAK at PC {} on cycle {}. Halting.", pr.pc, self.cycle_count);
                    self.pc = self.instruction_memory.len();
                    self.pipeline.ex_mem = Some(PipelineRegister {
                        instruction: Instruction::NOP,
                        pc: pr.pc,
                        dest: None,
                        result: None,
                    });
                }

                // NOP and default:
                Instruction::NOP => {
                    self.pipeline.ex_mem = Some(pr);
                }
            }
        }
    }

    // Memory access stage: simply forward EX/MEM to MEM/WB.
    fn memory_access(&mut self) {
        self.pipeline.mem_wb = self.pipeline.ex_mem.take();
    }

    // Write-back stage: update registers for instructions that produce a result.
    fn write_back(&mut self) {
        if let Some(pr) = self.pipeline.mem_wb.take() {
            match pr.instruction {
                Instruction::ADD { rd, .. }
                | Instruction::ADDI { rd, .. }
                | Instruction::SUB { rd, .. }
                | Instruction::SLL { rd, .. }
                | Instruction::SLLI { rd, .. }
                | Instruction::SLT { rd, .. }
                | Instruction::SLTI { rd, .. }
                | Instruction::SLTU { rd, .. }
                | Instruction::SLTIU { rd, .. }
                | Instruction::XOR { rd, .. }
                | Instruction::XORI { rd, .. }
                | Instruction::SRL { rd, .. }
                | Instruction::SRLI { rd, .. }
                | Instruction::SRA { rd, .. }
                | Instruction::SRAI { rd, .. }
                | Instruction::OR { rd, .. }
                | Instruction::ORI { rd, .. }
                | Instruction::AND { rd, .. }
                | Instruction::ANDI { rd, .. }
                | Instruction::LUI { rd, .. }
                | Instruction::AUIPC { rd, .. }
                | Instruction::LB { rd, .. }
                | Instruction::LH { rd, .. }
                | Instruction::LW { rd, .. }
                | Instruction::LBU { rd, .. }
                | Instruction::LHU { rd, .. } => {
                    if let Some(result) = pr.result {
                        if rd != 0 {
                            self.registers[rd] = result;
                        }
                    }
                }
                _ => {}
            }
        }
        // Ensure register x0 stays 0.
        self.registers[0] = 0;
    }

    fn flush_pipeline(&mut self) {
        self.pipeline.if_id = None;
        self.pipeline.id_ex = None;
        self.pipeline.ex_mem = None;
        self.pipeline.mem_wb = None;
    }

    // Run loop: advance pipeline stages and update PC.
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

            if self.pc >= self.instruction_memory.len() || self.cycle_count > 200 {
                break;
            }
        }
        println!("Final Register State: {:?}", self.registers);
    }
}
