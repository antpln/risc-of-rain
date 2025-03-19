# RISC-V CPU Simulator

This repository contains a simplified RISC-V CPU simulator written in Rust. The simulator models a pipelined CPU with basic data forwarding and hazard detection (including stalling) to handle instruction dependencies without requiring manual insertion of bubbles.

## Features

- **Pipelined Simulation:** Implements the standard pipeline stages:
  - **Fetch**
  - **Decode**
  - **Execute**
  - **Memory Access**
  - **Write-back**
- **Data Forwarding:** Automatically forwards computed results from later stages to earlier stages so that dependent instructions get up-to-date values.
- **Hazard Detection & Stalling:** Detects hazards in the decode stage and stalls the pipeline when needed to ensure that branch instructions evaluate correct operands.
- **Subset of RISC-V Instructions:** Supports key arithmetic instructions (e.g., `ADD`, `ADDI`), control flow instructions (e.g., `BEQ`, `JAL`, `JALR`), and system instructions (`NOP`).
- **Example Programs:** Contains sample programs (such as a multiplication program) to demonstrate and test the simulator.
