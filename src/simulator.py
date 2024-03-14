# simulator.py

from memory import instruction_memory, data_memory
from datapath import RegisterFile, ALU, SignExtend, Adder
from control_unit import ControlUnit

immediate = 0x00000000

class Simulator:
    """
    A class that simulates the execution of MIPS instructions.
    """
    # this is used to map the register names to their respective IDs
    inverted_register_mapping = {
        0: "$zero", 1: "$at", 2: "$v0", 3: "$v1",
        4: "$a0", 5: "$a1", 6: "$a2", 7: "$a3",
        8: "$t0", 9: "$t1", 10: "$t2", 11: "$t3",
        12: "$t4", 13: "$t5", 14: "$t6", 15: "$t7",
        16: "$s0", 17: "$s1", 18: "$s2", 19: "$s3",
        20: "$s4", 21: "$s5", 22: "$s6", 23: "$s7",
        24: "$t8", 25: "$t9", 26: "$k0", 27: "$k1",
        28: "$gp", 29: "$sp", 30: "$fp", 31: "$ra"
    }
    
    def __init__(self):
        self.registers = RegisterFile()
        self.alu = ALU()
        self.control_unit = ControlUnit()
        self.pc = 0  # Program counter
        self.data_memory = data_memory   # Store the data_memory reference
        

    def fetch(self):
        """
        Fetch the next instruction from instruction memory.
        """
        instruction = instruction_memory.read(self.pc)
        self.pc += 4  # Increment PC to the next word
        return instruction

    def decode(self, instruction):
        """
        Decode the instruction and update the control unit signals.
        """
        opcode = (instruction >> 26) & 0x3F
        rs = (instruction >> 21) & 0x1F
        rt = (instruction >> 16) & 0x1F
        rd = (instruction >> 11) & 0x1F
        immediate = instruction & 0xFFFF

        # You might need additional fields like these depending on your implementation
        #shamt = (instruction >> 6) & 0x1F
        #funct = instruction & 0x3F

        self.control_unit.set_control_signals(opcode)

    def execute(self, instruction):
        """
        Execute the instruction using the ALU and the control signals.
        """
        # Extract operands 
        rs = (instruction >> 21) & 0x1F
        rt = (instruction >> 16) & 0x1F
        rd = (instruction >> 11) & 0x1F
        immediate = instruction & 0xFFFF

        reg_value1 = self.registers.read(rs)
        reg_value2 = self.registers.read(rt)
        sign_extended_immediate = SignExtend.extend(immediate) 

        # Select correct operands for ALU
        alu_operand1 = reg_value1 
        alu_operand2 = reg_value2 if self.control_unit.alu_src == 0 else sign_extended_immediate
        
        print("ALU Operands:", alu_operand1, alu_operand2) # Add this line
        # Perform ALU Operation
        self.alu.operate(alu_operand1, alu_operand2, self.control_unit.alu_op)
        print("ALU Result:", self.alu.result) # Add this line

        # Perform ALU Operation
        self.alu.operate(alu_operand1, alu_operand2, self.control_unit.alu_op)

        # Handle memory operations
        if self.control_unit.mem_read:
            memory_address = self.alu.result
            data_from_memory = self.data_memory.read(memory_address)
        if self.control_unit.mem_write:
            memory_address = self.alu.result
            data_to_write = reg_value2  
            self.data_memory.write(memory_address, data_to_write)

        if self.control_unit.reg_write:
            destination_reg = rd if self.control_unit.reg_dst else rt  # This depends on your control unit's logic
            write_data = self.alu.result
            # Debug prints
            print(f"RegWrite: {self.control_unit.reg_write}, reg_dst: {self.control_unit.reg_dst}")
            print(f"Register to Write: {self.inverted_register_mapping.get(destination_reg, '$unknown')}, Data to Write: {hex(write_data)}")
            self.registers.write(destination_reg, write_data)



        # Update PC
        if self.control_unit.branch and self.alu.zero:
            pc_update = Adder.add(self.pc, SignExtend.extend(immediate) << 2) # Branch target
        elif self.control_unit.jump:
            jump_address = (instruction & 0x3FFFFFF) << 2  
            upper_pc_bits = (self.pc + 4) & 0xF0000000 
            pc_update = upper_pc_bits | jump_address 
        else:
            pc_update = self.pc + 4 # Normal increment

        self.pc = pc_update 

        if self.control_unit.reg_write:
            # ... 
            print("Register to Write:", destination_reg)  # Add this line
            print("Data to Write:", write_data) # Add this line
            self.registers.write(destination_reg, write_data)
        
    def run(self, instructions):
        """
        Run a list of MIPS instructions.
        :param instructions: A list of instructions in decimal representation.
        """
        for instruction in instructions:
            self.fetch()
            self.decode(instruction)
            self.execute(instruction)
    
def assemble_instruction(instruction_str):
    parts = instruction_str.split(maxsplit=1)
    mnemonic = parts[0]
    operands_str = parts[1] if len(parts) > 1 else ""
    operands = [op.strip() for op in operands_str.split(",")] if operands_str else []

    # Opcode and funct values based on the MIPS instruction set
    instruction_set = {
        "add": {"format": "R", "opcode": 0, "funct": 32},
        "addi": {"format": "I", "opcode": 8},
        "lw": {"format": "I", "opcode": 35},
        "sw": {"format": "I", "opcode": 43},
        "sll": {"format": "R", "opcode": 0, "funct": 0},
        "and": {"format": "R", "opcode": 0, "funct": 36},
        "andi": {"format": "I", "opcode": 12},
        "or": {"format": "R", "opcode": 0, "funct": 37},
        "ori": {"format": "I", "opcode": 13},
        "nor": {"format": "R", "opcode": 0, "funct": 39},
        "beq": {"format": "I", "opcode": 4},
        "j": {"format": "J", "opcode": 2},
        "jal": {"format": "J", "opcode": 3},
        "jr": {"format": "R", "opcode": 0, "funct": 8},
        "slt": {"format": "R", "opcode": 0, "funct": 42},
    }
    
    # register mapping hashmap
    register_mapping = {
        "$zero": 0, "$at": 1, "$v0": 2, "$v1": 3,
        "$a0": 4, "$a1": 5, "$a2": 6, "$a3": 7,
        "$t0": 8, "$t1": 9, "$t2": 10, "$t3": 11,
        "$t4": 12, "$t5": 13, "$t6": 14, "$t7": 15,
        "$s0": 16, "$s1": 17, "$s2": 18, "$s3": 19,
        "$s4": 20, "$s5": 21, "$s6": 22, "$s7": 23,
        "$t8": 24, "$t9": 25, "$k0": 26, "$k1": 27,
        "$gp": 28, "$sp": 29, "$fp": 30, "$ra": 31
    }
    
    inverted_register_mapping = {v: k for k, v in register_mapping.items()}

    instr_details = instruction_set.get(mnemonic)
    if not instr_details:
        raise ValueError(f"Unsupported instruction mnemonic: {mnemonic}")

    opcode = instr_details["opcode"]
    assembled = opcode << 26

    if instr_details["format"] == "R":
        rs, rt, rd = [register_mapping[op] if op in register_mapping else int(op[1:]) for op in operands[:3]]
        shamt = int(operands[3]) if len(operands) > 3 else 0
        funct = instr_details["funct"]
        assembled |= (rs << 21) | (rt << 16) | (rd << 11) | (shamt << 6) | funct
    elif instr_details["format"] == "I":
        if mnemonic in ["lw", "sw"]:  # Handle instructions with offset(base) addressing
            offset_base = operands[1].replace('(', ' ').replace(')', '').split()
            offset = int(offset_base[0])
            base = register_mapping[offset_base[1]]
            rt = register_mapping[operands[0]]
            assembled |= (base << 21) | (rt << 16) | (offset & 0xFFFF)
        else:  # Handle other I-type instructions
            rt = register_mapping[operands[0]] if operands[0] in register_mapping else int(operands[0][1:])
            rs = register_mapping[operands[1]] if operands[1] in register_mapping else int(operands[1][1:])
            immediate = int(operands[2])
            assembled |= (rs << 21) | (rt << 16) | (immediate & 0xFFFF)
    elif instr_details["format"] == "J":
        address = int(operands[0])
        assembled |= (address & 0x03FFFFFF)
    else:
        raise ValueError(f"Unsupported instruction format: {instr_details['format']}")

    return assembled
