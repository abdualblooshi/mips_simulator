# simulator.py

from memory import instruction_memory, data_memory
from datapath import RegisterFile, ALU, SignExtend, Adder
from control_unit import ControlUnit

immediate = 0x00000000

class Simulator:
    """
    A class that simulates the execution of MIPS instructions.
    """
    
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
            # rt is the destination register for I-type instructions
            # rd is the destination register for R-type instructions
            destination_reg = rt if self.control_unit.reg_dst == 0 else rd
            write_data = self.alu.result  
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

    instr_details = instruction_set.get(mnemonic)
    if not instr_details:
        raise ValueError(f"Unsupported instruction mnemonic: {mnemonic}")

    opcode = instr_details["opcode"]
    assembled = opcode << 26

    if instr_details["format"] == "R":
        rs, rt, rd = [int(op[1:]) for op in operands[:3]]
        shamt = int(operands[3]) if len(operands) > 3 else 0
        funct = instr_details["funct"]
        assembled |= (rs << 21) | (rt << 16) | (rd << 11) | (shamt << 6) | funct
    elif instr_details["format"] == "I":
        rs, rt, immediate = [int(op[1:]) if '$' in op else int(op) for op in operands]
        assembled |= (rs << 21) | (rt << 16) | (immediate & 0xFFFF)
    elif instr_details["format"] == "J":
        address = int(operands[0])
        assembled |= (address & 0x03FFFFFF)
    else:
        raise ValueError(f"Unsupported instruction format: {instr_details['format']}")

    return assembled
