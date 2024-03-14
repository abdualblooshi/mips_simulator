# simulator.py

from memory import instruction_memory, data_memory
from datapath import RegisterFile, ALU, SignExtend, Adder
from control_unit import ControlUnit

immediate = 0x00000000

class Simulator:
    # todo musab
    # make a class that simulates the execution of MIPS instructions basically this will be the driver class for the whole project
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
        self.pc = 0  # program counter
        self.data_memory = data_memory   # store the data_memory reference
        

    def fetch(self):
        instruction = instruction_memory.read(self.pc)
        print(f"Fetching at PC={self.pc}: Instruction={instruction}")
        self.pc += 4  # increment PC to the next word
        return instruction


    def decode(self, instruction):
        opcode = (instruction >> 26) & 0x3F
        rs = (instruction >> 21) & 0x1F
        rt = (instruction >> 16) & 0x1F
        rd = (instruction >> 11) & 0x1F
        print(f"Decoded: Opcode={opcode}, rs={rs} ({self.inverted_register_mapping[rs]}), rt={rt} ({self.inverted_register_mapping[rt]}), rd={rd} ({self.inverted_register_mapping[rd]})")
        self.control_unit.set_control_signals(opcode)


    def execute(self, instruction):
        """
        Execute the instruction using the ALU and the control signals.
        """
        # extract operands 
        rs = (instruction >> 21) & 0x1F
        rt = (instruction >> 16) & 0x1F
        rd = (instruction >> 11) & 0x1F
        immediate = instruction & 0xFFFF

        reg_value1 = self.registers.read(rs)
        reg_value2 = self.registers.read(rt)
        sign_extended_immediate = SignExtend.extend(immediate) 

        # select correct operands for ALU
        alu_operand1 = reg_value1 
        alu_operand2 = reg_value2 if self.control_unit.alu_src == 0 else sign_extended_immediate
        
        print(f"ALU Operation: {self.control_unit.alu_op}")
        
        print("ALU Operands:", alu_operand1, alu_operand2) # Add this line
        # perform ALU Operation
        self.alu.operate(alu_operand1, alu_operand2, self.control_unit.alu_op)
        print("ALU Result:", self.alu.result) # Add this line

        # perform ALU Operation
        self.alu.operate(alu_operand1, alu_operand2, self.control_unit.alu_op)

        # handle memory operations
        if self.control_unit.mem_read:
            memory_address = self.alu.result
            print(f"Memory Read from address {self.alu.result}")
            data_from_memory = self.data_memory.read(memory_address)
            print(f"Data from memory: {data_from_memory}")
        if self.control_unit.mem_write:
            memory_address = self.alu.result
            data_to_write = reg_value2  
            self.data_memory.write(memory_address, data_to_write)
            print(f"Memory Write to address {self.alu.result} with data {reg_value2}")

        if self.control_unit.reg_write:
            destination_reg = rd if self.control_unit.reg_dst else rt  # This depends on your control unit's logic
            write_data = data_from_memory if self.control_unit.mem_to_reg else self.alu.result
            # debug prints
            print(f"RegWrite: {self.control_unit.reg_write}, reg_dst: {self.control_unit.reg_dst}")
            print(f"Register to Write: {self.inverted_register_mapping.get(destination_reg, '$unknown')}, Data to Write: {hex(write_data)}")
            self.registers.write(destination_reg, write_data)

        # update PC
        if self.control_unit.branch and self.alu.zero:
            pc_update = Adder.add(self.pc, SignExtend.extend(immediate) << 2) # Branch target
        elif self.control_unit.jump:
            jump_address = (instruction & 0x3FFFFFF) << 2  
            upper_pc_bits = (self.pc + 4) & 0xF0000000 
            pc_update = upper_pc_bits | jump_address 
        else:
            pc_update = self.pc + 4 # normal increment

        self.pc = pc_update 

        if self.control_unit.reg_write:
            # ... 
            print("Register to Write:", destination_reg)  # add this line
            print("Data to Write:", write_data) # add this line
            self.registers.write(destination_reg, write_data)
            print("\n")
            print("-----------------------------------")
            print("\n")
        
    def run(self, instructions):
        """
        Run a list of MIPS instructions.
        :param instructions: A list of instructions in decimal representation.
        """
        for instruction in instructions:
            self.fetch()
            self.decode(instruction)
            self.execute(instruction)
            
def preprocess_instructions(instructions):
    # todo abdulrahman, essam, ahmed
    # to convert labels to addresses for beq and j instructions
    labels_to_addresses = {}
    pc = 0  # initial program counter value
    processed_instructions = []  # instructions without labels
    
    for instruction in instructions:
        if ':' in instruction:
            # this is a label definition
            label = instruction.replace(':', '').strip()
            labels_to_addresses[label] = pc
        else:
            # this is a regular instruction
            processed_instructions.append(instruction)
            pc += 4  # assuming all instructions are 4 bytes long
    
    return labels_to_addresses, processed_instructions

def assemble_instruction(instruction_str, labels_to_addresses, pc):
    # this function assembles a MIPS instruction from its string representation to its binary representation
    parts = instruction_str.split(maxsplit=1)
    instruct_name = parts[0]
    operands_str = parts[1] if len(parts) > 1 else ""
    operands = [op.strip() for op in operands_str.split(",")] if operands_str else []

    # opcode and funct values based on the MIPS instruction set
    #todo ahmed
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
    # todo essam
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
    
    instr_details = instruction_set.get(instruct_name)
    if not instr_details:
        raise ValueError(f"Unsupported instruction instruct_name: {instruct_name}")

    opcode = instr_details["opcode"]
    assembled = opcode << 26

    if instr_details["format"] == "R":
        if instruct_name in ["sll"]:
            rd = register_mapping[operands[0]]
            rt = register_mapping[operands[1]]
            shamt = int(operands[2])
            funct = instr_details["funct"]
            assembled |= (rt << 16) | (rd << 11) | (shamt << 6) | funct
            print(f"Instruction: {instruct_name}, RD: {operands[0]} ({rd}), RT: {operands[1]} ({rt}), Shamt: {shamt}")
        elif instruct_name in ["jr"]:
            rs = register_mapping[operands[0]]
            funct = instr_details["funct"]
            assembled |= (rs << 21) | funct
            print(f"Instruction: {instruct_name}, RS: {operands[0]} ({rs})")
        elif instruct_name in ["and"]:
            rd = register_mapping[operands[0]]
            rs = register_mapping[operands[1]]
            rt = register_mapping[operands[2]]
            funct = instr_details["funct"]
            assembled |= (rs << 21) | (rt << 16) | (rd << 11) | funct
            print(f"Instruction: {instruct_name}, RD: {operands[0]} ({rd}), RS: {operands[1]} ({rs}), RT: {operands[2]} ({rt})")
        else:
            rd = register_mapping[operands[0]]
            rs = register_mapping[operands[1]]
            rt = register_mapping[operands[2]]
            funct = instr_details["funct"]
            assembled |= (rs << 21) | (rt << 16) | (rd << 11) | funct
            print(f"Instruction: {instruct_name}, RD: {operands[0]} ({rd}), RS: {operands[1]} ({rs}), RT: {operands[2]} ({rt})")
    elif instr_details["format"] == "I":
        if instruct_name in ["sw"]:  # handle instructions with offset(base) addressing
            # split the offset(base) format properly
            offset, base_reg = operands[1].split('(')
            base_reg = base_reg.rstrip(')')  # Remove the closing parenthesis
            offset = int(offset)
            base = register_mapping[base_reg]
            rt = register_mapping[operands[0]]  # For lw and sw, rt is the first operand
            # assemble the instruction with the correct bit placements
            assembled |= (base << 21) | (rt << 16) | (offset & 0xFFFF)
            # debugging print statements
            print(f"Instruction: {instruct_name}, Base: {base_reg} ({base}), RT: {operands[0]} ({rt}), Offset: {offset}")
        elif instruct_name == "beq":
            rs = register_mapping[operands[0].strip()]
            rt = register_mapping[operands[1].strip()]
            label = operands[2].strip()

            if label not in labels_to_addresses:
                raise ValueError(f"Label {label} not found in symbol table")

            target_address = labels_to_addresses[label]
            pc_next = pc + 4
            relative_offset = (target_address - pc_next) // 4

            assembled |= (rs << 21) | (rt << 16) | (relative_offset & 0xFFFF)
            print(f"Instruction: beq, RS: {operands[0]}, RT: {operands[1]}, Label: {label}, Relative Offset: {relative_offset}")
            
        elif instruct_name in ["lw"]:  # Handle instructions with offset(base) addressing
            # split the offset(base) format properly
            offset, base_reg = operands[1].split('(')
            base_reg = base_reg.rstrip(')')
            offset = int(offset)
            base = register_mapping[base_reg]
            rt = register_mapping[operands[0]]
            # assemble the instruction with the correct bit placements
            assembled |= (base << 21) | (rt << 16) | (offset & 0xFFFF)
            # debugging print statements
            print(f"Instruction: {instruct_name}, Base: {base_reg} ({base}), RT: {operands[0]} ({rt}), Offset: {offset}")
        else:  # handle other I-type instructions (like addi, andi, ori, etc.)
            rt = register_mapping[operands[0]]  # The target register
            rs = register_mapping[operands[1]]  # The source register
            # process immediate value; no need to check if it's a register for these types of instructions
            immediate = int(operands[2])
            # assemble the instruction with the correct bit placements
            assembled |= (rs << 21) | (rt << 16) | (immediate & 0xFFFF)
            # debugging print statements
            print(f"Instruction: {instruct_name}, RS: {operands[1]} ({rs}), RT: {operands[0]} ({rt}), Immediate: {immediate}")
    elif instr_details["format"] == "J":
        address = int(operands[0])
        assembled |= (address & 0x03FFFFFF)
    else:
        raise ValueError(f"Unsupported instruction format: {instr_details['format']}")

    print("Assembled (before returning):", assembled)
    return assembled
