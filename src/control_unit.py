# control_unit.py

from enum import Enum

class ALUOp(Enum):
    # this serves as a mapping from the ALUOp field in the instruction to the ALU control lines
    # like hashmaps in java but with a more readable syntax and uses object oriented programming
    ADD = 0
    SUB = 1
    AND = 2
    OR = 3
    SLT = 4
    SLL = 5
    NOR = 6


# Correcting the implementation using decimal values for opcodes and function codes
class ControlUnit:
    """
    A class to simulate the control unit of the MIPS processor.
    It generates control signals based on the current instruction.
    """
    
    def __init__(self):
        self.reg_dst = 0
        self.jump = 0
        self.branch = 0
        self.mem_read = 0
        self.mem_to_reg = 0
        self.alu_op = 0
        self.mem_write = 0
        self.alu_src = 0
        self.reg_write = 0

    def set_control_signals(self, opcode):
        # Reset control signals
        self.reset_control_signals()
        
        # Logic to set control signals based on the decimal opcode
        if opcode == 32:  # add
            self.reg_dst = 1
            self.alu_op = ALUOp.ADD.value
            self.reg_write = 1
        elif opcode == 8:  # addi
            self.alu_src = 1
            self.alu_op = ALUOp.ADD.value
            self.reg_write = 1
        elif opcode == 35:  # lw
            self.mem_read = 1
            self.mem_to_reg = 1
            self.alu_src = 1
            self.reg_write = 1
        elif opcode == 43:  # sw
            self.alu_src = 1
            self.mem_write = 1
        elif opcode == 4:  # beq
            self.branch = 1
            self.alu_op = ALUOp.SUB.value
        # Add other opcodes as necessary

    def reset_control_signals(self):
        """Reset all control signals to their default (0/off) state."""
        self.reg_dst = 0
        self.jump = 0
        self.branch = 0
        self.mem_read = 0
        self.mem_to_reg = 0
        self.alu_op = 0
        self.mem_write = 0
        self.alu_src = 0
        self.reg_write = 0


    def execute(self, instruction):
        """
        Decode the instruction and execute it by setting the control signals.
        :param instruction: The instruction to execute (as a decimal value)
        """
        opcode = instruction >> 26  # Get the opcode
        funct = instruction & 0x3F  # Get the function code for R-type instructions
        self.set_control_signals(opcode, funct)

# Instantiate the control unit
cu = ControlUnit()

# Set up a dictionary to hold the control signals for each instruction
control_signals = {}

# List of instructions with their opcode/function code
instructions = {
    "add": (0, 32),  # R-type, function code 32
    "addi": (8, None),  # I-type, opcode 8
    "lw": (35, None),  # I-type, opcode 35
    "sw": (43, None),  # I-type, opcode 43
    "sll": (0, 0),  # R-type, function code 0
    "slt": (0, 42),  # R-type, function code 42
    "and": (0, 36),  # R-type, function code 36
    "andi": (12, None),  # I-type, opcode 12
    "or": (0, 37),  # R-type, function code 37
    "ori": (13, None),  # I-type, opcode 13
    "nor": (0, 39),  # R-type, function code 39
    "beq": (4, None),  # I-type, opcode 4
    "j": (2, None),  # J-type, opcode 2
    "jal": (3, None),  # J-type, opcode 3
    "jr": (0, 8)  # R-type, function code 8
}