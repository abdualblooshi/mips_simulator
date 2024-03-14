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
        # Logic to set control signals based on the decimal opcode
        if opcode == 0:  # add
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
        elif opcode == 2:  # j
            self.jump = 1
        elif opcode == 3:  # jal
            self.jump = 1
            self.reg_write = 1
        elif opcode == 0:  # jr
            self.jump = 1
        elif opcode == 12:  # andi
            self.alu_src = 1
            self.alu_op = ALUOp.AND.value
            self.reg_write = 1
        elif opcode == 13:  # ori
            self.alu_src = 1
            self.alu_op = ALUOp.OR.value
            self.reg_write = 1
        elif opcode == 4:  # beq
            self.branch = 1
            self.alu_op = ALUOp.SUB.value
        elif opcode == 2:  # j
            self.jump = 1
        elif opcode == 3:  # jal
            self.jump = 1
            self.reg_write = 1
        elif opcode == 0:  # jr
            self.jump = 1
        elif opcode == 42:  # slt
            self.alu_op = ALUOp.SLT.value
            self.reg_write = 1
        elif opcode == 0:  # sll
            self.alu_op = ALUOp.SLL.value
            self.reg_write = 1
        else:
            raise ValueError(f"Unsupported opcode: {opcode}")
        print(f"Control Signals: ALUSrc={self.alu_src}, RegDst={self.reg_dst}, MemRead={self.mem_read}, MemWrite={self.mem_write}, RegWrite={self.reg_write}, Branch={self.branch}, ALUOp={self.alu_op}, MemReg={self.mem_to_reg}")

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

# here we instantiate the control unit class
cu = ControlUnit()

# guys this is an empty dictionary its like an empty arraylist that we will add values to later or like hashmap in java
control_signals = {}