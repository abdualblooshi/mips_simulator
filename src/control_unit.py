# control_unit.py

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
        """
        Set the control signals based on the opcode of the instruction.
        :param opcode: The opcode part of the instruction (as a decimal value)
        """
        # Reset control signals
        self.reg_dst = 0
        self.jump = 0
        self.branch = 0
        self.mem_read = 0
        self.mem_to_reg = 0
        self.alu_op = 0
        self.mem_write = 0
        self.alu_src = 0
        self.reg_write = 0

        if opcode == 0:  # R-type instruction
            self.reg_dst = 1
            self.reg_write = 1
            self.alu_op = 2  # ALU operation for add, sub, etc.
        elif opcode == 35:  # lw instruction
            self.mem_to_reg = 1
            self.reg_write = 1
            self.mem_read = 1
            self.alu_src = 1
            self.alu_op = 0  # ALU operation for add (to compute address)
        elif opcode == 43:  # sw instruction
            self.alu_src = 1
            self.mem_write = 1
            self.alu_op = 0  # ALU operation for add (to compute address)
        elif opcode == 4:  # beq instruction
            self.branch = 1
            self.alu_op = 1  # ALU operation for subtract (to compare values)
        # Additional control signal settings for other instructions will go here

    def execute(self, instruction):
        """
        Decode the instruction and execute it by setting the control signals.
        :param instruction: The instruction to execute (as a decimal value)
        """
        opcode = instruction >> 26  # Get the opcode from the instruction
        self.set_control_signals(opcode)

# Example usage
cu = ControlUnit()
instruction = 0x8C130004  # Example instruction in decimal
cu.execute(instruction)
print(f"Control signals: reg_dst={cu.reg_dst}, jump={cu.jump}, branch={cu.branch}, mem_read={cu.mem_read}, mem_to_reg={cu.mem_to_reg}, alu_op={cu.alu_op}, mem_write={cu.mem_write}, alu_src={cu.alu_src}, reg_write={cu.reg_write}")
