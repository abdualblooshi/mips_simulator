# datapath.py

from control_unit import ControlUnit
from memory import instruction_memory, data_memory
from control_unit import ALUOp

class RegisterFile:
    """
    A class to simulate the register file of the MIPS processor.
    It contains 32 registers each of 32 bits, with register 0 being constant zero.
    """

    def __init__(self):
        self.registers = [0] * 32

    def read(self, reg_number):
        """
        Read a value from a register.
        :param reg_number: Register number to read from
        :return: 32-bit value contained in the register
        """
        self._validate_register_number(reg_number)
        return self.registers[reg_number]

    def write(self, reg_number, data):
        """
        Write a value to a register.
        :param reg_number: Register number to write to
        :param data: 32-bit value to write into the register
        """
        self._validate_register_number(reg_number)
        if reg_number != 0:  # Register 0 is always 0
            self.registers[reg_number] = data

    @staticmethod
    def _validate_register_number(reg_number):
        if not (0 <= reg_number < 32):
            raise ValueError("Register number must be between 0 and 31 inclusive.")

# Expanding the ALU class
class ALU:
    def __init__(self):
        self.result = None
        self.zero = False
    
    def operate(self, operand1, operand2, operation, funct=None):
        # these are the arithmetic operations that the ALU can perform
        # the operation is determined by the control unit
        if operation == ALUOp.ADD.value:
            self.result = operand1 + operand2
        elif operation == ALUOp.SUB.value:
            self.result = operand1 - operand2
        elif operation == ALUOp.AND.value:
            self.result = operand1 & operand2
        elif operation == ALUOp.OR.value:
            self.result = operand1 | operand2
        elif operation == ALUOp.SLT.value:
            self.result = int(operand1 < operand2)
        elif operation == ALUOp.SLL.value:
            self.result = operand1 << operand2
        elif operation == ALUOp.NOR.value:
            self.result = ~(operand1 | operand2)
        else:
            raise ValueError(f"Unsupported ALU operation: {operation}")
        self.zero = (self.result == 0)
    
    def funct_based_operations(self, operand1, operand2, funct):
        if funct == 32:  # add
            return operand1 + operand2
        # Add more mappings from funct codes to ALU operations
        else:
            raise ValueError(f"Unsupported function code: {funct}")

class ProgramCounter:
    def __init__(self):
        self.address = 0

    def update(self, new_address):
        self.address = new_address

class Adder:
    @staticmethod
    def add(value1, value2):
        return value1 + value2

class Multiplexer:
    @staticmethod
    def select(input1, input2, control_signal):
        return input1 if control_signal == 0 else input2

class SignExtend:
    @staticmethod
    def extend(value):
        # Assumes 'value' is 16 bits
        return value if value < 0x8000 else value | 0xFFFF0000 # here we assume 0xFFFF0000 is the sign extension and 0x8000 is the sign bit mask for 16 bits which converts the 16 bit value to 32 bit value
    
        # like when we have a binary number 1111 1111 1111 1111 and we want to convert it to 32 bit number we add zeroes to the left of the number to make it 32 bits
        # 0000 0000 0000 0000 1111 1111 1111 1111

class ShiftLeft2:
    @staticmethod
    def shift(value):
        return value << 2

# Other components you already have (e.g., RegisterFile, ALU) are used here.

def run_cycle():
    pc = ProgramCounter()
    adder = Adder()
    mux = Multiplexer()
    sign_extend = SignExtend()
    shift_left2 = ShiftLeft2()
    reg_file = RegisterFile()
    alu = ALU()
    control_unit = ControlUnit()
    
    while True:  # This loop simulates the clock cycles of the MIPS processor
        # Fetch
        instruction = instruction_memory.read(pc.address)
        pc.update(adder.add(pc.address, 4))  # Increment PC
        
        # Decode
        opcode, rs, rt, rd, shamt, funct, immediate, address = parse_instruction(instruction)
        control_unit.execute(instruction)  # Set control signals
        
        # Execute
        operand1 = reg_file.read(rs)
        operand2 = mux.select(reg_file.read(rt), sign_extend.extend(immediate), control_unit.alu_src)
        alu_control_signal = control_unit.alu_op
        alu.operate(operand1, operand2, alu_control_signal)

        # Memory Access
        if control_unit.mem_read or control_unit.mem_write:
            memory_address = alu.result
            if control_unit.mem_read:
                read_data = data_memory.read(memory_address)
            if control_unit.mem_write:
                data_memory.write(memory_address, reg_file.read(rt))

        # Write Back
        if control_unit.reg_write:
            write_data = mux.select(alu.result, read_data, control_unit.mem_to_reg)
            destination_reg = mux.select(rt, rd, control_unit.reg_dst)
            reg_file.write(destination_reg, write_data)

        # Update PC based on branching
        branch_target = adder.add(pc.address, shift_left2.shift(sign_extend.extend(immediate)))
        pc_update = mux.select(adder.add(pc.address, 4), branch_target, control_unit.branch & alu.zero)
        pc_update = mux.select(pc_update, address, control_unit.jump)
        pc.update(pc_update)

def parse_instruction(instruction_str):
    """Parses a single MIPS assembly instruction."""
    parts = instruction_str.split()  
    mnemonic = parts[0]
    operands = parts[1].split(",")  
    for i in range(len(operands)):  # Convert register names and immediates
        if operands[i].startswith("$"):  # Register
            operands[i] = int(operands[i][1:]) 
        else:  # Immediate
            operands[i] = int(operands[i])  
    return mnemonic, operands
