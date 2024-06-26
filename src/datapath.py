# datapath.py
# todo musab

from control_unit import ControlUnit
from memory import instruction_memory, data_memory
from control_unit import ALUOp

class RegisterFile:
    """
    this is a class that simulates the register file in the MIPS processor
    """

    def __init__(self):
        self.registers = [0] * 32 # 32 registers in the MIPS processor

    def read(self, reg_number):
        """
        read a value from a register, the register number is passed as an argument to the function
        """
        self._validate_register_number(reg_number)
        return self.registers[reg_number]

    def write(self, reg_number, data):
        """
        write a value to a register, the register number and the data to be written are passed as arguments to the function
        """
        self._validate_register_number(reg_number)
        if reg_number != 0:  # register 0 is always 0
            self.registers[reg_number] = data

    @staticmethod
    def _validate_register_number(reg_number):
        if not (0 <= reg_number < 32):
            raise ValueError("Register number must be between 0 and 31 inclusive.")

# expanding the ALU class
class ALU:
    #todo abdulrahman
    def __init__(self):
        self.result = None
        self.zero = False
    
    def operate(self, operand1, operand2, operation):
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

class ProgramCounter:
    #todo ahmed
    def __init__(self):
        self.address = 0

    def update(self, new_address):
        self.address = new_address

class Adder:
    #todo ahmed
    @staticmethod
    def add(value1, value2):
        return value1 + value2

class Multiplexer:
    #todo essam
    @staticmethod
    def select(input1, input2, control_signal):
        return input1 if control_signal == 0 else input2

class SignExtend:
    #todo musab
    @staticmethod
    def extend(value):
        # assumes 'value' is 16 bits
        return value if value < 0x8000 else value | 0xFFFF0000 # here we assume 0xFFFF0000 is the sign extension and 0x8000 is the sign bit mask for 16 bits which converts the 16 bit value to 32 bit value
    
        # like when we have a binary number 1111 1111 1111 1111 and we want to convert it to 32 bit number we add zeroes to the left of the number to make it 32 bits
        # 0000 0000 0000 0000 1111 1111 1111 1111

class ShiftLeft2:
    #todo abdulrahman
    @staticmethod
    def shift(value):
        return value << 2

# other components you already have (e.g., RegisterFile, ALU) are used here.

def run_cycle():
    pc = ProgramCounter()
    adder = Adder()
    mux = Multiplexer()
    sign_extend = SignExtend()
    shift_left2 = ShiftLeft2()
    reg_file = RegisterFile()
    alu = ALU()
    control_unit = ControlUnit()
    
    
    # basically it goes through the phases of the MIPS processor the ones that dr hend explained in the lectures
    # FETCH -> DECODE -> EXECUTE -> MEMORY ACCESS -> WRITE BACK
    
    while True:  # this loop simulates the clock cycles of the MIPS processor
        # fetch
        # todo abdulrahman
        instruction = instruction_memory.read(pc.address)
        pc.update(adder.add(pc.address, 4))
        
        # decode
        # todo musab
        opcode = (instruction & 0xFC000000) >> 26
        rs = (instruction & 0x03E00000) >> 21
        rt = (instruction & 0x001F0000) >> 16
        rd = (instruction & 0x0000F800) >> 11
        shamt = (instruction & 0x000007C0) >> 6
        funct = instruction & 0x0000003F
        immediate = instruction & 0x0000FFFF
        address = instruction & 0x03FFFFFF
        
        # execute
        # todo ahmed
        reg_value1 = reg_file.read(rs)
        reg_value2 = reg_file.read(rt)
        sign_extended_immediate = sign_extend.extend(immediate)
        
        alu_operand1 = reg_value1
        alu_operand2 = reg_value2 if control_unit.alu_src == 0 else sign_extended_immediate
        
        alu.operate(alu_operand1, alu_operand2, control_unit.alu_op)
        
        #  memory access
        # todo essam
        if control_unit.mem_read:
            memory_address = alu.result
            data_memory.read(memory_address)
        if control_unit.mem_write:
            memory_address = alu.result
            data_to_write = reg_value2
            data_memory.write(memory_address, data_to_write)
        
        #  write back
        # todo ahmed
        if control_unit.reg_write:
            destination_reg = rd if control_unit.reg_dst else rt
            write_data = alu.result
            reg_file.write(destination_reg, write_data)
        
        # update PC
        # todo abdulrahman
        if control_unit.branch and alu.zero:
            pc.update(adder.add(pc.address, shift_left2.shift(sign_extended_immediate)))
        elif control_unit.jump:
            pc.update(mux.select(pc.address, shift_left2.shift(address), 1))
        else:
            pc.update(adder.add(pc.address, 4))
            

if __name__ == "__main__":
    run_cycle()