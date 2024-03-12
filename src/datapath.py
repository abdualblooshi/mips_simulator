# datapath.py

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

class ALU:
    """
    A class that simulates the Arithmetic Logic Unit (ALU) of the MIPS processor.
    """

    def __init__(self):
        self.result = None
        self.zero = False

    def operate(self, operand1, operand2, operation):
        """
        Perform an ALU operation on two operands.
        :param operand1: First operand
        :param operand2: Second operand
        :param operation: The ALU operation to perform (e.g., 'add', 'sub')
        """
        if operation == 'add':
            self.result = operand1 + operand2
        elif operation == 'sub':
            self.result = operand1 - operand2
        # More operations like AND, OR, etc., will be implemented here.
        else:
            raise ValueError(f"Unsupported ALU operation: {operation}")
        self.zero = (self.result == 0)

# Example usage
reg_file = RegisterFile()
alu = ALU()

# Example operations on the ALU
try:
    reg_file.write(1, 10)  # Writing to register $1
    reg_file.write(2, 20)  # Writing to register $2
    alu.operate(reg_file.read(1), reg_file.read(2), 'add')
    print(f"ALU result: {alu.result}")
except ValueError as e:
    print(f"Error: {e}")
