# simulator.py

from memory import instruction_memory, data_memory
from datapath import RegisterFile, ALU
from control_unit import ControlUnit

class Simulator:
    """
    A class that simulates the execution of MIPS instructions.
    """
    
    def __init__(self):
        self.registers = RegisterFile()
        self.alu = ALU()
        self.control_unit = ControlUnit()
        self.pc = 0  # Program counter

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
        self.control_unit.execute(instruction)
        # Additional decoding logic will be added here to extract the fields from the instruction

    def execute(self, instruction):
        """
        Execute the instruction using the ALU and the control signals.
        """
        # Here we will use the control unit signals to determine the operation and operands
        # This is where you'll implement the logic for different instructions using the ALU and other components

    def run(self, instructions):
        """
        Run a list of MIPS instructions.
        :param instructions: A list of instructions in decimal representation.
        """
        for instruction in instructions:
            instr = self.fetch()
            self.decode(instr)
            self.execute(instr)

        # Output the final state of the simulator
        print("Simulation complete.")
        print("Final register state:")
        for i, val in enumerate(self.registers.registers):
            print(f"Register ${i}: {val}")
        print("Memory state:")
        for i in range(0, len(data_memory.memory), 4):
            print(f"Memory address {i}: {data_memory.read(i)}")

# Example usage:
instructions = [
    # List your instructions as decimal values here
    # Convert from your instruction representation to decimal
]

sim = Simulator()
sim.run(instructions)
