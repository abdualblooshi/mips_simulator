import argparse
from simulator import Simulator, assemble_instruction, preprocess_instructions

def run_from_file(file_path):
    with open(file_path, 'r') as file:
        instructions = [line.strip() for line in file.readlines() if line.strip()]
    run_simulator(instructions)

def run_simulator(instructions):
    # guys we will probably use this to run the simulator, maybe make it run when i press a button on GUI
    simulator = Simulator()
    labels_to_addresses = preprocess_instructions(instructions)
    for instruction_str in instructions:
        machine_code = assemble_instruction(instruction_str, labels_to_addresses, simulator.pc)
        simulator.run([machine_code])
        # Output the state of the registers and memory after running each instruction
        print_register_state(simulator)
        print_memory_state(simulator)

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

def print_register_state(simulator):
    for i, reg in enumerate(simulator.registers.registers):
        # Use the inverted_register_mapping to get the register name from its ID
        reg_name = inverted_register_mapping.get(i, f"$unknown{i}")
        print(f"{reg_name}: {hex(reg)}")

def print_memory_state(simulator):
    # Example: print first few memory locations for simplicity
    for i in range(0, 100, 4):
        data = simulator.data_memory.read(i)
        print(f"Memory at {hex(i)}: {hex(data)}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run MIPS Simulator")
    parser.add_argument("-f", "--file", help="Path to the file containing MIPS instructions")
    args = parser.parse_args()
    
    if args.file:
        run_from_file(args.file)
    else:
        print("Please provide a file containing MIPS instructions using the -f option.")
