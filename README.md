# MIPS Processor Simulator

Commit history is pretty weird due to working on Live Share 💀, I made this during my <b>Computer Architecture</b> course in British University in Dubai

This MIPS Simulator is a Python-based project designed to simulate the execution of MIPS assembly instructions. It includes a basic graphical user interface (GUI) for easier interaction, making it suitable for educational purposes, such as learning about MIPS architecture or testing simple MIPS programs.

## Project Structure

The project is organized as follows:

- `src/`: Main source code directory.
  - `control_unit.py`: Implements the control unit logic, generating control signals based on the current instruction.
  - `datapath.py`: Handles the datapath components like the ALU, register file, etc.
  - `debug.py`: Provides functionality to run MIPS instructions from a file for debugging purposes.
  - `gui.py`: Contains the graphical user interface code for interacting with the simulator.
  - `memory.py`: Simulates word-addressable memory for instructions and data.
  - `simulator.py`: Coordinates the simulation of MIPS instructions, tying together the control unit, datapath, and memory.
  - `examples/`: Contains example MIPS instruction files.
    - `memory_tests.txt`
    - `r_type.txt`
- `__pycache__/`: Python cache directory (automatically generated).

## Setup

To run the MIPS Simulator, you will need Python installed on your machine. The project has been tested with Python 3.10 but should be compatible with other Python 3 versions.

1. Clone the repository or download the source code.
2. Navigate to the project directory.
3. Run the GUI version of the simulator:

```bash
python src/gui.py
```

4. Alternatively, you can run the simulator from the command line with a MIPS instruction file:

## Usage

```bash
python src/debug.py src/examples/r_type.txt
```



