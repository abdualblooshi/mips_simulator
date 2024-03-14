# memory.py
# todo abdulrahman & essam
class Memory:
    """
    A class that simulates memory for the MIPS simulator.
    Memory is word-addressable and data is accessed in word (4 bytes).
    """
    
    def __init__(self, size):
        # make sure that the memory size is a multiple of 4
        if size % 4 != 0:
            raise ValueError("Memory size must be a multiple of 4.")
        self.memory = bytearray(size) # initialize memory with zeros

    def read(self, address):
        """
        Read a word (4 bytes) from a specified address.
        :param address: byte-addressable memory location
        :return: 32-bit data read from memory
        """
        if address % 4 != 0:
            raise ValueError("Address must be word-aligned.")
        word_address = address // 4
        word = self.memory[4 * word_address: 4 * (word_address + 1)]
        return int.from_bytes(word, byteorder='little')

    def write(self, address, data):
        """
        Write a word (4 bytes) to a specified address.
        :param address: byte-addressable memory location
        :param data: 32-bit data to write to memory
        """
        if address % 4 != 0:
            raise ValueError("Address must be word-aligned.")
        if not (0 <= data < (1 << 32)):
            raise ValueError("Data must be a 32-bit value.")
        word_address = address // 4
        self.memory[4 * word_address: 4 * (word_address + 1)] = data.to_bytes(4, byteorder='little')


# initialize the memory for instructions and data with default size
instruction_memory = Memory(4096)  # 4KB
data_memory = Memory(4096)  # 4KB

