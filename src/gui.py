# gui.py
# todo ahmed 

import tkinter as tk
from tkinter import ttk  # import ttk for the Combobox 
from tkinter import scrolledtext
from tkinter import messagebox
from tkinter import filedialog

from simulator import Simulator, assemble_instruction, preprocess_instructions

class MIPS_GUI:
    def __init__(self, master):
        self.master = master
        master.title("MIPS Simulator")

        self.simulator = Simulator()  # instantiate the simulator
        self.dark_mode = True
        self.setup_ui()
        self.apply_theme()
        
    def open_file(self):
        file_path = filedialog.askopenfilename(filetypes=[("Text files", "*.txt"), ("All files", "*.*")])
        if file_path:
            try:
                with open(file_path, 'r') as file:
                    code = file.read()
                    self.code_editor.delete("1.0", tk.END)
                    self.code_editor.insert(tk.INSERT, code)
            except Exception as e:
                messagebox.showerror("Open File", f"Failed to read file\n{str(e)}")

        

    def setup_ui(self):
        # open file button
        self.menu_bar = tk.Menu(self.master)
        self.file_menu = tk.Menu(self.menu_bar, tearoff=0)
        self.file_menu.add_command(label="Open", command=self.open_file)
        self.file_menu.add_command(label="Toggle Dark Mode", command=self.toggle_dark_mode)
        self.file_menu.add_separator()
        self.file_menu.add_command(label="Exit", command=self.exit_app)
        self.menu_bar.add_cascade(label="File", menu=self.file_menu)
        self.master.config(menu=self.menu_bar)

        # code editor
        self.code_editor = scrolledtext.ScrolledText(self.master, undo=True, height=20, width=60)
        self.code_editor.grid(row=0, column=0, pady=10, padx=10, sticky="nsew")
        
        instructions_label = tk.Label(self.master, text="Instructions:\n"
                                                "  * Press run to load the registers and memory\n")       
        instructions_label.grid(row=2, column=0) # Place this below your code editor 

        # run button
        self.run_btn = tk.Button(self.master, text="Run", command=self.run_simulation)
        self.run_btn.grid(row=1, column=0, pady=10, sticky="ew")

        # register states display
        self.registers_display = scrolledtext.ScrolledText(self.master, height=10, width=30)
        self.registers_display.grid(row=0, column=1, padx=10, pady=10, sticky="nsew")
        self.registers_display.insert(tk.INSERT, "Registers\n")
        self.registers_display.configure(state='disabled')

        # memory contents display
        self.memory_display = scrolledtext.ScrolledText(self.master, height=10, width=30)
        self.memory_display.grid(row=1, column=1, padx=10, pady=10, sticky="nsew")
        self.memory_display.insert(tk.INSERT, "Memory\n")
        self.memory_display.configure(state='disabled')

    def toggle_dark_mode(self):
        self.dark_mode = not self.dark_mode
        self.apply_theme()

    def apply_theme(self):
        themes = {
            "dark": {
                "bg": "#1e1e1e", "fg": "#d4d4d4", "insert_bg": "white",
                "btn_bg": "#333333"
            },
            "light": {
                "bg": "#ffffff", "fg": "#000000", "insert_bg": "black",
                "btn_bg": "#f0f0f0"
            }
        }
        theme = themes["dark"] if self.dark_mode else themes["light"]

        self.master.config(bg=theme["bg"])
        self.code_editor.config(bg=theme["bg"], fg=theme["fg"], insertbackground=theme["insert_bg"])
        self.run_btn.config(bg=theme["btn_bg"], fg=theme["fg"])
        self.registers_display.config(bg=theme["bg"], fg=theme["fg"])
        self.memory_display.config(bg=theme["bg"], fg=theme["fg"])

    def run_simulation(self):
        code = self.code_editor.get("1.0", tk.END).strip()  
        instructions = code.splitlines()  

        machine_code_instructions = []
        labels_to_addresses = preprocess_instructions(instructions)
        for instruction_str in instructions:
            # pass the instruction string to the assemble_instruction function to get the machine code and the pc
            machine_code = assemble_instruction(instruction_str, self.simulator.pc, labels_to_addresses)
            machine_code_instructions.append(machine_code)

        try:
            self.simulator.run(machine_code_instructions) 
            self.update_gui_from_simulator()  
        except Exception as e:  
            messagebox.showerror("Simulation Error", str(e))
            
    def update_gui_from_simulator(self):
        
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
        
        # access register values from your simulator and map them to their names
        registers = {inverted_register_mapping.get(i, f"$unknown{i}"): hex(val) 
                    for i, val in enumerate(self.simulator.registers.registers)} 
        self.update_registers_display(registers)

        # access memory contents from your simulator
        memory = {}
        for i in range(0, len(self.simulator.data_memory.memory), 4):
            address = hex(i)
            word = int.from_bytes(self.simulator.data_memory.memory[i:i+4], byteorder='little')
            memory[address] = hex(word)
        self.update_memory_display(memory)


            
    def update_registers_display(self, registers):
        self.registers_display.configure(state='normal')
        self.registers_display.delete("1.0", tk.END)
        self.registers_display.insert(tk.INSERT, "Registers\n")
        for reg, val in registers.items():
            self.registers_display.insert(tk.INSERT, f"{reg}: {val}\n")
        self.registers_display.configure(state='disabled')

    def update_memory_display(self, memory):
        self.memory_display.configure(state='normal')
        self.memory_display.delete("1.0", tk.END)
        self.memory_display.insert(tk.INSERT, "Memory\n")
        for address, val in memory.items():
            self.memory_display.insert(tk.INSERT, f"{address}: {val}\n")
        self.memory_display.configure(state='disabled')

    def exit_app(self):
        self.master.quit()

if __name__ == "__main__":
    root = tk.Tk()
    gui = MIPS_GUI(root)
    root.mainloop()




