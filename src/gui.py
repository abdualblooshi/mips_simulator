import tkinter as tk
from tkinter import scrolledtext
from tkinter import messagebox

class MIPS_GUI:
    def __init__(self, master):
        self.master = master
        master.title("MIPS Simulator")

        self.dark_mode = True
        self.setup_ui()
        self.apply_theme()

    def setup_ui(self):
        # Create a menu bar
        self.menu_bar = tk.Menu(self.master)
        self.file_menu = tk.Menu(self.menu_bar, tearoff=0)
        self.file_menu.add_command(label="Toggle Dark Mode", command=self.toggle_dark_mode)
        self.file_menu.add_separator()
        self.file_menu.add_command(label="Exit", command=self.exit_app)
        self.menu_bar.add_cascade(label="File", menu=self.file_menu)
        self.master.config(menu=self.menu_bar)

        # Code editor
        self.code_editor = scrolledtext.ScrolledText(self.master, undo=True, height=20, width=60)
        self.code_editor.grid(row=0, column=0, pady=10, padx=10, sticky="nsew")

        # Run button
        self.run_btn = tk.Button(self.master, text="Run", command=self.run_simulation)
        self.run_btn.grid(row=1, column=0, pady=10, sticky="ew")

        # Register states display
        self.registers_display = scrolledtext.ScrolledText(self.master, height=10, width=30)
        self.registers_display.grid(row=0, column=1, padx=10, pady=10, sticky="nsew")
        self.registers_display.insert(tk.INSERT, "Registers\n")
        self.registers_display.configure(state='disabled')

        # Memory contents display
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
        # Placeholder for simulation logic
        messagebox.showinfo("Simulation", "Simulation started... This is a placeholder.")
        # Update displays here using real simulation data
        self.update_registers_display({"$t0": "0x00", "$t1": "0x01"})
        self.update_memory_display({"0x10010000": "0xFF"})

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
