# 🐩 Poodle Programming Language

[![Poodle Video](https://img.youtube.com/vi/odOh4-f9qrM/maxresdefault.jpg)](https://youtu.be/odOh4-f9qrM)
*Click on the image to watch the demo video*

## 📋 Overview

Poodle is a statically-typed new programming language designed for it ease of use by combining the best features of C++, python and others . It features a clean syntax with support for variables, control structures, loops, and more.

## ✨ Features

- **Static typing** with support for `int`, `float`, `string`, and `bool` data types
- **Control structures** including `if`, `if-else`, and `if-elseif-else`
- **Loop structures** - `for`, compact `for` (Python-style), and `while` loops
- **Expressions** with standard arithmetic operators and precedence rules
- **Boolean operators** - `&&` (AND), `||` (OR), and `!!` (NOT)
- **Comparison operators** - `==`, `<`, `>`, `<=`, `>=`, `!!=`
- **Ternary expressions** - `(condition ? expr1 : expr2)`

## 🛠️ Project Structure

```
SER502-Poodle-Team23/
├── doc/
│   └── 502-PROJECT.pptx                         # Presentation doc
|   └── Contribution.txt                         # Team member contributions
|   └── SER502-Team23-project-description.pdf    # In depth, feature and working of our programming language           
├── src/
│   ├── Lexer.py               # Lexical analyzer
│   ├── interpreter.pl         # Interpreter for execution
│   ├── mainFile.pl            # Main program runner
│   ├── parser.pl              # Parser for syntax analysis
│   ├── tokenReader.pl         # Token file reader
│   ├── testprog.poo           # Sample program
│   └── testprog.pootokens     # Generated tokens
└── README.md                  # Project documentation
```

## 🔍 How It Works

1. **Lexical Analysis**: The lexer (`Lexer.py`) converts source code into tokens
2. **Parsing**: The parser (`parser.pl`) validates syntax and generates a parse tree
3. **Interpretation**: The interpreter (`interpreter.pl`) executes the parse tree

## 📥 Installation Requirements

1. **SWI-Prolog**: Required for interpretation
   - Download from [SWI-Prolog website](https://www.swi-prolog.org/download/stable)

2. **Python with PLY (Python Lex-Yacc)**:
   ```bash
   python3 -m pip install ply
   ```

## 🚀 Running Poodle Programs

### Method 1: Command Line

```bash
# Step 1: Generate tokens
python3 Lexer.py testprog.poo

# Step 2: Run the program through the interpreter
swipl -g "main_prog('testprog.pootokens')" mainFile.pl
```

### Method 2: Using VIM (for development)

1. Configure VIM for .poo files:
   ```bash
   # Open .vimrc
   vim ~/.vimrc
   
   # Add this code
   augroup imp_ft
   au!
   autocmd BufNewFile,BufRead *.imp set filetype=imp
   augroup END
   ```

2. Create `~/.vim/ftplugin/imp.vim` with:
   ```vim
   nnoremap <F10> :w<CR>:!python Lexer.py --evaluate %<CR>
   ```

3. Create and edit Poodle files:
   ```bash
   touch newfile.poo
   vim newfile.poo
   ```

4. Press F10 to compile and run the program

## 📝 Sample Code

```
int x = 5;
bool isValid = true;
string message = "Hello, Poodle!";

if (x > 3) {
    print>>"x is greater than 3";
    
    if (isValid) {
        print>>"and isValid is true";
    }
}

print>>"Loop demo:";
for (int i = 0; i < 3; ++i) {
    print>>i;
}

print>>"Compact loop:";
for j in range (1;5) {
    print>>j;
}
```

## ✨ Contributors

- Shloka Manish Pandya - Interpreter and Token Reader
- Vipsa Kamani - Parser
- Malavika Anand - Lexer and Token Reader

## 🙏 Acknowledgments

- Dr. Ajay Bansal
