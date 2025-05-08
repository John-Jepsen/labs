# Local Development Setup

This guide will help you set up a local COBOL development environment using GnuCOBOL, a free and open-source COBOL compiler that allows you to develop and test COBOL programs without access to a mainframe.

## Installing GnuCOBOL

### macOS

```bash
# Using Homebrew
brew install gnucobol
```

### Linux (Ubuntu/Debian)

```bash
sudo apt-get update
sudo apt-get install gnucobol
```

### Windows (WSL)

```bash
# First, install WSL and Ubuntu from Microsoft Store
# Then run:
sudo apt-get update
sudo apt-get install gnucobol
```

### Building from Source

If you need the latest version or have specific requirements:

```bash
# Download and extract GnuCOBOL source
wget https://ftp.gnu.org/gnu/gnucobol/gnucobol-3.1.2.tar.xz
tar xf gnucobol-3.1.2.tar.xz
cd gnucobol-3.1.2

# Configure and build
./configure
make
sudo make install
```

## Verifying Installation

After installation, verify GnuCOBOL is properly installed:

```bash
cobc --version
```

You should see output similar to:

```
GnuCOBOL 3.1.2
```

## Setting Up Your Development Environment

### VS Code Setup

1. Install VS Code from [code.visualstudio.com](https://code.visualstudio.com)
2. Install the "COBOL" extension by Broadcom
3. Configure the extension:
   - Open VS Code settings
   - Search for "COBOL"
   - Set the compiler path to your GnuCOBOL installation

### Project Structure

Create a basic project structure:

```
cobol-project/
├── src/
│   └── programs/
├── data/
├── lib/
└── build/
```

## Your First COBOL Program

Create a file named `hello.cob` in your `src/programs` directory:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       PROCEDURE DIVISION.
           DISPLAY "Hello, COBOL World!".
           STOP RUN.
```

### Compiling and Running

```bash
# Compile
cobc -x hello.cob

# Run
./hello
```

## Common Compiler Options

- `-x`: Create an executable
- `-free`: Use free format (not fixed column)
- `-std=cobol85`: Use COBOL-85 standard
- `-Wall`: Enable all warnings
- `-debug`: Include debug information

## Best Practices

1. **File Organization**

   - Keep source files in `src/`
   - Store data files in `data/`
   - Place libraries in `lib/`
   - Output executables to `build/`

2. **Naming Conventions**

   - Use `.cob` extension for source files
   - Use meaningful program IDs
   - Follow consistent naming patterns

3. **Version Control**

   - Initialize git repository
   - Create `.gitignore`:
     ```
     build/
     *.exe
     *.o
     ```

4. **Documentation**
   - Include program documentation in comments
   - Document data structures
   - Explain complex logic

## Troubleshooting

### Common Issues

1. **Compiler Not Found**

   - Check PATH environment variable
   - Verify installation
   - Try reinstalling

2. **Permission Denied**

   - Check file permissions
   - Use `chmod` to set proper permissions

3. **Compilation Errors**
   - Check column alignment
   - Verify syntax
   - Review compiler messages

## Next Steps

Now that you have your environment set up, proceed to [COBOL Fundamentals](02-cobol-fundamentals.md) to learn about the language structure and basic concepts.

## Additional Resources

- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/guides.html)
- [VS Code COBOL Extension](https://marketplace.visualstudio.com/items?itemName=broadcomMFD.cobol-language-support)
- [COBOL Programming Guide](https://www.tutorialspoint.com/cobol/index.htm)
