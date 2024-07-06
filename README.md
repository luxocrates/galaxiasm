# Galaxiasm

Galaxiasm is a Haskell-based Z80 assembler/disassembler for building ROM sets
for the 1979 Namco arcade game Galaxian. It includes a number of sample programs
for test ROMs, and, in a future iteration, will include a symbolicated,
relocatable, assembly-language source file equivalent to the game.

## Building Galaxian ROMs

Prerequisites: you’ll need the GHC Haskell compiler (search for `ghcup` if you
don’t have one) and GNU `make`.

To build the assembler/disassembler:

    make

To then build Galaxian ROMs:

    ./galaxiasm asm/(source file)

This will create a set of 2KiB ROM files in the `out` directory, using filenames
appropriate for the `galaxian` MAME ROMset.

## Using the disassembler

To disassemble a Z80 binary file to stdout, run:

    ./galaxiasm -d (binary file)

## Using the assembler

See the `ASSEMBLER.md` file for more information about syntax and usage.

## License

The Haskell sources for the Galaxiasm dis/assembler are in the public domain.
