
![Logo](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/th5xamgrr6se0x5ro4g6.png)


# Neptune - An Intermediate Language for Music Composition on the Commodore 64

Neptune is an intermediate language designed to bridge the gap between high-level programming and direct interaction with the Commodore 64 and its iconic SID chip. It provides a more accessible way to harness the power of this classic machine.

This project was created as part of a fourth-semester semester project at Aalborg University (Copenhagen). Our task was to develop a compiler, and we chose to translate a custom music language into 6502 assembly code. This assembly code can then be executed on the Commodore 64, allowing it to utilize the SID 6581 sound chip to produce authentic 8-bit chiptune music.

Curious to learn more? You can access the complete project report [here](https://github.com/P4-Group/Neptune-Report/blob/main/SW4_Group_8.pdf).

[Click here](http://www.youtube.com/watch?v=EqYkdAdsoK8) below to hear a sample of the Tetris theme, produced by Neptune.

## Table of Contents

* [Neptune - An Intermediate Language for Music Composition on the Commodore 64](#neptune---an-intermediate-language-for-music-composition-on-the-commodore-64)
    * [Features](#features)
    * [Running Locally](#running-locally)
        * [Prerequisites](#prerequisites)
        * [Compiling the Project](#compiling-the-project)
        * [Running Tests](#running-tests)
        * [Command-Line Usage](#command-line-usage)
            * [Essential Usage](#essential-usage)
            * [Optional Flags](#optional-flags)
            * [Examples](#examples)
    * [Code Snippet Sample](#code-snippet-samples)
        * [Incy Wincy Spider](#incy-wincy-spider)
    * [Playing the Music](#playing-the-music)
        * [Using an Emulator (Recommended: Vice)](#using-an-emulator-recommended-vice)
    * [Socials for Project Founders](#socials-for-project-founders)
    * [Acknowledgements](#acknowledgements)
    * [License](#license)

## Features

- Clear Error reporting: Provides line and char number if errors occur.
- 6502 Assembli Compliation: Translate Neptune code into 6502 Assembly ready
- dasm Integration: Integrates with the dasm assembler for easy generation of .prg files for the Commodore 64.
- High-Level Music Language: intuitive and expressive if need be.
- SID Chip Control: Gives simplified control over the Commodore 64's SID Sound chip.



## Running Locally

### Prerequisites

This project relies on the following:

* **OCaml**: Version 4.07.0 or newer.
* **MenhirLib**: Any version.
* **Dune**: Version 3.17 or newer.

For full functionality, you'll also need:

* **dasm**: Ensure the `dasm` executable is in the compilers PATH.

### Compiling the Project

Clone the project

```bash
  git clone hhttps://github.com/P4-Group/Neptune
```

Go to the project directory

```bash
  cd Neptune
```

Install dependencies

```bash
  opam install dune &
  opam install MenhirLib
```

Build the project

```bash
  dune build
```

An executable will be generated in "_build/default/bin/"

### Running Tests

To run tests, run the following command

```bash
  dune runtest
```


### Command-Line Usage

The Neptune compiler is a command-line tool. You **must** provide a source file, and you can add optional flags to modify its behavior.

```bash
./Neptune -s <source_file> [OPTIONS]
```

#### Essential Usage

**Specify Your Source File:** The core of using this compiler is telling it which file to process. You do this with the `-s` flag, followed by the path to your source file.

Example: 
```bash
./Neptune -s my_project/song.nptn
```

#### Optional Flags

These flags can be added individually or combined to get different outputs or behaviors:

| **Flag** | **Description** |
|----------|-----------------|
| `-dasm` | Enable the use of DASM to assemble output. Requires the DASM executable to be available in your system's PATH. |
| `-tgt-ast` | Display the Target Abstract Syntax Tree (AST) that the compiler generates. |
| `-src-ast` | Display the Source Abstract Syntax Tree (AST) as parsed from your input file. |
| `-sym-tab` | Print the compiler's Symbol Table, showing defined symbols and their properties. |
| `-debug` | Turn on debug mode for more detailed output that can help with troubleshooting. |
| `-h` | Show a help message with all available options and exit. |

#### Examples

**Basic Compilation:**
```bash
./Neptune -s song.nptn
```

**Compile and view Source AST:**
```bash
./Neptune -s chiptune.nptn -src-ast
```

**Full debugging with DASM and Symbol Table:**
```bash
./Neptune -s complex_song.nptn -dasm -sym-tab -debug
```

## Neptune Sample Code

More code samples can be found [here](https://github.com/P4-Group/Neptune/tree/main/src_code_samples).

#### What Is Love
```
tempo = 110
timeSignature = (4,4)
standardPitch = 440

sequence melody = { r:2 d:8:4 f:8:4 d:8:4 f:8:4 | r:8 d:8:4 r:4 r:4 d:8:4 f:8:4 | 
                    r:8 d:8:4 r:4 r:4 d:8:4 c:8:4 | r:2 r:8 d:8:4 f:8:4 g:8:4 } 
sequence bass1 = { d:8:3 r:8 d:8:3 r:8 d:8:3 r:16 d:16:3 r:8 e_:8:3 }
sequence bass2 = { c:8:3 r:8 c:8:3 r:8 c:8:3 r:16 c:16:3 r:8 c:8:3 }
sequence mid1 = { b_:8:3 a:8:3 b_:8:3 g:8:3 }
sequence mid2 = { b_:8:3 a:8:3 b_:8:3 f:8:3 }
sequence mid3 = { a:8:3 g:8:3 a:8:3 f:8:3 }

voice1 = [(melody, vPulse)]
voice2 = [(bass1, sawtooth), (bass1, sawtooth), (bass1, sawtooth), (bass2, sawtooth)]
voice3 = [(mid1, triangle), (mid1, triangle), (mid2, triangle), (mid2, triangle), 
          (mid3, triangle), (mid3, triangle), (mid3, triangle), (mid3, triangle)]
```

## Playing the Music

Once you have compiled your Neptune code, you will have a .prg file (e.g., song.prg) in your project directory. This file contains the 6502 assembly code that the Commodore 64 understands. To play your music, you need a Commodore 64 emulator or the actual Commodore 64 hardware.

### Using an Emulator (Recommended: Vice)

We highly recommend using the **Vice** emulator. Here is how to load and play your .prg file:


1.  **Generate your .prg file:** Compile your Neptune source file with the -dasm flag:

    ./Neptune -s your_song.nptn -dasm

    This will create your_song.prg in your current directory.

2.  **Launch Vice:** Start the Vice emulator on your system.

3.  **Load the program:**
    * Go to File > Autostart disk/tape image...
    * Navigate to where your your_song.prg file is saved and select it.

Vice will automatically load and run the program, and you should hear your sweet Nep**tunes**!

### Recommended Settings for VICE

To achieve the most accurate emulation of the SID 6581, and well-produced sound output we recommend the following audio settings in VICE:
* Go to Preferences > Settings > Audio > SID
  
**For UNIX**:
* select the '6581 (reSID)' and 'Resampling' options

**For Windows**:
* select the '6581' and 'Resampling' options


## Project Founders

| Name                  | Socials                                                                                                                                                                                                                                                                                                                        |
| :-------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Emil S. Andersen      | [![linkedin](https://img.shields.io/badge/linkedin-0A66C2?style=for-the-badge&logo=linkedin&logoColor=white)](https://www.linkedin.com/in/emil-andersen-4a92ba2b3/) [![github](https://img.shields.io/badge/github-100000?style=for-the-badge&logo=github&logoColor=white)](https://github.com/emandersen)               |
| Felix B. Lindberg     | [![linkedin](https://img.shields.io/badge/linkedin-0A66C2?style=for-the-badge&logo=linkedin&logoColor=white)](https://www.linkedin.com/in/felix-bjerre-baa3a1150/) [![github](https://img.shields.io/badge/github-100000?style=for-the-badge&logo=github&logoColor=white)](https://github.com/FelixBjerre)                                                                                                                                                                                   |
| Alberte Lohse         | [![github](https://img.shields.io/badge/github-100000?style=for-the-badge&logo=github&logoColor=white)](https://github.com/wefjyl)                                                                                                                                                                                      |
| Nikolaj K. van Gool   | [![linkedin](https://img.shields.io/badge/linkedin-0A66C2?style=for-the-badge&logo=linkedin&logoColor=white)](https://www.linkedin.com/in/nikolaj-van-gool-339b772b2/) [![github](https://img.shields.io/badge/github-100000?style=for-the-badge&logo=github&logoColor=white)](https://github.com/BinGbONg7aau)                                                                                                                                                                                 |
| Cecilie S. Vebner     | [![linkedin](https://img.shields.io/badge/linkedin-0A66C2?style=for-the-badge&logo=linkedin&logoColor=white)](https://www.linkedin.com/in/cecilie-foege-vebner-b898702a6/) [![github](https://img.shields.io/badge/github-100000?style=for-the-badge&logo=github&logoColor=white)](https://github.com/ceci000)                                                                                                                                                                                      |

## Acknowledgements

- [The dasm assembler Project](https://github.com/dasm-assembler/dasm)
- Our Supervisor Léon Gondelman, deepest gratitude for his guidance and support
## License

[GNU General Public License v3.0](https://github.com/P4-Group/Neptune/blob/main/LICENSE)

