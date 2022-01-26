# Register Machine

## Source files

The first line specifies the **initial configuration** of the program:

`(l, r0, r1, ..., rn)`

where
- `l` is the initial instruction number
- `ri` is the initial value of register `i` (a natural number)

Subsequent lines specify the program instructions. Each line is of the form `l: <body>`, where
- `l` is the label of that instruction (a natural number)
- `<body>` is one of the three instruction types below

body | instruction
-----|------------
`ADD r, l` | Add 1 to the contents of register `r`, and jump to instruction labelled `l`
`SUB r, l1, l2` | If register `r` > 0, subtract 1 from the contents of register `r` and jump to instruction `l1`; else do nothing and jump to instruction `l2`
`HALT` | Stop program execution

## Compiling

Make sure `ocamlbuild` is installed.

Compile the machine with `make`.

To clean up, run `make clean`.

## Usage

Usage: `machine.byte <file>`
