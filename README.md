# FPSE Project

## Using cli
### Building
1. Run dune build, which should output a folder _build
2. Executable will be named "cli.exe" in _build/default/src/bin
### Running
- ./cli.exe [grid-rows] [grid-columns] will print a grid of 1x1 spaces
- Currently the numbers just wrap around to 0 when it hits double digits, might want to figure out better way of doing this later

