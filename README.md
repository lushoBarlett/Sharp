# Philosophy

Problem: The machine likes less stuff, but the prgrammer likes more stuff. Every higher level concept can be implemented with lower level concepts. We want the programmer to be able to know and use ideas freely, but let the final product be simple and fast.

Answer: syntax sugar

The concepts that the interpreter / compiler will have to know are very few, but the tools that the programmer is provided to interface with those concepts are many. This simplifies the backend code while letting the programmer go wild with the expression of their ideas.

# Crisp

### Registers

* $ip is the instruction pointer
* $flags are the flags (like Zero, Overflow, Carry, etc)
* $sp is the stack pointer
* $bp is the base pointer, for stack management

### Directives

* .data is for constants
* .text is for the program

### Tags

Tags are just names for memory addresses, so they are treated as such. Dereferencing memory addresses can be done with parentheses.

### Operation Syntax

Operations have suffixes based on what is operated on. 1, 2, 4 and 8 are for integer operations of that amount of bytes. f and d are float and double, meaning single and double precision floating point numbers.

Most binary operations are of the form
```
op <dst> <src1> <src2>
```
and every single one can be a memory access, for example

```
...
add4 $sp $sp 4
add4 ($sp) -4($bp) -8($bp)
```