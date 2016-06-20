# SVM

You can try it <a href="https://cdn.rawgit.com/AndreZWei/SVM/master/demo.html">here</a>.

This is a simple virtual machine implemented by Elm.

The Virtual Machine has 8 registers:
<ul>
<li>Register PC is the program counter,
<li>Register PSW is the program status word,
<li>Register RA is the return address register,
<li>Register Zero holds the constant 0,
<li>Registers RO through R3 are general purpose registers.</li>
</ul>

It takes in 17 types of instructions:
<ul>
<li> LOD Rd, offset, Rs: let base be the contents of register Rs. Then this loads RAM[base + offset] into register Rd.
<li>Li Rd, number: loads number into register Rd.
<li>STO Rs, offset, Rd: let base be the contents of register Rd, stores the contents of register Rs into location base + offset in the memory.
<li>MOV Rd, Rs: copies the contents of register Rs into register Rd.
<li>ADD Rd, Rs, Rt: adds the contents of registers Rs and Rt and stores the sum in register Rd.
<li>SUB Rd, Rs, Rt: subtracts the contents of register Rt from Rs and stores the difference in register Rd.
<li>MUL Rd, Rs, Rt: multiplies the contents of register Rt from Rs and stores the product in register Rd.
<li>DIV Rd, Rs, Rt: divides the contents of register Rs by Rt and stores the integer quotient in register Rd.
<li>CMP Rs, Rt: sets PSW = Rs - Rt. Note that if Rs > Rt, then PSW will be positive, if Rs == Rt, then PSW will be 0 and if Rs < Rt, then PSW will be negative.
<li>JSR disp: sets RA = PC and then PC = PC + disp.
<li>R: sets PC = RA.
<li>BLT disp: if PSW is negative, causes the new value of PC to be the sum PC + disp. Note that if disp is negative, this will cause <li>the program to jump backward in the sequence of instructions. If PSW >= 0, this instruction does nothing.
<li>BEQ disp: if PSW == 0, causes the new value of PC to be the sum PC + disp. Note that if disp is negative, this will cause the program to jump backward in the sequence of instructions. If PSW != 0, this instruction does nothing.
<li>BGT disp: if PSW is positive, causes the new value of PC to be the sum PC + disp. Note that if disp is negative, this will cause the program to jump backward in the sequence of instructions. If PSW <= 0, this instruction does nothing.
<li>JMP disp: causes the new value of PC to be the sum PC + disp.
<li>HLT: causes the svm machine to print the contents of registers PC, PSW, RA, R0, R1, R2 and R3. It then stops, returning ().
</ul>

The SVM is currently in its demo version.
