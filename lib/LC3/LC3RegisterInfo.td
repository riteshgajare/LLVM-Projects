class LC3Reg<string n> : Register<n> {
  field bits<6> Num;
  let Namespace = "LC3";
}

class LC3CtrlReg<string n>: Register<n> {
  let Namespace = "LC3";
}

// Registers are identified with 6-bit ID numbers.
// R - -bit integer registers
class R<bits<6> num, string n> : LC3Reg<n> {
  let Num = num;
}

// Control Registers
def NZP : LC3CtrlReg<"NZP">;

// Integer registers
def R0 : R< 0, "R0">, DwarfRegNum<[0]>;
def R1 : R< 1, "R1">, DwarfRegNum<[1]>;
def R2 : R< 2, "R2">, DwarfRegNum<[2]>; 
def R3 : R< 3, "R3">, DwarfRegNum<[3]>;    
def R4 : R< 4, "R4">, DwarfRegNum<[4]>;
def R5 : R< 5, "R5">, DwarfRegNum<[5]>;
def R6 : R< 6, "R6">, DwarfRegNum<[6]>;
def R7 : R< 7, "R7">, DwarfRegNum<[7]>;

// Register classes.
//
// The register order should be defined in terms of the preferred
// allocation order...
//
def GPRegs : RegisterClass<"LC3", [i32], 32,
                            (add R0, R1, R2, R3, // general usage
			         R7, // return address         
                                 R4, // global data section
				 R5, // frame pointer 
				 R6 // stack pointer
                                 )>;

def CtrlRegs : RegisterClass<"LC3", [i8], 8, (add NZP)>;
