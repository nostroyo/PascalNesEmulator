unit UImpl6502;

interface
uses
  Generics.Collections
  , UIntBus
  , UInt6502;
type

  TUint8Func = function: UInt8 of object;
  
  T6502Flags = (
    cfCarry = 1 shl 0,
    cfZero = 1 shl 1,
    cfDisableInterrupts = 1 shl 2,
    cfDecimalMode = 1 shl 3, // unused
    cfBreak = 1 shl 4,
    cfUnused = 1 shl 5,
    cfOverflow = 1 shl 6,
    cfNegative = 1 shl 7
    );

  T6502Opcode = record
    Name: string;
    OperateFunc: TUint8Func;
    AddressModeFunc: TUint8Func;
    TotalCycle: UInt8;

    constructor Create(const AName: string; AOpFunc, AAddFunc: TUint8Func; ATotalCycle: UInt8);
  end;

  T6502 = class(TInterfacedObject, I6502)
  private
    [weak] FBus: IBus;

    FStatus: UInt8;

    Fa, Fx, Fy, FStckP: UInt8;
    FPc: UInt16;

    FFetched: UInt8;
    FAbsoluteAddress, FRelativAddress: UInt16;
    FCurrentOpcode, FRemainningCycleForCurrOpcd: UInt8;

    // adressing mode
    function IMP(): UInt8;	function IMM(): UInt8;
	  function ZP0(): UInt8;	function ZPX(): UInt8;
	  function ZPY(): UInt8;	function REL(): UInt8;
	  function ABS(): UInt8;	function ABX(): UInt8;
	  function ABY(): UInt8;	function IND(): UInt8;
	  function IZX(): UInt8;	function IZY(): UInt8;

    // opcode
    function ADC(): UInt8;	function &AND(): UInt8;	function ASL(): UInt8;	function BCC(): UInt8;
    function BCS(): UInt8;	function BEQ(): UInt8;	function BIT(): UInt8;	function BMI(): UInt8;
    function BNE(): UInt8;	function BPL(): UInt8;	function BRK(): UInt8;	function BVC(): UInt8;
    function BVS(): UInt8;	function CLC(): UInt8;	function CLD(): UInt8;	function CLI(): UInt8;
    function CLV(): UInt8;	function CMP(): UInt8;	function CPX(): UInt8;	function CPY(): UInt8;
    function DEC(): UInt8;	function DEX(): UInt8;	function DEY(): UInt8;	function EOR(): UInt8;
    function Inc(): UInt8;	function INX(): UInt8;	function INY(): UInt8;	function JMP(): UInt8;
    function JSR(): UInt8;	function LDA(): UInt8;	function LDX(): UInt8;	function LDY(): UInt8;
    function LSR(): UInt8;	function NOP(): UInt8;	function ORA(): UInt8;	function PHA(): UInt8;
    function PHP(): UInt8;	function PLA(): UInt8;	function PLP(): UInt8;	function ROL(): UInt8;
    function ROR(): UInt8;	function RTI(): UInt8;	function RTS(): UInt8;	function SBC(): UInt8;
    function SEC(): UInt8;	function SED(): UInt8;	function SEI(): UInt8;	function STA(): UInt8;
    function STX(): UInt8;	function STY(): UInt8;	function TAX(): UInt8;	function TAY(): UInt8;
    function TSX(): UInt8;	function TXA(): UInt8;	function TXS(): UInt8;	function TYA(): UInt8;

    function XXX(): UInt8;


    procedure Clock;
    procedure Reset;
    procedure Irq;
    procedure Nmi;

    function Fetch: UInt8;

    function GetFlag(AFlag: T6502Flags): UInt8;
    procedure SetFlag(AFlag: T6502Flags; AIsSet: Boolean = True);
    function InternalBranching(AFlagToTest: T6502Flags; AValueOfFlag: Boolean = True): UInt8;
  public
    FLookupOpcode: TArray<T6502Opcode>;
    constructor Create(ABus: IBus);
    destructor Destroy; override;

    procedure Write(AAdress: UInt16; AData: UInt8);
    function Read(AAdress: UInt16): UInt8;
    function GetStatus: UInt8;
    function GetA: UInt8;
    function GetX: UInt8;
    function GetY: UInt8;
    function GetStackP: UInt8;
    function GetPC: UInt16;
    function GetCurrentOpcode: string;

  end;

implementation
const
  STACK_BASE_ADDRESS = $0100;
{ T6502 }

function T6502.ABS: UInt8;
var
  LHighAdr, LLowAdr: UInt16;
begin
  LLowAdr := Read(FPc);
  System.Inc(FPc);
  LHighAdr := Read(FPc);
  System.Inc(FPc);

  FAbsoluteAddress := (LHighAdr shl 8) or LLowAdr;

  Result := 0;
end;

function T6502.ABX: UInt8;
var
  LHighAdr, LLowAdr: UInt16;
begin
  LLowAdr := Read(FPc);
  System.Inc(FPc);
  LHighAdr := Read(FPc);
  System.Inc(FPc);

  FAbsoluteAddress := (LHighAdr shl 8) or LLowAdr;
  System.Inc(FAbsoluteAddress, Fx);

  if (FAbsoluteAddress and $FF00) <> (LHighAdr shl 8) then
    Result := 1
  else
    Result := 0;
end;

function T6502.ABY: UInt8;
var
  LHighAdr, LLowAdr: UInt16;
begin
  LLowAdr := Read(FPc);
  System.Inc(FPc);
  LHighAdr := Read(FPc);
  System.Inc(FPc);

  FAbsoluteAddress := (LHighAdr shl 8) or LLowAdr;
  System.Inc(FAbsoluteAddress, Fy);

  if (FAbsoluteAddress and $FF00) <> (LHighAdr shl 8) then
    Result := 1
  else
    Result := 0;
end;



function T6502.ADC: UInt8;
var
  LTempResult: UInt16;
  LOverflow1, LOverflow2: Boolean;
begin
  Fetch;

  LTempResult := UInt16(Fa) + UInt16(FFetched) + UInt16(GetFlag(cfCarry));
  SetFlag(cfCarry, LTempResult > 255);
  SetFlag(cfZero, (LTempResult and $00FF) = 0);
  SetFlag(cfNegative, (LTempResult and $80) > 0); // MSB is set

  //  (~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);
  // (~((uint16_t)a ^ (uint16_t)fetched)
  LOverflow1 := (UInt16(Fa) xor UInt16(FFetched)) = 0;
  // ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);
  LOverflow2 := ((UInt16(Fa) xor UInt16(LTempResult)) and $0080) <> 0;
  SetFlag(cfOverflow, LOverflow1 and LOverflow2);

  Fa := LTempResult and $00FF;

  Result := 1;

end;

function T6502.&AND: UInt8;
begin
  Fetch;
  Fa := (Fa and FFetched);

  SetFlag(cfZero, Fa = 0);
  SetFlag(cfNegative, (Fa and $80) > 0);

  Result := 1;
end;

function T6502.ASL: UInt8;
var
  LTemp: Uint16;
begin
  Fetch;

  LTemp := UInt16(FFetched) shl 1;
  SetFlag(cfCarry, (LTemp and $FF00) > 0);
  SetFlag(cfZero, (LTemp and $00FF) = $00);
  SetFlag(cfNegative, (LTemp and $80) > 0);

  if FLookupOpcode[FCurrentOpcode].AddressModeFunc = IMP then
    Fa := LTemp and $00FF
  else
    Write(FAbsoluteAddress, LTemp and $00FF);

  Result := 0;
end;

function T6502.BCC: UInt8;
begin
  Result := InternalBranching(cfCarry, False);
end;

function T6502.BCS: UInt8;
begin
  Result := InternalBranching(cfCarry);
end;

function T6502.BEQ: UInt8;
begin
  Result := InternalBranching(cfZero);
end;

function T6502.BIT: UInt8;
var
  LTemp: UInt16;
begin
  Fetch;
  LTemp := Fa and FFetched;
  SetFlag(cfZero, (LTemp and $00FF) > 0);
  SetFlag(cfNegative, (FFetched and (1 shl 7)) > 0);
  SetFlag(cfOverflow, (FFetched and (1 shl 6)) > 0);

  Result := 0;
end;

function T6502.BMI: UInt8;
begin
  Result := InternalBranching(cfNegative);
end;

function T6502.BNE: UInt8;
begin
  Result := InternalBranching(cfZero, False);
end;

function T6502.BPL: UInt8;
begin
  Result := InternalBranching(cfNegative, False);
end;

function T6502.BRK: UInt8;
begin
  System.Inc(FPc);

  SetFlag(cfDisableInterrupts);
  Write(STACK_BASE_ADDRESS + FStckP, (FPc shr 8) and $00FF);
  System.Dec(FStckP);
  Write(STACK_BASE_ADDRESS + FStckP, FPc and $00FF);
  System.Dec(FStckP);

  SetFlag(cfBreak);
  Write(STACK_BASE_ADDRESS + FStckP, FStatus);
  System.Dec(FStckP);
  SetFlag(cfBreak, False);

  FPc := (Uint16(Read($FFFE))) or (UInt16(Read($FFFF)) shl 8);

  Result := 0;
end;

function T6502.BVC: UInt8;
begin
  Result := InternalBranching(cfOverflow, False);
end;

function T6502.BVS: UInt8;
begin
  Result := InternalBranching(cfOverflow);
end;

function T6502.CLC: UInt8;
begin
  SetFlag(cfCarry, False);
  Result := 0;

end;

function T6502.CLD: UInt8;
begin
  SetFlag(cfDecimalMode, False);
  Result := 0;
end;

function T6502.CLI: UInt8;
begin
  SetFlag(cfDisableInterrupts, False);
  Result := 0;
end;

procedure T6502.Clock;
var
  LAdditionnalCycleOperate, LAdditionnalCycleAddressMode: UInt8;
begin
//  if FRemainningCycleForCurrOpcd = 0 then
  begin
    FCurrentOpcode := Read(FPc);
    System.Inc(FPC);

    FRemainningCycleForCurrOpcd := FLookupOpcode[FCurrentOpcode].TotalCycle;

    LAdditionnalCycleAddressMode := FLookupOpcode[FCurrentOpcode].AddressModeFunc;
    LAdditionnalCycleOperate := FLookupOpcode[FCurrentOpcode].OperateFunc;

    System.Inc(FRemainningCycleForCurrOpcd, LAdditionnalCycleAddressMode and LAdditionnalCycleOperate);
  end;

  System.Dec(FRemainningCycleForCurrOpcd);
end;

function T6502.CLV: UInt8;
begin
  SetFlag(cfOverflow, False);
  Result := 0;
end;

function T6502.CMP: UInt8;
var
  LTemp: UInt16;
begin
  Fetch;
  LTemp := UInt16(Fa) - UInt16(FFetched);
  SetFlag(cfCarry, Fa >= FFetched);
  SetFlag(cfZero, (LTemp and $00FF) = 0);
  SetFlag(cfNegative, (LTemp and $80) > 0);
  Result := 1;
end;

function T6502.CPX: UInt8;
var
  LTemp: UInt16;
begin
  Fetch;
  LTemp := UInt16(Fx) - UInt16(FFetched);
  SetFlag(cfCarry, Fx >= FFetched);
  SetFlag(cfZero, (LTemp and $00FF) = 0);
  SetFlag(cfNegative, (LTemp and $80) > 0);
  Result := 0;

end;

function T6502.CPY: UInt8;
var
  LTemp: UInt16;
begin
  Fetch;
  LTemp := UInt16(Fy) - UInt16(FFetched);
  SetFlag(cfCarry, Fy >= FFetched);
  SetFlag(cfZero, (LTemp and $00FF) = 0);
  SetFlag(cfNegative, (LTemp and $80) > 0);
  Result := 0;
end;

constructor T6502.Create(ABus: IBus);
begin
  FBus := ABus;

  FLookupOpcode := [T6502Opcode.Create('BRK', BRK, IMM, 7 )]+[T6502Opcode.Create( 'ORA', ORA, IZX, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 8 )]+[T6502Opcode.Create( '???', NOP, IMP, 3 )]+[T6502Opcode.Create( 'ORA', ORA, ZP0, 3 )]+[T6502Opcode.Create( 'ASL', ASL, ZP0, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 5 )]+[T6502Opcode.Create( 'PHP', PHP, IMP, 3 )]+[T6502Opcode.Create( 'ORA', ORA, IMM, 2 )]+[T6502Opcode.Create( 'ASL', ASL, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( '???', NOP, IMP, 4 )]+[T6502Opcode.Create( 'ORA', ORA, ABS, 4 )]+[T6502Opcode.Create( 'ASL', ASL, ABS, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )]+
		[T6502Opcode.Create( 'BPL', BPL, REL, 2 )]+[T6502Opcode.Create( 'ORA', ORA, IZY, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 8 )]+[T6502Opcode.Create( '???', NOP, IMP, 4 )]+[T6502Opcode.Create( 'ORA', ORA, ZPX, 4 )]+[T6502Opcode.Create( 'ASL', ASL, ZPX, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )]+[T6502Opcode.Create( 'CLC', CLC, IMP, 2 )]+[T6502Opcode.Create( 'ORA', ORA, ABY, 4 )]+[T6502Opcode.Create( '???', NOP, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 7 )]+[T6502Opcode.Create( '???', NOP, IMP, 4 )]+[T6502Opcode.Create( 'ORA', ORA, ABX, 4 )]+[T6502Opcode.Create( 'ASL', ASL, ABX, 7 )]+[T6502Opcode.Create( '???', XXX, IMP, 7 )]+
		[T6502Opcode.Create( 'JSR', JSR, ABS, 6 )]+[T6502Opcode.Create( '&AND', &AND, IZX, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 8 )]+[T6502Opcode.Create( 'BIT', BIT, ZP0, 3 )]+[T6502Opcode.Create( '&AND', &AND, ZP0, 3 )]+[T6502Opcode.Create( 'ROL', ROL, ZP0, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 5 )]+[T6502Opcode.Create( 'PLP', PLP, IMP, 4 )]+[T6502Opcode.Create( '&AND', &AND, IMM, 2 )]+[T6502Opcode.Create( 'ROL', ROL, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( 'BIT', BIT, ABS, 4 )]+[T6502Opcode.Create( '&AND', &AND, ABS, 4 )]+[T6502Opcode.Create( 'ROL', ROL, ABS, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )]+
		[T6502Opcode.Create(  'BMI', BMI, REL, 2 )]+[T6502Opcode.Create( '&AND', &AND, IZY, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 8 )]+[T6502Opcode.Create( '???', NOP, IMP, 4 )]+[T6502Opcode.Create( '&AND', &AND, ZPX, 4 )]+[T6502Opcode.Create( 'ROL', ROL, ZPX, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )]+[T6502Opcode.Create( 'SEC', SEC, IMP, 2 )]+[T6502Opcode.Create( '&AND', &AND, ABY, 4 )]+[T6502Opcode.Create( '???', NOP, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 7 )]+[T6502Opcode.Create( '???', NOP, IMP, 4 )]+[T6502Opcode.Create( '&AND', &AND, ABX, 4 )]+[T6502Opcode.Create( 'ROL', ROL, ABX, 7 )]+[T6502Opcode.Create( '???', XXX, IMP, 7 )]+
		[T6502Opcode.Create(  'RTI', RTI, IMP, 6 )]+[T6502Opcode.Create( 'EOR', EOR, IZX, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 8 )]+[T6502Opcode.Create( '???', NOP, IMP, 3 )]+[T6502Opcode.Create( 'EOR', EOR, ZP0, 3 )]+[T6502Opcode.Create( 'LSR', LSR, ZP0, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 5 )]+[T6502Opcode.Create( 'PHA', PHA, IMP, 3 )]+[T6502Opcode.Create( 'EOR', EOR, IMM, 2 )]+[T6502Opcode.Create( 'LSR', LSR, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( 'JMP', JMP, ABS, 3 )]+[T6502Opcode.Create( 'EOR', EOR, ABS, 4 )]+[T6502Opcode.Create( 'LSR', LSR, ABS, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )];
  FLookupOpcode := FLookupOpcode + [T6502Opcode.Create( 'BVC', BVC, REL, 2 )]+[T6502Opcode.Create( 'EOR', EOR, IZY, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 8 )]+[T6502Opcode.Create( '???', NOP, IMP, 4 )]+[T6502Opcode.Create( 'EOR', EOR, ZPX, 4 )]+[T6502Opcode.Create( 'LSR', LSR, ZPX, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )]+[T6502Opcode.Create( 'CLI', CLI, IMP, 2 )]+[T6502Opcode.Create( 'EOR', EOR, ABY, 4 )]+[T6502Opcode.Create( '???', NOP, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 7 )]+[T6502Opcode.Create( '???', NOP, IMP, 4 )]+[T6502Opcode.Create( 'EOR', EOR, ABX, 4 )]+[T6502Opcode.Create( 'LSR', LSR, ABX, 7 )]+[T6502Opcode.Create( '???', XXX, IMP, 7 )]+
		[T6502Opcode.Create(  'RTS', RTS, IMP, 6 )]+[T6502Opcode.Create( 'ADC', ADC, IZX, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 8 )]+[T6502Opcode.Create( '???', NOP, IMP, 3 )]+[T6502Opcode.Create( 'ADC', ADC, ZP0, 3 )]+[T6502Opcode.Create( 'ROR', ROR, ZP0, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 5 )]+[T6502Opcode.Create( 'PLA', PLA, IMP, 4 )]+[T6502Opcode.Create( 'ADC', ADC, IMM, 2 )]+[T6502Opcode.Create( 'ROR', ROR, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( 'JMP', JMP, IND, 5 )]+[T6502Opcode.Create( 'ADC', ADC, ABS, 4 )]+[T6502Opcode.Create( 'ROR', ROR, ABS, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )]+
		[T6502Opcode.Create(  'BVS', BVS, REL, 2 )]+[T6502Opcode.Create( 'ADC', ADC, IZY, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 8 )]+[T6502Opcode.Create( '???', NOP, IMP, 4 )]+[T6502Opcode.Create( 'ADC', ADC, ZPX, 4 )]+[T6502Opcode.Create( 'ROR', ROR, ZPX, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )]+[T6502Opcode.Create( 'SEI', SEI, IMP, 2 )]+[T6502Opcode.Create( 'ADC', ADC, ABY, 4 )]+[T6502Opcode.Create( '???', NOP, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 7 )]+[T6502Opcode.Create( '???', NOP, IMP, 4 )]+[T6502Opcode.Create( 'ADC', ADC, ABX, 4 )]+[T6502Opcode.Create( 'ROR', ROR, ABX, 7 )]+[T6502Opcode.Create( '???', XXX, IMP, 7 )]+
		[T6502Opcode.Create(  '???', NOP, IMP, 2 )]+[T6502Opcode.Create( 'STA', STA, IZX, 6 )]+[T6502Opcode.Create( '???', NOP, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )]+[T6502Opcode.Create( 'STY', STY, ZP0, 3 )]+[T6502Opcode.Create( 'STA', STA, ZP0, 3 )]+[T6502Opcode.Create( 'STX', STX, ZP0, 3 )]+[T6502Opcode.Create( '???', XXX, IMP, 3 )]+[T6502Opcode.Create( 'DEY', DEY, IMP, 2 )]+[T6502Opcode.Create( '???', NOP, IMP, 2 )]+[T6502Opcode.Create( 'TXA', TXA, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( 'STY', STY, ABS, 4 )]+[T6502Opcode.Create( 'STA', STA, ABS, 4 )]+[T6502Opcode.Create( 'STX', STX, ABS, 4 )]+[T6502Opcode.Create( '???', XXX, IMP, 4 )]+
		[T6502Opcode.Create(  'BCC', BCC, REL, 2 )]+[T6502Opcode.Create( 'STA', STA, IZY, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )]+[T6502Opcode.Create( 'STY', STY, ZPX, 4 )]+[T6502Opcode.Create( 'STA', STA, ZPX, 4 )]+[T6502Opcode.Create( 'STX', STX, ZPY, 4 )]+[T6502Opcode.Create( '???', XXX, IMP, 4 )]+[T6502Opcode.Create( 'TYA', TYA, IMP, 2 )]+[T6502Opcode.Create( 'STA', STA, ABY, 5 )]+[T6502Opcode.Create( 'TXS', TXS, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 5 )]+[T6502Opcode.Create( '???', NOP, IMP, 5 )]+[T6502Opcode.Create( 'STA', STA, ABX, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 5 )]+
		[T6502Opcode.Create(  'LDY', LDY, IMM, 2 )]+[T6502Opcode.Create( 'LDA', LDA, IZX, 6 )]+[T6502Opcode.Create( 'LDX', LDX, IMM, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )]+[T6502Opcode.Create( 'LDY', LDY, ZP0, 3 )]+[T6502Opcode.Create( 'LDA', LDA, ZP0, 3 )]+[T6502Opcode.Create( 'LDX', LDX, ZP0, 3 )]+[T6502Opcode.Create( '???', XXX, IMP, 3 )]+[T6502Opcode.Create( 'TAY', TAY, IMP, 2 )]+[T6502Opcode.Create( 'LDA', LDA, IMM, 2 )]+[T6502Opcode.Create( 'TAX', TAX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( 'LDY', LDY, ABS, 4 )]+[T6502Opcode.Create( 'LDA', LDA, ABS, 4 )]+[T6502Opcode.Create( 'LDX', LDX, ABS, 4 )]+[T6502Opcode.Create( '???', XXX, IMP, 4 )]+
		[T6502Opcode.Create(  'BCS', BCS, REL, 2 )]+[T6502Opcode.Create( 'LDA', LDA, IZY, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 5 )]+[T6502Opcode.Create( 'LDY', LDY, ZPX, 4 )]+[T6502Opcode.Create( 'LDA', LDA, ZPX, 4 )]+[T6502Opcode.Create( 'LDX', LDX, ZPY, 4 )]+[T6502Opcode.Create( '???', XXX, IMP, 4 )]+[T6502Opcode.Create( 'CLV', CLV, IMP, 2 )]+[T6502Opcode.Create( 'LDA', LDA, ABY, 4 )]+[T6502Opcode.Create( 'TSX', TSX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 4 )]+[T6502Opcode.Create( 'LDY', LDY, ABX, 4 )]+[T6502Opcode.Create( 'LDA', LDA, ABX, 4 )]+[T6502Opcode.Create( 'LDX', LDX, ABY, 4 )]+[T6502Opcode.Create( '???', XXX, IMP, 4 )]+
		[T6502Opcode.Create(  'CPY', CPY, IMM, 2 )]+[T6502Opcode.Create( 'CMP', CMP, IZX, 6 )]+[T6502Opcode.Create( '???', NOP, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 8 )]+[T6502Opcode.Create( 'CPY', CPY, ZP0, 3 )]+[T6502Opcode.Create( 'CMP', CMP, ZP0, 3 )]+[T6502Opcode.Create( 'DEC', DEC, ZP0, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 5 )]+[T6502Opcode.Create( 'INY', INY, IMP, 2 )]+[T6502Opcode.Create( 'CMP', CMP, IMM, 2 )]+[T6502Opcode.Create( 'DEX', DEX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( 'CPY', CPY, ABS, 4 )]+[T6502Opcode.Create( 'CMP', CMP, ABS, 4 )]+[T6502Opcode.Create( 'DEC', DEC, ABS, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )]+
		[T6502Opcode.Create(  'BNE', BNE, REL, 2 )]+[T6502Opcode.Create( 'CMP', CMP, IZY, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 8 )]+[T6502Opcode.Create( '???', NOP, IMP, 4 )]+[T6502Opcode.Create( 'CMP', CMP, ZPX, 4 )]+[T6502Opcode.Create( 'DEC', DEC, ZPX, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )]+[T6502Opcode.Create( 'CLD', CLD, IMP, 2 )]+[T6502Opcode.Create( 'CMP', CMP, ABY, 4 )]+[T6502Opcode.Create( 'NOP', NOP, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 7 )]+[T6502Opcode.Create( '???', NOP, IMP, 4 )]+[T6502Opcode.Create( 'CMP', CMP, ABX, 4 )]+[T6502Opcode.Create( 'DEC', DEC, ABX, 7 )]+[T6502Opcode.Create( '???', XXX, IMP, 7 )]+
		[T6502Opcode.Create(  'CPX', CPX, IMM, 2 )]+[T6502Opcode.Create( 'SBC', SBC, IZX, 6 )]+[T6502Opcode.Create( '???', NOP, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 8 )]+[T6502Opcode.Create( 'CPX', CPX, ZP0, 3 )]+[T6502Opcode.Create( 'SBC', SBC, ZP0, 3 )]+[T6502Opcode.Create( 'System.Inc', Inc, ZP0, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 5 )]+[T6502Opcode.Create( 'INX', INX, IMP, 2 )]+[T6502Opcode.Create( 'SBC', SBC, IMM, 2 )]+[T6502Opcode.Create( 'NOP', NOP, IMP, 2 )]+[T6502Opcode.Create( '???', SBC, IMP, 2 )]+[T6502Opcode.Create( 'CPX', CPX, ABS, 4 )]+[T6502Opcode.Create( 'SBC', SBC, ABS, 4 )]+[T6502Opcode.Create( 'Inc', Inc, ABS, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )]+
		[T6502Opcode.Create(  'BEQ', BEQ, REL, 2 )]+[T6502Opcode.Create( 'SBC', SBC, IZY, 5 )]+[T6502Opcode.Create( '???', XXX, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 8 )]+[T6502Opcode.Create( '???', NOP, IMP, 4 )]+[T6502Opcode.Create( 'SBC', SBC, ZPX, 4 )]+[T6502Opcode.Create( 'System.Inc', Inc, ZPX, 6 )]+[T6502Opcode.Create( '???', XXX, IMP, 6 )]+[T6502Opcode.Create( 'SED', SED, IMP, 2 )]+[T6502Opcode.Create( 'SBC', SBC, ABY, 4 )]+[T6502Opcode.Create( 'NOP', NOP, IMP, 2 )]+[T6502Opcode.Create( '???', XXX, IMP, 7 )]+[T6502Opcode.Create( '???', NOP, IMP, 4 )]+[T6502Opcode.Create( 'SBC', SBC, ABX, 4 )]+[T6502Opcode.Create( 'Inc', Inc, ABX, 7 )]+[T6502Opcode.Create( '???', XXX, IMP, 7 )];
	
end;

function T6502.DEC: UInt8;
var
  LTemp: UInt16;
begin
  Fetch;
  LTemp := FFetched - 1;
  Write(FAbsoluteAddress, LTemp and $00FF);
  SetFlag(cfZero, (LTemp and $00FF) = 0);
  SetFlag(cfNegative, (LTemp and $80) > 0);
  Result := 0;
end;

destructor T6502.Destroy;
begin
  inherited;
end;

function T6502.DEX: UInt8;
begin
  System.Dec(Fx);
  SetFlag(cfZero, Fx = 0);
  SetFlag(cfNegative, (Fx and $80) <> 0);
  Result := 0;
end;

function T6502.DEY: UInt8;
begin
  System.Dec(Fy);
  SetFlag(cfZero, Fy = 0);
  SetFlag(cfNegative, (Fy and $80) <> 0);
  Result := 0;
end;

function T6502.EOR: UInt8;
begin
  Fetch;
  Fa := Fa xor FFetched;
  SetFlag(cfZero, Fa = 0);
  SetFlag(cfNegative, (Fa and $80) <> 0);
  Result := 0;
end;

function T6502.Fetch: UInt8;
begin
  FFetched := 0;
  if ((TMethod(FLookupOpcode[FCurrentOpcode].AddressModeFunc).Code) <> @T6502.IMP) then
  begin
    FFetched := Read(FAbsoluteAddress);
  end;
  Result := FFetched;
end;



function T6502.GetA: UInt8;
begin
  Result := Fa;
end;

function T6502.GetCurrentOpcode: string;
begin
  Result := FLookupOpcode[FCurrentOpcode].Name;
end;

function T6502.GetFlag(AFlag: T6502Flags): UInt8;
begin
  if ((FStatus and Ord(AFlag)) > 0) then
    Result := 1
  else
    Result := 0;
end;

function T6502.GetPC: UInt16;
begin
  Result := FPc;
end;

function T6502.GetStackP: UInt8;
begin
  Result := FStckP;
end;

function T6502.GetStatus: UInt8;
begin
  Result := FStatus;
end;

function T6502.GetX: UInt8;
begin
  Result := Fx;
end;

function T6502.GetY: UInt8;
begin
  Result := Fy;
end;

function T6502.IMM: UInt8;
begin
  FAbsoluteAddress := FPc;
  System.Inc(FPc);

  Result := 0;
end;

function T6502.IMP: UInt8;
begin
  FFetched := FA;

  Result := 0;
end;

function T6502.Inc: UInt8;
var
  LTemp: UInt16;
begin
  Fetch;
  LTemp := FFetched + 1;
  Write(FAbsoluteAddress, LTemp and $00FF);
  SetFlag(cfZero, (LTemp and $00FF) = 0);
  SetFlag(cfNegative, (LTemp and $80) > 0);
  Result := 0;

end;

function T6502.IND: UInt8;
var
  LFinalPtr, LHighPtr, LLowPtr: UInt16;
  
begin
  LLowPtr := Read(FPc);
  System.Inc(FPc);
  LHighPtr := Read(FPc);
  System.Inc(FPc);

  LFInalPtr := (LHighPtr shl 8) or LLowPtr;

  if LLowPtr = $00FF then // Simulate page boundary HW bug
    FAbsoluteAddress := (Read(LFinalPtr and $FF00) shl 8) or (Read(LFinalPtr)) 
  else
    FAbsoluteAddress := (Read(LFinalPtr + 1) shl 8) or (Read(LFinalPtr));

  Result := 0;
end;

function T6502.InternalBranching(AFlagToTest: T6502Flags; AValueOfFlag: Boolean = True): UInt8;
var
  LValueToTest: UInt8;
begin

  if AValueOfFlag then
    LValueToTest := 1
  else
    LValueToTest := 0;
  if GetFlag(AFlagToTest) = LValueToTest then
  begin
    System.Inc(FRemainningCycleForCurrOpcd);
    FAbsoluteAddress := FPc + FRelativAddress;

    if (FAbsoluteAddress and $FF00) <> (FPc and $FF00) then
    begin
      System.Inc(FRemainningCycleForCurrOpcd);
    end;

    FPc := FAbsoluteAddress;
  end;

  Result := 0;
end;

function T6502.INX: UInt8;
begin
  System.Inc(Fx);
  SetFlag(cfZero, Fx = 0);
  SetFlag(cfNegative, (Fx and $80) <> 0);
  Result := 0;
end;

function T6502.INY: UInt8;
begin
  System.Inc(Fy);
  SetFlag(cfZero, Fy = 0);
  SetFlag(cfNegative, (Fy and $80) <> 0);
  Result := 0;
end;

procedure T6502.Irq;
var
  LLow, LHigh: UInt16;
begin
  if GetFlag(cfDisableInterrupts) = 0 then
  begin
    Write(STACK_BASE_ADDRESS + FStckP, (FPc shr 8) and $00FF);
    System.Dec(FStckP);
    Write(STACK_BASE_ADDRESS + FStckP, FPc and $00FF);
    System.Dec(FStckP);

    SetFlag(cfBreak, False);
    SetFlag(cfUnused);
    SetFlag(cfDisableInterrupts);
    Write(STACK_BASE_ADDRESS + FStckP, FStatus);
    System.Dec(FStckP);

    FAbsoluteAddress := $FFFE;
    LLow := Read(FAbsoluteAddress);
    LHigh := Read(FAbsoluteAddress + 1);
    FPc := (LHigh shl 8) or LLow;

    FRemainningCycleForCurrOpcd := 7;
  end;
end;

function T6502.IZX: UInt8;
var
  LAdd, LLowAdr, LHighAdr: UInt16;
begin
  LAdd := Read(FPc);
  System.Inc(FPc);

  LLowAdr := Read(Uint16(LAdd + UInt16(Fx)) and $00FF);
  LHighAdr := Read(Uint16(LAdd + UInt16(Fx) + 1) and $00FF);

  FAbsoluteAddress := (LHighAdr shl 8) or LLowAdr;

  Result := 0;  
end;

function T6502.IZY: UInt8;
var
  LAdrWhereActualAdrIs, LLowAdr, LhighAdr: UInt16;
begin
  LAdrWhereActualAdrIs := Read(FPc);

  LLowAdr := Read(LAdrWhereActualAdrIs and $00FF);
  LHighAdr := Read(LAdrWhereActualAdrIs + 1) and $00FF;

  FAbsoluteAddress := (LhighAdr shl 8) or LLowAdr;
  System.Inc(FAbsoluteAddress, FY);

  if ((FAbsoluteAddress and $FF00) <> (LhighAdr shl 8)) then
    Result := 1
  else
    Result := 0;
end;

function T6502.JMP: UInt8;
begin
  Fpc := FAbsoluteAddress;
  Result := 0;
end;

function T6502.JSR: UInt8;
begin
  System.Dec(FPc);

  Write(STACK_BASE_ADDRESS + FStckP, (FPc shr 8) and $00FF);
  System.Dec(FStckP);
  Write(STACK_BASE_ADDRESS + FStckP, FPc and $00FF);
  System.Dec(FStckP);

  Fpc := FAbsoluteAddress;
  Result := 0;

end;

function T6502.LDA: UInt8;
begin
  Fetch;
  Fa := FFetched;

  SetFlag(cfZero, Fa = 0);
  SetFlag(cfNegative, (Fa and $80) <> 0);
  Result := 1;

end;

function T6502.LDX: UInt8;
begin
  Fetch;
  Fx:= FFetched;

  SetFlag(cfZero, Fx = 0);
  SetFlag(cfNegative, (Fx and $80) <> 0);
  Result := 1;
end;

function T6502.LDY: UInt8;
begin
  Fetch;
  Fy:= FFetched;

  SetFlag(cfZero, Fy = 0);
  SetFlag(cfNegative, (Fy and $80) <> 0);
  Result := 1;
end;

function T6502.LSR: UInt8;
var
  LTemp: UInt16;
begin
  Fetch;

  SetFlag(cfCarry, (FFetched and $0001) <> 0);
  LTemp := FFetched shr 1;
  SetFlag(cfZero, (LTemp and $00FF) = 0);
  SetFlag(cfNegative, (LTemp and $0080) <> 0);
  if FLookupOpcode[FCurrentOpcode].AddressModeFunc = IMP then
    Fa := LTemp and $00FF
  else
    Write(FAbsoluteAddress, LTemp and $00FF);

  Result := 0;
end;

procedure T6502.Nmi;
var
  LLow, LHigh: UInt16;
begin
    Write(STACK_BASE_ADDRESS + FStckP, (FPc shr 8) and $00FF);
    System.Dec(FStckP);
    Write(STACK_BASE_ADDRESS + FStckP, FPc and $00FF);
    System.Dec(FStckP);

    SetFlag(cfBreak, False);
    SetFlag(cfUnused);
    SetFlag(cfDisableInterrupts);
    Write(STACK_BASE_ADDRESS + FStckP, FStatus);
    System.Dec(FStckP);

    FAbsoluteAddress := $FFFA;
    LLow := Read(FAbsoluteAddress);
    LHigh := Read(FAbsoluteAddress + 1);
    FPc := (LHigh shl 8) or LLow;

    FRemainningCycleForCurrOpcd := 7;
end;

function T6502.NOP: UInt8;
begin
  case FCurrentOpcode of
   $1C, $3C, $5C, $7C, $DC, $FC: Result := 1;
  else
    Result := 0;
  end;
end;

function T6502.ORA: UInt8;
begin
  Fetch;
  Fa := Fa or FFetched;
  SetFlag(cfZero, Fa = 0);
  SetFlag(cfNegative, (Fa and $80) <> 0);
  Result := 1;
end;

function T6502.PHA: UInt8;
begin
  Write(STACK_BASE_ADDRESS + FStckP, Fa);
  System.Dec(FStckP);

  Result := 0;
end;

function T6502.PHP: UInt8;
begin
  Write(STACK_BASE_ADDRESS + FStckP, (FStatus or Ord(cfBreak)) or Ord(cfUnused));
  SetFlag(cfBreak, False);
  SetFlag(cfUnused, False);
  System.Dec(FStckP);

  Result := 0;
end;

function T6502.PLA: UInt8;
begin
  System.Inc(FStckP);

  Fa := Read(STACK_BASE_ADDRESS+ FStckP);
  SetFlag(cfZero, Fa = $00);
  SetFlag(cfNegative, (Fa and $80) <> 0);

  Result := 0;
end;

function T6502.PLP: UInt8;
begin
  System.Inc(FStckP);

  FStatus := Read(STACK_BASE_ADDRESS+ FStckP);
  SetFlag(cfUnused);

  Result := 0;
end;

function T6502.Read(AAdress: UInt16): UInt8;
begin
  Result := FBus.Read(AAdress);
end;

function T6502.REL: UInt8;
begin
  FRelativAddress := Read(FPc);
  System.Inc(FPc);

  if ((FRelativAddress and $80) <> 0) then
    FRelativAddress := FRelativAddress or $FF00;
  Result := 0;  
end;

procedure T6502.Reset;
var
  LLow, LHigh: UInt16;
begin
  Fa := 0;
  Fx := 0;
  Fy := 0;
  FStckP := $FD;
  FStatus := $00 or Ord(cfUnused);

  FAbsoluteAddress := $FFFC;

  LLow := Read(FAbsoluteAddress + 0);
  LHigh := Read(FAbsoluteAddress + 1);

  FPc := (LHigh shl 8) or LLow;

  FRelativAddress := 0;
  FAbsoluteAddress := 0;
  FFetched := $00;

  FRemainningCycleForCurrOpcd := 8;
end;

function T6502.ROL: UInt8;
var
  LTemp: Uint16;
begin
  Fetch;
  LTemp := UInt16(FFetched shl 1) or GetFlag(cfCarry);
  SetFlag(cfCarry, (LTemp and $FF00) <> 0);
  SetFlag(cfZero, (LTemp and $00FF) = 0);
  SetFlag(cfNegative, (LTemp and $0080) <> 0);
  if FLookupOpcode[FCurrentOpcode].AddressModeFunc = IMP then
    Fa := LTemp and $00FF
  else
    Write(FAbsoluteAddress, LTemp and $00FF);

  Result := 0;

end;

function T6502.ROR: UInt8;
var
  LTemp: UInt16;
begin
  Fetch;
  LTemp := UInt16(GetFlag(cfCarry) shl 7) or (FFetched shl 1);
  SetFlag(cfCarry, (FFetched and $01) <> 0);
  SetFlag(cfZero, (LTemp and $00FF) = 0);
  SetFlag(cfNegative, (LTemp and $0080) <> 0);
  if FLookupOpcode[FCurrentOpcode].AddressModeFunc = IMP then
    Fa := LTemp and $00FF
  else
    Write(FAbsoluteAddress, LTemp and $00FF);

  Result := 0;
end;

function T6502.RTI: UInt8;
begin
  System.Inc(FStckP);
  FStatus := Read(STACK_BASE_ADDRESS + FStckP);
  FStatus := FStatus and (not Ord(cfBreak));
  FStatus := FStatus and (not Ord(cfUnused));

  System.Inc(FStckP);
  FStckP := UInt16(Read(STACK_BASE_ADDRESS + FStckP));
  System.Inc(FStckP);
  FStckP := FStckP or (UInt16(STACK_BASE_ADDRESS + FStckP) shl 8);

  Result := 0;
end;

function T6502.RTS: UInt8;
begin
  System.Inc(FStckP);
  FStckP := UInt16(Read(STACK_BASE_ADDRESS + FStckP));
  System.Inc(FStckP);
  FStckP := FStckP or (UInt16(STACK_BASE_ADDRESS + FStckP) shl 8);

  System.Inc(FPc);
  Result := 0;
end;

function T6502.SBC: UInt8;
var
  LTempResult: UInt16;
  LValue: Uint16;
  LOverflow1: Boolean;
  LOverflow2: Boolean;
begin
  Fetch;

  LValue := Uint16(FFetched) xor $00FF;

  LTempResult := UInt16(Fa) + LValue + UInt16(GetFlag(cfCarry));
  SetFlag(cfCarry, LTempResult > 255);
  SetFlag(cfZero, (LTempResult and $00FF) = 0);
  SetFlag(cfNegative, (LTempResult and $80) > 0); // MSB is set

  //  (~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);
  // (~((uint16_t)a ^ (uint16_t)fetched)
  LOverflow1 := (UInt16(Fa) xor UInt16(FFetched)) = 0;
  // ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);
  LOverflow2 := ((UInt16(Fa) xor UInt16(LTempResult)) and $0080) <> 0;
  SetFlag(cfOverflow, LOverflow1 and LOverflow2);

  Fa := LTempResult and $00FF;

  Result := 1;
end;

function T6502.SEC: UInt8;
begin
  SetFlag(cfCarry);
  Result := 0;
end;

function T6502.SED: UInt8;
begin
  SetFlag(cfDecimalMode);
  Result := 0;
end;

function T6502.SEI: UInt8;
begin
  SetFlag(cfDisableInterrupts);
  Result := 0;
end;

procedure T6502.SetFlag(AFlag: T6502Flags; AIsSet: Boolean);
begin
  if AIsSet then
    FStatus := (FStatus or Ord(AFlag))
  else
    FStatus := FStatus and (not Ord(AFlag));
end;

function T6502.STA: UInt8;
begin
  Write(FAbsoluteAddress, Fa);
  Result := 0;
end;

function T6502.STX: UInt8;
begin
  Write(FAbsoluteAddress, Fx);
  Result := 0;
end;

function T6502.STY: UInt8;
begin
  Write(FAbsoluteAddress, Fy);
  Result := 0;
end;

function T6502.TAX: UInt8;
begin
  Fx := Fa;
  SetFlag(cfZero, Fx = $00);
  SetFlag(cfNegative, (Fx and $80) <> 0);
  Result := 0;
end;

function T6502.TAY: UInt8;
begin
  Fy := Fa;
  SetFlag(cfZero, Fy = $00);
  SetFlag(cfNegative, (Fy and $80) <> 0);
  Result := 0;
end;

function T6502.TSX: UInt8;
begin
  Fx := FStckP;
  SetFlag(cfZero, Fx = $00);
  SetFlag(cfNegative, (Fx and $80) <> 0);
  Result := 0;
end;

function T6502.TXA: UInt8;
begin
  Fa := Fx;
  SetFlag(cfZero, Fa = $00);
  SetFlag(cfNegative, (Fa and $80) <> 0);
  Result := 0;
end;

function T6502.TXS: UInt8;
begin
  FStckP := Fx;
  Result := 0;
end;

function T6502.TYA: UInt8;
begin
  Fa := Fy;
  SetFlag(cfZero, Fa = $00);
  SetFlag(cfNegative, (Fa and $80) <> 0);
  Result := 0;
end;

procedure T6502.Write(AAdress: UInt16; AData: UInt8);
begin
  FBus.Write(AAdress, AData);
end;

function T6502.XXX: UInt8;
begin
  Result := 0;
end;

function T6502.ZP0: UInt8;
begin
  FAbsoluteAddress := Read(Fpc);
  System.Inc(Fpc);
  FAbsoluteAddress := FAbsoluteAddress and $00FF;

  Result := 0;
end;

function T6502.ZPX: UInt8;
begin
  FAbsoluteAddress := (Read(Fpc)) + Fx;
  System.Inc(Fpc);
  FAbsoluteAddress := FAbsoluteAddress and $00FF;

  Result := 0;
end;

function T6502.ZPY: UInt8;
begin
  FAbsoluteAddress := (Read(Fpc)) + Fy;
  System.Inc(Fpc);
  FAbsoluteAddress := FAbsoluteAddress and $00FF;

  Result := 0;
end;

{ T6502Opcode }

constructor T6502Opcode.Create(const AName: string; AOpFunc,
  AAddFunc: TUint8Func; ATotalCycle: UInt8);
begin
  Name := AName;
  AddressModeFunc := AAddFunc;
  OperateFunc := AOpFunc;
  TotalCycle := ATotalCycle;
end;

end.
