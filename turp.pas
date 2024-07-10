uses
  SysUtils,
  StrUtils,
  Types;

type
  TStep = (Left, Right);

  TState = String;
  TSymbol = String;
  TSymbolDynArray = TStringDynArray;

  TTurp = record
    Current : TState;
    Read    : TSymbol;
    Write   : TSymbol;
    Step    : TStep;
    Next    : TState;
  end;

  TMachine = record
    Tape : TSymbolDynArray;
    Head : LongWord;
    State : TState;
  end;

function MachineNext(var Machine: TMachine; Turps: array of TTurp): Boolean;
var
  Turp : TTurp;
begin
  for Turp in Turps do
    if (Turp.Current = Machine.State) and (Turp.Read = Machine.Tape[Machine.Head]) then
    begin
      Machine.Tape[Machine.Head] := Turp.Write;
      case Turp.Step of
        Left:
          if Machine.Head = 0 then
            Machine.Head := High(Machine.Tape)
          else
            Dec(Machine.Head);
        Right:
          Machine.Head := (Machine.Head + 1) mod Length(Machine.Tape);
      end;
      Machine.State := Turp.Next;
      Exit(True);
    end;
  MachineNext := False;
end;

procedure MachineDump(Machine: TMachine);
var
  Cell : TSymbol;
  I, J : LongWord;
begin
  WriteLn('STATE: ', Machine.State);
  for Cell in Machine.Tape do
  begin
    Write(Cell, ' ');
  end;
  WriteLn;
  for I := 0 to High(Machine.Tape) do
  begin
    if I = Machine.Head then Write('^');
    for J := 0 to Length(Cell) - 1 do
      Write(' ');
    if I <> Machine.Head then Write(' ');
  end;
end;

function ReadTurp(FilePath: String; Line: LongWord; Source: String): TTurp;
const
  CURRENT : Word = 0;
  READ    : Word = 1;
  WRITE   : Word = 2;
  STEP    : Word = 3;
  NEXT    : Word = 4;
var
  Tokens : TStringDynArray;
begin
  Tokens := SplitString(Source, ' ');
  if Length(Tokens) <> 5 then
  begin
    WriteLn(StdErr, Format('%s:%d: Error: A single turp is expected to have 5 tokens', [FilePath, Line]));
    Halt(1);
  end;

  ReadTurp.Current := Tokens[CURRENT];
  ReadTurp.Read := Tokens[READ];
  ReadTurp.Write := Tokens[WRITE];
  case Tokens[STEP] of
    'L': ReadTurp.Step := Left;
    'R': ReadTurp.Step := Right;
  else
    WriteLn(StdErr, Format('%s:%d: Error: "%s" is not a correct step. Expected "L" or "R"', [FilePath, Line, Tokens[STEP]]));
  end;
  ReadTurp.Next := Tokens[NEXT];
end;

function SlurpFile(FilePath: String): String;
var
  FileHandle : Text;
  Line : String;
begin
  Assign(FileHandle, FilePath);
  {$I-}
  Reset(FileHandle);
  {$I+}
  if IOResult <> 0 then
  begin
    WriteLn(StdErr, 'Error: Could not open file "', FilePath, '"');
    Halt(1);
  end;

  SlurpFile := '';

  while not Eof(FileHandle) do
  begin
    ReadLn(FileHandle, Line);
    SlurpFile := SlurpFile + Line + LineEnding;
  end;

  if Length(SlurpFile) > 0 then
    SetLength(SlurpFile, Length(SlurpFile) - Length(LineEnding));

  Close(FileHandle);
end;

var
  I : LongWord;

  TurpFilePath : String;
  TapeFilePath : String;

  Lines : TStringArray;
  Line : String;

  Turps : array of TTurp = ();

  Machine : TMachine;
begin
  if ParamCount = 0 then
  begin
    WriteLn(StdErr, 'Error: Input turp file is not provided');
    WriteLn(StdErr, 'Usage: turp <input.turp> <input.tape>');
    Halt(1);
  end;
  TurpFilePath := ParamStr(1);

  if ParamCount = 1 then
  begin
    WriteLn(StdErr, 'Error: Input tape file is not provided');
    WriteLn(StdErr, 'Usage: turp <input.turp> <input.tape>');
    Halt(1);
  end;
  TapeFilePath := ParamStr(2);

  Lines := SplitString(SlurpFile(TurpFilePath), LineEnding);
  for I := 0 to High(Lines) do
  begin
    Line := Trim(Lines[I]);
    if Length(Line) > 0 then
      Insert(ReadTurp(TurpFilePath, I + 1, Line), Turps, Length(Turps));
  end;

  Machine.Tape := SplitString(SlurpFile(TapeFilePath), ' ');

  Write('INITIAL STATE: ');
  ReadLn(Input, Machine.State);

  repeat
    MachineDump(Machine);
    ReadLn(Input);
  until not MachineNext(Machine, Turps);
end.
