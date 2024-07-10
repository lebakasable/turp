uses
  SysUtils,
  StrUtils,
  Types;

type
  TStep = (Left, Right);

  TState = AnsiString;
  TStateArray = array of TState;
  TSymbol = AnsiString;
  TSymbolArray = array of TSymbol;

  TTurp = record
    Current : TState;
    Read    : TSymbol;
    Write   : TSymbol;
    Step    : TStep;
    Next    : TState;
  end;

  TMachine = record
    Tape : TSymbolArray;
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

function Tokenize(Source: AnsiString): TStringDynArray;
var
  Tokens : TStringDynArray = ();
  Words : TStringDynArray;
  Word : AnsiString;
  I : LongWord;
begin
  Words := SplitString(Source, ' ');
  for I := 0 to High(Words) do
  begin
    Word := Trim(Words[I]);
    if Length(Word) > 0 then
      Insert(Word, Tokens, Length(Tokens));
  end;
  Tokenize := Tokens;
end;

function ParseTurp(FilePath: AnsiString; Line: LongWord; Source: AnsiString): TTurp;
const
  CURRENT : Word = 0;
  READ    : Word = 1;
  WRITE   : Word = 2;
  STEP    : Word = 3;
  NEXT    : Word = 4;
var
  Tokens : TStringDynArray;
begin
  Tokens := Tokenize(Source);
  if Length(Tokens) <> 5 then
  begin
    WriteLn(StdErr, Format('%s:%d: Error: A single turp is expected to have 5 tokens', [FilePath, Line]));
    Halt(1);
  end;

  ParseTurp.Current := UpCase(Tokens[CURRENT]);
  ParseTurp.Read := Tokens[READ];
  ParseTurp.Write := Tokens[WRITE];
  case Tokens[STEP] of
    'L': ParseTurp.Step := Left;
    'R': ParseTurp.Step := Right;
  else
    WriteLn(StdErr, Format('%s:%d: Error: "%s" is not a correct step. Expected "L" or "R"', [FilePath, Line, Tokens[STEP]]));
  end;
  ParseTurp.Next := UpCase(Tokens[NEXT]);
end;

function GetTurpsStates(Turps: array of TTurp): TStateArray;
var
  States : TStateArray = ();
  Turp : TTurp;

  function StatesContains(Needle: TState): Boolean;
  var
    State : TState;
  begin
    for State in States do
      if State = Needle then
        Exit(True);
    StatesContains := False;
  end;
begin
  for Turp in Turps do
  begin
    if not StatesContains(Turp.Current) then
      Insert(Turp.Current, States, Length(States));
    if not StatesContains(Turp.Next) then
      Insert(Turp.Next, States, Length(States));
  end;
  GetTurpsStates := States;
end;

function ReadLines(FilePath: AnsiString): TStringDynArray;
var
  FileHandle : Text;
  Result : TStringDynArray = ();
  Line : AnsiString;
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

  while not Eof(FileHandle) do
  begin
    ReadLn(FileHandle, Line);
    Insert(Line, Result, Length(Result));
  end;

  Close(FileHandle);
  ReadLines := Result;
end;

function ReadTokens(FilePath: AnsiString): TStringDynArray;
var
  Tokens : TStringDynArray = ();
  Lines : TStringDynArray;
  Line : AnsiString;
  LineTokens : TStringDynArray;
  I, J : LongWord;
begin
  Lines := ReadLines(FilePath);
  for I := 0 to High(Lines) do
  begin
    Line := Trim(Lines[I]);
    if Length(Line) > 0 then
    begin
      LineTokens := Tokenize(Line);
      for J := 0 to High(LineTokens) do
        Insert(LineTokens[J], Tokens, Length(Tokens));
    end;
  end;
  ReadTokens := Tokens;
end;

var
  I : LongWord;

  TurpFilePath : AnsiString;
  TapeFilePath : AnsiString;

  Lines : TStringDynArray;
  Line : AnsiString;

  Turps : array of TTurp = ();

  Machine : TMachine;
  State : TState;
begin
  if ParamCount > 0 then
    TurpFilePath := ParamStr(1)
  else
  begin
    WriteLn(StdErr, 'Error: Input turp file is not provided');
    WriteLn(StdErr, 'Usage: turp <input.turp> <input.tape>');
    Halt(1);
  end;

  if ParamCount > 1 then
    TapeFilePath := ParamStr(2)
  else
  begin
    WriteLn(StdErr, 'Error: Input tape file is not provided');
    WriteLn(StdErr, 'Usage: turp <input.turp> <input.tape>');
    Halt(1);
  end;

  Lines := ReadLines(TurpFilePath);
  for I := 0 to High(Lines) do
  begin
    Line := Trim(Lines[I]);
    if (Length(Line) > 0) and (Line[Low(Line)] <> '!') then
      Insert(ParseTurp(TurpFilePath, I + 1, Line), Turps, Length(Turps));
  end;

  Machine.Tape := ReadTokens(TapeFilePath);

  WriteLn('AVAILABLE STATES:');
  for State in GetTurpsStates(Turps) do
    WriteLn('  ' + State);

  Write('INITIAL STATE: ');
  ReadLn(Input, State);
  Machine.State := UpCase(State);

  repeat
    MachineDump(Machine);
    ReadLn(Input);
  until not MachineNext(Machine, Turps);
end.
