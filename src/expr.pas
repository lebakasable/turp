unit Expr;

interface

uses
  Lexer;

type
  PExprArray = ^TExprArray;
  PExpr = ^TExpr;

  TExprAtom = record
    Name : TSymbol;
  end;

  TExprTuple = record
    OpenParen : TSymbol;
    Elements : PExprArray;
  end;

  TExprKind = (ExprAtom, ExprTuple);

  TExpr = record
    case Kind : TExprKind of
      ExprAtom: (Atom: ^TExprAtom);
      ExprTuple: (Tuple: ^TExprTuple);
  end;
  TExprArray = Array of TExpr;

function ExprToStr(const Expr: TExpr): AnsiString;
function ExprLoc(const Expr: TExpr): TLoc;
function ExprAtomName(const Expr: TExpr): PSymbol;
function ExprMatches(const Expr, Other: TExpr): Boolean;
function ExprSubstitute(const Expr: TExpr; const Symbol, By: TSymbol): TExpr;

function ParseExpr(var Lexer: TLexer): TExpr;

implementation

function ExprToStr(const Expr: TExpr): AnsiString;
var
  I : LongWord;
begin
  case Expr.Kind of
    ExprAtom: ExprToStr := Expr.Atom^.Name.Name;
    ExprTuple:
    with Expr.Tuple^ do
    begin
      ExprToStr := '(';
      for I := 0 to High(Elements^) do
      begin
        if I > 0 then
          ExprToStr := ExprToStr + ' ';
        ExprToStr := ExprToStr + ExprToStr(Elements^[I]);
      end;
      ExprToStr := ExprToStr + ')';
    end;
  end;
end;

function ExprLoc(const Expr: TExpr): TLoc;
begin
  case Expr.Kind of
    ExprAtom: ExprLoc := Expr.Atom^.Name.Loc;
    ExprTuple: ExprLoc := Expr.Tuple^.OpenParen.Loc;
  end;
end;

function ExprAtomName(const Expr: TExpr): PSymbol;
begin
  ExprAtomName := Nil;
  if Expr.Kind = ExprAtom then
  begin
    New(ExprAtomName);
    ExprAtomName^ := Expr.Atom^.Name;
  end;
end;

function ExprMatches(const Expr, Other: TExpr): Boolean;
var
  I : LongWord;
begin
  if (Expr.Kind = ExprAtom) and (Other.Kind = ExprAtom) then
    ExprMatches := Expr.Atom^.Name.Name = Other.Atom^.Name.Name
  else if (Expr.Kind = ExprTuple) and (Other.Kind = ExprTuple) then
  begin
    if Length(Expr.Tuple^.Elements^) <> Length(Other.Tuple^.Elements^) then
      Exit(False);
    for I := 0 to High(Expr.Tuple^.Elements^) do
      if not ExprMatches(Expr.Tuple^.Elements^[I], Other.Tuple^.Elements^[I]) then
        Exit(False);
    ExprMatches := True;
  end
  else
    ExprMatches := False;
end;

function ExprSubstitute(const Expr: TExpr; const Symbol, By: TSymbol): TExpr;
var
  Element : TExpr;
begin
  case Expr.Kind of
    ExprAtom:
      if Expr.Atom^.Name.Name = Symbol.Name then
      begin
        New(ExprSubstitute.Atom);
        ExprSubstitute.Atom^.Name := By;
      end
      else
        ExprSubstitute := Expr;
    ExprTuple:
    with Expr.Tuple^ do
    begin
      New(ExprSubstitute.Tuple);
      ExprSubstitute.Tuple^.OpenParen := OpenParen;
      New(ExprSubstitute.Tuple^.Elements);
      for Element in Elements^ do
        Insert(ExprSubstitute(Element, Symbol, By), ExprSubstitute.Tuple^.Elements^, Length(ExprSubstitute.Tuple^.Elements^));
    end;
  end;
end;

function ParseExpr(var Lexer: TLexer): TExpr;
var
  Symbol1 : TSymbol;
  Symbol2 : PSymbol;
  Elements : Array of TExpr = ();
begin
  Symbol1 := LexerParseSymbol(Lexer);
  if Symbol1.Name = '(' then
  begin
    Symbol2 := LexerPeekSymbol(Lexer);
    while Symbol2 <> Nil do
    begin
      if Symbol2^.Name = ')' then Break;
      Insert(ParseExpr(Lexer), Elements, Length(Elements));
      Symbol2 := LexerPeekSymbol(Lexer);
    end;
    LexerExpectSymbols(Lexer, [')']);
    ParseExpr.Kind := ExprTuple;
    New(ParseExpr.Tuple);
    ParseExpr.Tuple^.OpenParen := Symbol1;
    New(ParseExpr.Tuple^.Elements);
    ParseExpr.Tuple^.Elements^ := Elements;
  end
  else
  begin
    ParseExpr.Kind := ExprAtom;
    New(ParseExpr.Atom);
    ParseExpr.Atom^.Name := Symbol1;
  end;
end;

end.
