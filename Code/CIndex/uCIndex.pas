unit uCIndex;

interface

uses
  Classes, System.Generics.Collections, Contnrs;

type
  TCIndex = class(TObject)
  public type
    TIdent = class(TComponent)
    private
      FFollows: TComponentList;

    public
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;
      property Follows: TComponentList read FFollows;
    end;

    TWord = class(TIdent)
    private
      FW: WideChar;
      procedure SetW(const Value: WideChar);
    published
      property W: WideChar read FW write SetW;
    end;
  end;

implementation

{ TCIndex.TIdent }

procedure TCIndex.TIdent.AfterConstruction;
begin
  inherited;
  FFollows := TComponentList.Create;
end;

procedure TCIndex.TIdent.BeforeDestruction;
begin
  inherited;
  FFollows.Free;
end;

{ TCIndex.TWord }

procedure TCIndex.TWord.SetW(const Value: WideChar);
begin
  FW := Value;
end;

end.
