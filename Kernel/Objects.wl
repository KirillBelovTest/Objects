(* ::Package:: *)

(* ::Chapter:: *)
(*Objects*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["KirillBelov`Objects`"]


(* ::Section:: *)
(*Names*)


CreateType::usage = 
"CreateType[type] create a type inhered from the Object without custom fields
CreateType[type, parent] create a type inhered from the parant without custom fields
CreateType[type, ..., {fields}] create a type with custom fields that have default Authomatic values
CreateType[type, ..., {field1 -> value1, ..}] create a type with custom fields that have default specific values"


TypeQ::usage = 
"TypeQ[expr] check that expr is mutale object"


Object::usage = 
"Object[] base mutable object"


ObjectQ::usage = 
"ObjectQ[expr] check that expr is Object or inherited from the Object"


(* ::Section:: *)
(*Private context*)


Begin["`Private`"]


(* ::Section:: *)
(*Direcotory*)


$directory = 
ParentDirectory[DirectoryName[$InputFileName]]


(* ::Section:: *)
(*Patternt test functions*)


TypeQ[___] := 
False


Object /: 
TypeQ[Object] := 
True


ObjectQ[___] := 
False


Object /: 
ObjectQ[Object[symbol_Symbol?AssociationQ]] := 
True


(* ::Section:: *)
(*Get*)


Object /: 
get[Object[symbol_Symbol?AssociationQ], fields: {(_Symbol | _String)...}] := 
With[{keys = Map[If[StringQ[#], #, SymbolName[#]]&] @ fields}, 
	Fold[Construct, symbol, keys]
]


(* ::Section:: *)
(*Set*)


Object /: 
set[Object[symbol_Symbol?AssociationQ], fields: {(_Symbol | _String)..}, value_] := 
With[{keys = Map[If[StringQ[#], #, SymbolName[#]]&] @ fields}, 
	With[{lastKey = Last[keys], firstKeys = Drop[keys, -1]}, 
		If[Length[keys] == 1, 
			symbol[lastKey] = value, 
			
			Module[{temp = get[Object[symbol], firstKeys]}, 
				Which[
					ObjectQ[temp], 
						set[temp, {lastKey}, value], 
					AssociationQ[temp], 
						temp[lastKey] = value; 
						set[Object[symbol], firstKeys, temp];  
						value, 
					True, 
						Message[Object::setraw, Row[{Object[symbol], "[", keys, "]", " = ", value}]]; 
						Null
				]
			]
		]
	]
]


(* ::Section:: *)
(*Object constructor*)


SetAttributes[Object, HoldAll]


Options[Object] = {
	"Icon" -> Import[FileNameJoin[{$directory, "Images", "ObjectIcon.png"}]], 
	"Init" -> Identity
}


Object[opts: OptionsPattern[]] := 
With[{symbol = Unique[ToString[Object] <> "`$"]}, 
	symbol = <||>; 
	Table[
		symbol[opt] = OptionValue[Object, Flatten[{opts}], opt], 
		{opt, Keys[Options[Object]]}
	]; 
	symbol["Self"] = Object[symbol]; 
	symbol["Properties"] = {}; 
	symbol["Properties"] = Keys[symbol]; 
	symbol["Init"] @ Object[symbol]; 
	Return[Object[symbol]]
]


(* ::Section:: *)
(*Object getters*)


Object[symbol_Symbol?AssociationQ][fields___] := 
get[Object[symbol], {fields}]


Object /: 
Dot[Object[symbol_Symbol?AssociationQ], fields__] := 
get[Object[symbol], {fields}]


(* ::Section:: *)
(*Object setters*)


Object /: 
Set[name_Symbol, Object[symbol_Symbol?AssociationQ]] := 
Module[{nameString}, 
	ClearAll[name]; 
	nameString = SymbolName[name]; 
	Block[{Object}, SetAttributes[Object, HoldAll]; name = Object[symbol]]; 
	name /: Set[name[fields__], value_] := (
		Object[symbol]["Properties"] = DeleteDuplicates[Append[Object[symbol]["Properties"], ToString[fields]]]; 
		ResourceFunction["AddCodeCompletion"][nameString][Object[symbol]["Properties"]];
		set[Object[symbol], {fields}, value]
	); 
	ResourceFunction["AddCodeCompletion"][nameString][Object[symbol]["Properties"]];
	name
]


Object /: 
Set[Object[symbol_Symbol?AssociationQ][fields__], value_] := 
set[Object[symbol], {fields}, value]


(* ::Section:: *)
(*Override Set*)


Unprotect[Set]


Set[Dot[object_?ObjectQ, fields__], value_] := 
set[object, {fields}, value]


Protect[Set]


(* ::Section:: *)
(*Summary box*)


SetAttributes[objectFieldsBox, HoldAll]


objectFieldsBox[assoc_Symbol?AssociationQ] := 
Function[fields, Sequence@@{First[#], Partition[Flatten[Rest[#]], UpTo[2]]}& @ 
Append[Partition[fields, UpTo[2]], {}]] @ 
Join[
	{
		{BoxForm`SummaryItem[{"Symbol: ", Defer[assoc]}], SpanFromLeft}, 
		{BoxForm`SummaryItem[{"Properties: ", assoc["Properties"]}], SpanFromLeft}
	}, 
	KeyValueMap[{BoxForm`SummaryItem[{#1 <> ": ", #2}], SpanFromLeft}&] @ KeyDrop[assoc, {"Self", "Icon", "Properties"}]
]


Object /: 
MakeBoxes[Object[symbol_Symbol?AssociationQ], form: StandardForm | TraditionalForm] := 
BoxForm`ArrangeSummaryBox[
	Object, Object[symbol], Object[symbol]["Icon"], 
	objectFieldsBox[symbol], 
	form, "Interpretable" -> Automatic
]


(* ::Section:: *)
(*Create type*)


fieldsPattern[] := {(_String | _Symbol | Rule[_String | _Symbol, _])...}


initPattern[] := Except[_fieldsPattern | _Association | _?TypeQ]


toRule[key_String -> value_] := key -> value


toRule[key_Symbol -> value_] := ToString[key] -> value


toRule[key_String] := key -> Automatic


toRule[key_Symbol] := ToString[key] -> Automatic


toRule[fields: fieldsPattern[]] := <|Map[toRule] @ fields|>


CreateType[type_Symbol, parent_Symbol?TypeQ, init: initPattern[], fields: <|Rule[_String, _]...|>] := (
	ClearAll[type]; 
	Language`ExtendedFullDefinition[type] = Language`ExtendedFullDefinition[parent] /. parent -> type; 
	Options[type] = Normal[<|Join[Options[type], Normal[fields], {If[init === Automatic, Nothing, "Init" -> init]}]|>]; 
	type
)


CreateType[type_Symbol, parent: _Symbol?TypeQ: Object, init: initPattern[]: Automatic, fields: fieldsPattern[]: {}] := 
CreateType[type, parent, init, toRule[fields]]


CreateType[type_Symbol, init: initPattern[]: Automatic, fields: fieldsPattern[]: {}] := 
CreateType[type, Object, init, fields]


CreateType[type_Symbol, parent: _Symbol?TypeQ: Object, fields: fieldsPattern[]: {}] := 
CreateType[type, parent, Automatic, fields]


CreateType[type_Symbol, fields: fieldsPattern[]: {}] := 
CreateType[type, Object, Automatic, fields]


(* ::Section:: *)
(*End private context*)


End[] (*`Private`*)


(* ::Section:: *)
(*Ena package*)


EndPackage[] (*KirillBelov`Objects`*)
