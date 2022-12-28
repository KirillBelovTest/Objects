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


Object /: 
TypeQ[Object] := 
True


Object /: 
ObjectQ[Object[symbol_Symbol?AssociationQ]] := 
True


(* ::Section:: *)
(*Object*)


$objectIcon = 
Import[FileNameJoin[{$directory, "Images", "ObjectIcon.png"}]]


SetAttributes[Object, HoldAll]


Options[Object] = {
	"Icon" -> $objectIcon
}


Object[opts: OptionsPattern[]] := 
With[{symbol = Unique[ToString[Object] <> "`$"]}, 
	symbol = <||>; 
	Table[
		symbol[opt] = OptionValue[Object, Flatten[{opts}], opt], 
		{opt, Keys[Options[Object]]}
	]; 
	symbol["Self"] = Object[symbol]
]


Object /: 
Set[name_Symbol, Object[symbol_Symbol?AssociationQ]] := 
Block[{Object}, 
	ClearAll[name]; 
	SetAttributes[Object, HoldAll]; 
	name = Object[symbol]; 
	name /: Set[name[field_], value_] := Set[Object[symbol][field], value]; 
	name
]


Object /: 
Set[Object[symbol_Symbol?AssociationQ][field_String], value_] := 
symbol[field] = value


Object /: 
Set[object: Object[symbol_Symbol?AssociationQ][field_Symbol], value_] := 
With[{fieldStr = SymbolName[field]}, symbol[fieldStr] = value]


Object[symbol_Symbol?AssociationQ][field_String] := 
symbol[field]


Object[symbol_Symbol?AssociationQ][field_Symbol] := 
symbol[ToString[field]]


Object[symbol_Symbol?AssociationQ][fields: (_Symbol | _String)..] := 
Fold[Construct, Object[symbol], {fields}]


Object /: 
Dot[Object[symbol_Symbol?AssociationQ], keys__] := 
Fold[Construct, Object[symbol], {keys}]


Object[symbol_Symbol?AssociationQ][] := 
symbol


(* ::Section:: *)
(*Set*)


Unprotect[Set]


Set[Dot[object_?ObjectQ, keys__], value_] := 
Module[{temp, firstKeys, lastKey = Last[{keys}]}, 
	If[Length[{keys}] == 1, 
		With[{key = lastKey}, object[key] = value], 
		firstKeys = Sequence @@ {keys}[[ ;; -2]];
		temp = object[firstKeys]; 
		Which[
			ObjectQ[temp], 
				With[{key = lastKey}, temp[key] = value], 
			AssociationQ[temp], 
				With[{key = lastKey}, temp[key] = value]; 
				With[{expr = Hold @@ {object, firstKeys}}, 
					expr /. Hold[o_, args__] :> Set[o[args], temp]], 
			True, 
				Message[Object::setraw, Row[{object, "[", keys, "]", " = ", value}]]; 
				Null
		]
	]; 
	value
]


Protect[Set]


(* ::Section:: *)
(*Summary box*)


SetAttributes[objectFieldsBox, HoldAll]


objectFieldsBox[assoc_Symbol?AssociationQ] := 
Function[fields, Sequence@@{First[#], Partition[Flatten[Rest[#]], UpTo[2]]}& @ 
Append[Partition[fields, UpTo[2]], {}]] @ 
Join[
	{{BoxForm`SummaryItem[{"Symbol: ", Defer[assoc]}], SpanFromLeft}}, 
	KeyValueMap[{BoxForm`SummaryItem[{#1 <> ": ", #2}], SpanFromLeft}&] @ KeyDrop[assoc, {"Self", "Icon"}]
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


toRule[key_String -> value_] := key -> value


toRule[key_Symbol -> value_] := ToString[key] -> value


toRule[key_String] := key -> Automatic


toRule[key_Symbol] := ToString[key] -> Automatic


toRule[fields: fieldsPattern[]] := <|Map[toRule] @ fields|>


CreateType[type_Symbol, parent_Symbol?TypeQ, fields: <|Rule[_String, _]...|>] := (
	ClearAll[type]; 
	Language`ExtendedFullDefinition[type] = Language`ExtendedFullDefinition[parent] /. parent -> type; 
	Options[type] = Normal[<|Join[Options[type], Normal[fields]]|>]; 
	type
)


CreateType[type_Symbol, parent_Symbol?TypeQ, fields: fieldsPattern[]: {}] := 
CreateType[type, parent, toRule[fields]]


CreateType[type_Symbol, fields: fieldsPattern[]: {}] := 
CreateType[type, Object, fields]


(* ::Section:: *)
(*End private context*)


End[] (*`Private`*)


(* ::Section:: *)
(*Ena package*)


EndPackage[] (*KirillBelov`Objects`*)
