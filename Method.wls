#!/usr/bin/env wolframscript
(* ::Package:: *)

(obj[ref_])[key_String] := ref[key]


obj[ref_][(method_Symbol)[args___]] := method[obj[ref], args]


o1 = obj[o1ref]


o1ref = <|"x" -> 1|>


o1["x"]


o1@f[1]


Hold[]
