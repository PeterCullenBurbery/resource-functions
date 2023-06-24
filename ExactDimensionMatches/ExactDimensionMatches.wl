ClearAll[ExactDimensionMatches]
ExactDimensionMatches::usage="ExactDimensionMatches[{{dimensionname1,dimensionpower1},{dimensionname2,dimensionpower2},\[Ellipsis]}] gives a list of all physical quantity dimension entities that match the dimension specifications in list exactly by having the exact dimensions dimensionname1^dimensionpower1*dimensionname2^dimensionpower^2*\[Ellipsis] and so on. ExactDimensionMatches[<|dimensionname1->dimensionpower1,dimensionname2->dimensionpower2,\[Ellipsis]|>] does the same thing but for an association. ExactDimensionMatches[quantity] computes the dimensions for quantity and gives that as input to ExactDimensionMatches. ExactDimensionMatches[\!\(\*StyleBox[\"dimensionspec\", \"TI\"]\),\!\(\*StyleBox[\"property\", \"TI\"]\)] computes \!\(\*StyleBox[\"property\", \"TI\"]\) for physical dimension entities that match \!\(\*StyleBox[\"dimensionspec\", \"TI\"]\).";

ExactDimensionMatches[dimensions_?(Function[{l},MatchQ[l,{{_?StringQ,_?RealValuedNumericQ}..}],{}]),
Optional[property_,"Entities"],Optional[form_,"List"]]:=
EntityValue[FilteredEntityClass["PhysicalQuantity",
EntityFunction[physicalQuantityDimension,ContainsExactly[MapAt[Rationalize,Cases[dimensions,{_,Except[0]}],
{All,2}]][physicalQuantityDimension["Dimensions"]]]],Sequence@@{property,Which[form==="List",Nothing,form=!="List",form]}]

ExactDimensionMatches[dimensions_?(Function[{l},AssociationQ[l],{}]),Optional[property_,"Entities"],Optional[form_,"List"]]:=
ExactDimensionMatches[Transpose[{Keys[#],Values[#]}&[dimensions]],property,form]

ExactDimensionMatches[unit_?(Function[{l},QuantityQ[l],{}]),Optional[property_,"Entities"],Optional[form_,"List"]]:=
ExactDimensionMatches[UnitDimensions[unit],property,form]
ExactDimensionMatches[args___]:=Null/;CheckArguments[ExactDimensionMatches[args],{1,3}]