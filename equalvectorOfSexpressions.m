function [r] = equalvectorOfSexpressions(input)
%Parse a vector of s-expressions.
p = pbind(@equalopenSquare, @lambdaminus77752);
r = p(input);
function [retvalminus77753] = lambdaminus77752(ignore)
%
retvalminus77753 = pbind(equalgreaterThanzeroOrMore(@equalsexpressionplusspaces), @lambdaminus77754);
function [retvalminus77755] = lambdaminus77754(contents)
%
retvalminus77755 = pbind(@equalcloseSqare, @lambdaminus77756);
function [retvalminus77757] = lambdaminus77756(ignore)
%
retvalminus77757 = preturn([ cellArray(makeSymbol('vector')) contents ]);

end

end

end

end