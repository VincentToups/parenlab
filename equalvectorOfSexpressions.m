function [r] = equalvectorOfSexpressions(input)
%Parse a vector of s-expressions.
p = pbind(@equalopenSquare, @lambdaminus82891);
r = p(input);
function [retvalminus82892] = lambdaminus82891(ignore)
%
retvalminus82892 = pbind(equalgreaterThanzeroOrMore(@equalsexpressionplusspaces), @lambdaminus82893);
function [retvalminus82894] = lambdaminus82893(contents)
%
retvalminus82894 = pbind(@equalcloseSqare, @lambdaminus82895);
function [retvalminus82896] = lambdaminus82895(ignore)
%
retvalminus82896 = preturn([ cellArray(makeSymbol('vector')) contents ]);

end

end

end

end